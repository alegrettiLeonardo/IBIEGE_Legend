! F06: tabular presence is metadata, never inferred from positivity.
module rd_bearing_contract
  use rd_kinds, only: wp
  use, intrinsic :: ieee_arithmetic, only: ieee_is_finite
  implicit none
  private
  integer, parameter :: max_tables=9
  integer, save :: row_width(max_tables)=0,rows_read(max_tables)=0
  real(wp), save :: previous_speed(max_tables)=0._wp
  public :: bearing_table_reset, bearing_read_row, bearing_has_tilt
  public :: bearing_validate_table, bearing_speed_in_range
contains
  subroutine bearing_table_reset()
    row_width=0; rows_read=0; previous_speed=0._wp
  end subroutine

  logical function bearing_has_tilt(table)
    integer, intent(in) :: table
    bearing_has_tilt=.false.
    if(table>=1.and.table<=max_tables) bearing_has_tilt=row_width(table)==11
  end function

  subroutine bearing_read_row(text,table,row,values,errmsg,ok)
    character(len=*), intent(in) :: text
    integer, intent(in) :: table,row
    real(wp), intent(out) :: values(11)
    character(len=*), intent(out) :: errmsg
    integer, intent(out) :: ok
    integer :: i,j,n,count,ios
    logical :: after_comma,seen_value
    character(len=1) :: ch
    ok=-1; errmsg=''; values=0._wp; count=0
    if(table<1.or.table>max_tables.or.row<1) then
      errmsg='bearing table: invalid table/row identifier'; return
    end if
    n=len_trim(text)
    do i=1,n
      if(text(i:i)=='!'.or.text(i:i)=='#') then
        n=i-1; exit
      end if
    end do
    i=1; after_comma=.false.; seen_value=.false.
    do while(i<=n)
      ch=text(i:i)
      if(ch==' '.or.iachar(ch)==9) then
        i=i+1; cycle
      end if
      if(ch==',') then
        if(after_comma.or..not.seen_value) then
          errmsg='bearing table: empty comma-separated field'; return
        end if
        after_comma=.true.; i=i+1; cycle
      end if
      j=i
      do while(j<=n)
        ch=text(j:j)
        if(ch==' '.or.iachar(ch)==9.or.ch==',') exit
        if(index('0123456789+-.eEdD',ch)==0) then
          errmsg='bearing table: nonnumeric, repeat, or slash field is not allowed'; return
        end if
        j=j+1
      end do
      count=count+1
      if(count>11) then
        errmsg='bearing table: expected 9 or 11 numeric fields'; return
      end if
      read(text(i:j-1),*,iostat=ios) values(count)
      if(ios/=0) then
        write(errmsg,'(a,i0,a,i0)') 'bearing table: invalid value, table ',table,' row ',row
        return
      end if
      if(.not.ieee_is_finite(values(count))) then
        errmsg='bearing table: nonfinite coefficient'; return
      end if
      seen_value=.true.; after_comma=.false.; i=j
    end do
    if(after_comma.or.(count/=9.and.count/=11)) then
      write(errmsg,'(a,i0,a,i0)') 'bearing table: expected 9 or 11 fields, got ',count,' row ',row
      return
    end if
    if(row==1) then
      row_width(table)=count; rows_read(table)=0
    end if
    if(row_width(table)/=count) then
      errmsg='bearing table: optional tilt columns must be present in every row'; return
    end if
    if(values(1)<0._wp) then
      errmsg='bearing table: speed must be nonnegative rpm'; return
    end if
    if(row/=rows_read(table)+1) then
      errmsg='bearing table: rows must be read consecutively'; return
    end if
    if(row>1) then
      if(values(1)<=previous_speed(table)) then
        errmsg='bearing table: speeds must be strictly increasing; duplicate/out-of-order row'; return
      end if
    end if
    previous_speed(table)=values(1); rows_read(table)=row
    ok=0
  end subroutine

  subroutine bearing_validate_table(table,speed,k,c,tilt,scale,errmsg,ok)
    integer, intent(in) :: table
    real(wp), intent(in) :: speed(:),k(:,:),c(:,:),tilt(:,:),scale
    character(len=*), intent(out) :: errmsg
    integer, intent(out) :: ok
    integer :: i,n
    ok=-1; errmsg=''; n=size(speed)
    if(table<1.or.table>max_tables.or.n<2.or.n>99) then
      errmsg='bearing table: expected 2..99 speed rows'; return
    end if
    if(size(k,1)/=n.or.size(c,1)/=n.or.size(k,2)/=4.or.size(c,2)/=4.or. &
       size(tilt,1)/=n.or.size(tilt,2)/=2) then
      errmsg='bearing table: coefficient dimensions do not match speeds'; return
    end if
    if(.not.ieee_is_finite(scale).or.scale<=0._wp) then
      errmsg='bearing table: coefficient scale must be positive and finite'; return
    end if
    if(.not.all(ieee_is_finite(speed)).or..not.all(ieee_is_finite(k)).or. &
       .not.all(ieee_is_finite(c)).or..not.all(ieee_is_finite(tilt))) then
      errmsg='bearing table: speed or coefficient is nonfinite'; return
    end if
    if(any(speed<0._wp)) then
      errmsg='bearing table: negative rpm is outside the declared model'; return
    end if
    do i=2,n
      if(speed(i)<=speed(i-1)) then
        write(errmsg,'(a,i0,a,i0)') 'bearing table: speeds must increase, table ',table,' row ',i
        return
      end if
    end do
    ! Signed cross-coupled coefficients are intentionally not symmetrized.
    ! Tilt zeros are valid. Signed direct terms require the declared external
    ! linear model; they are not silently clamped to a passive-bearing value.
    if(row_width(table)/=9.and.row_width(table)/=11) then
      errmsg='bearing table: column presence unavailable; read the original TABLE input'; return
    end if
    ok=0
  end subroutine

  subroutine bearing_speed_in_range(rpm,minimum,maximum,factors,errmsg,ok)
    real(wp), intent(in) :: rpm,minimum,maximum,factors(2)
    character(len=*), intent(out) :: errmsg
    integer, intent(out) :: ok
    real(wp) :: low,high
    ok=-1; errmsg=''
    if(.not.ieee_is_finite(rpm).or..not.ieee_is_finite(minimum).or. &
       .not.ieee_is_finite(maximum).or..not.all(ieee_is_finite(factors))) then
      errmsg='bearing table: nonfinite speed range'; return
    end if
    if(rpm<0._wp.or.minimum<0._wp.or.maximum<=minimum.or. &
       factors(1)<0._wp.or.factors(1)>1._wp.or.factors(2)<1._wp) then
      errmsg='bearing table: invalid rpm or extrapolation factors'; return
    end if
    if(maximum>huge(1._wp)/factors(2)) then
      errmsg='bearing table: extrapolation limit overflows'; return
    end if
    low=minimum*factors(1); high=maximum*factors(2)
    ! No finite zero-speed bearing law is fabricated by extrapolation.
    if(rpm==0._wp.and.minimum>0._wp) then
      errmsg='bearing table: zero rpm requires an explicit finite row at zero'; return
    end if
    if(rpm<low.or.rpm>high) then
      write(errmsg,'(a,es12.4,a,es12.4,a,es12.4)') &
        'bearing rpm=',rpm,' outside [',low,',',high
      return
    end if
    ok=0
  end subroutine
end module rd_bearing_contract
