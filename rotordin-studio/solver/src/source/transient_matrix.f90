!
!     Physical matrix update for the true transient solver.
!
subroutine transient_check_bearing_range(rpm,errmsg,ok)
  use com_knd, only: cknd, supr
  use com_nbc, only: cp, scl, bc
  use com_pmk, only: nt, kmc, cmc, rmc
  use rd_kinds, only: lrk, wp
  implicit none
  integer :: mpm, mxm
  parameter(mxm=9,mpm=99)
  real(wp) :: rpm
  character(len=*) :: errmsg
  integer :: ok, i, j
  ok=-1
  if(cknd.ne.'2') then
    ok=0
    return
  endif
  do i=1,cp
    if(nt(i).lt.2) then
      errmsg='transient: bearing table needs at least two points'
      return
    endif
    do j=2,nt(i)
      if(rmc(i,j).le.rmc(i,j-1)) then
        errmsg='transient: bearing-table RPM must be increasing'
        return
      endif
    enddo
    if(rpm.lt.real(rmc(i,1), wp)-1e-7_wp .or.rpm.gt.real(rmc(i,nt(i)), wp)+1e-7_wp) then
      write(errmsg,'(A,F14.4,A,I3,A,F14.4,A,F14.4,A)')&
      &'transient: RPM ',rpm,' outside bearing table ',i,&
      &' [',rmc(i,1),',',rmc(i,nt(i)),']'
      return
    endif
  enddo
  ok=0
  return
end subroutine transient_check_bearing_range
!
subroutine transient_matrix_update(dm,pp,rpm,omega,alpha,&
&mass,gyro,kbase,cbase,kphys,cphys,errmsg,ok)
  use com_mat, only: mm, mg, gdm => dm, smn
  use com_mtk, only: mk1
  use rd_kinds, only: lrk, wp
  implicit none
  integer :: mtg
  parameter(mtg=500)
  integer :: dm, pp, ok, oki, i, j
  real(wp) :: rpm, omega, alpha
  real(wp) :: mass(mtg,mtg), gyro(mtg,mtg)
  real(wp) :: kbase(mtg,mtg), cbase(mtg,mtg)
  real(wp) :: kphys(mtg,mtg), cphys(mtg,mtg)
  complex(wp) :: wk(mtg,mtg), wc(mtg,mtg)
  character(len=*) :: errmsg
  intrinsic :: real
  ok=-1
  if(dm.ne.gdm .or. dm.gt.mtg) then
    errmsg='transient: inconsistent global matrix dimension'
    return
  endif
  call transient_check_bearing_range(rpm,errmsg,oki)
  if(oki.ne.0) return
  call prpkc(real(omega, lrk),dm,pp,wk,wc,0,errmsg,oki)
  if(oki.ne.0) return
  do j=1,dm
    do i=1,dm
      mass(i,j)=real(mm(i,j), wp)
      gyro(i,j)=real(mg(i,j), wp)
      kbase(i,j)=real(mk1(i,j), wp)+real(real(wk(i,j)), wp)
      cbase(i,j)=real(real(wc(i,j)), wp)
      kphys(i,j)=kbase(i,j)+alpha*gyro(i,j)
      cphys(i,j)=cbase(i,j)+omega*gyro(i,j)
    enddo
  enddo
  ok=0
  return
end subroutine transient_matrix_update
