!     $Id$
!     ==================================================================
!
!>    @file matfun.f
!>    @brief matrix and math functions, last changes:<br>
!>    removed unused subs - francisco - 03/12/2008<br>
!>    added copmat1_r - francisco - 10/03/2009<br>
!>    added copmat2_rc and sommat_rdc - francisco - 10/03/2009<br>
!>    added cplxmat_d - francisco - 18/11/2014<br>
!>    moved xtime to saidas.f - francisco jul-15<br>
!>    added vetorial transformation vrotate moved from
!>    resp_fv.f and resp_f.f - francisco - jul-15<br>
!>    moved intlag and f1camp from campbl.f - francisco - oct-15<br>
!>    added toradf, todegf and iiff,riff,ciff functions - francisco - ma
!>    added pi functions, central messages, conversion - francisco - apr
!>    added dslu and dslu2, Cholesky solution symmetric matix - francisc
!>    added dtprc double precision matrix transposition - francisco - de
!>    added rpeqf check equals with precision - francisco - jan-20<br>
!>    added matrix copy and diagonal invert with offset - francisco - ja
!>    added function to convert rpm to Hz - francisco - jan-20<br>
!>    added zervec copmat_c routines - francisco - may-20<br>
!>    added cadjf function - francisco - sep-20<br>
!>    added ifidxof fourth index on four basis indexing - francisco - oc
!>    added medfilt1w moveable median filter, used on bending stress - f
!>    added adjangf function adjust angles 0-360 - francisco dec-20<br>
!>    added degseqf function to adjust angle sequence - francisco jan-21
!>    updated order functions code cleanup  - francisco - jan-21<br>
!>    added complex argument functions dargf and rargf - francisco - jan
!>    added function to signal of given input number dsgnf - francisco m
!>    added three point quadratic functions qcf_ and mxq_ real and doubl
!>    moved from rdfra and linhael - francisco - mar-21<br>
!>    added function ivcoef, inverse coefficient - francisco - jun-21<br
!>    added zero division check on mxqd and qcfd - francisco - sep - 21<
!
!     ==================================================================
!>    @brief pi value as double.
!
!>    @return pi value as double.
!
real(wp) function dpif()
  use rd_kinds, only: wp
  implicit none
!
  real(wp) :: dpi
  parameter (dpi =3.14159265358979323846264338327950288419716939937510582097494459_wp)
!
  dpif = dpi
!
  return
!
end function dpif
!
!     ==================================================================
!>    @brief pi value as real.
!
!>    @return pi value as real.
!
real(lrk) function rpif()
  use rd_kinds, only: lrk, wp
  implicit none
!
  real(wp) :: dpi, dpif
!
  intrinsic :: real
!
  dpi = dpif()
  rpif = real(dpi, lrk)
!
  return
!
end function rpif
!
!     ==================================================================
!>    @brief convert angle from degree to radian.
!
!>    @param[in] deg angle given in degree
!>    @return given input angle in degree converted to radian
!
real(wp) function toraddf(deg)
  use rd_kinds, only: wp
  implicit none
!
  real(wp) :: deg, dpi, hdg
  parameter (dpi =3.14159265358979323846264338327950288419716939937510582097494459_wp,hdg = 180._wp)

!
  toraddf = deg*dpi/hdg
!
  return
!
end function toraddf
!
!     ==================================================================
!>    @brief convert angle from degree to radian up to 2*pi.
!>    if set ipos, angle will be always positive, 0-2*pi range.
!
!>    @param[in] deg angle given in degree
!>    @param[in] ipos if set angle will be always positive
!>    @return input angle in degree converted to radian, 0-2*pi range.
!
real(lrk) function toradpf(deg,ipos)
  use rd_kinds, only: lrk, wp
  implicit none
!
  integer :: ipos
  real(lrk) :: deg
!
  real(wp) :: toraddf
  real(lrk) :: tpi, rpif, hdeg, toradp
  parameter (hdeg = 180.0_lrk)
!
  intrinsic :: abs, real
!
  tpi = 2.0_lrk*rpif()
  toradp = real(toraddf(real(deg, wp)), lrk)
  do while (abs(toradp) .gt. tpi)
    if (toradp .gt. 0) then
      toradp = toradp-tpi
    else
      toradp = toradp+tpi
    end if
  end do
!     only psositive
  if (ipos .ne. 0) then
    if (toradp .lt. 0) toradp = toradp+tpi
  end if
!
  toradpf = toradp
!
  return
!
end function toradpf
!
!     ==================================================================
!>    @brief convert angle from degree to radian up to 2*pi.
!>    Convenience function. Only convert to radian.
!
!>    @param[in] deg angle given in degree
!>    @return given input angle in degree converted to radian
!
real(lrk) function toradf(deg)
  use rd_kinds, only: lrk
  implicit none
!
  real(lrk) :: deg
  real(lrk) :: torad, toradpf
!
  torad = toradpf(deg,0)
  toradf = torad
!
  return
!
end function toradf
!
!     ==================================================================
!>    @brief just convert angle from radians to degree
!
!>    @param[in] rad angle given in radian
!>    @return given input angle in radian converted to degree
!
real(lrk) function todegjf(rad)
  use rd_kinds, only: lrk
  implicit none
!
  real(lrk) :: rad
!
  real(lrk) :: rpif, hdeg, todegj
  parameter (hdeg = 180.0_lrk)
!
  todegj = rad*hdeg/rpif()
  todegjf = todegj
!
  return
!
end function todegjf
!
!     ==================================================================
!>    @brief adjust input angle degree to continue a sequence.
!
!>    @param[in] am current anlge (degree)
!>    @param[in] dl last angle (degree)
!>    @return sequence angle in degree
!
real(lrk) function degseqf(am,dl)
  use rd_kinds, only: lrk
  implicit none
!
  real(lrk) :: am, dl
!
  real(lrk) :: degseq, ts, oe
  parameter (ts = 360.0_lrk,oe = ts/2.0_lrk)
!
  degseq = am
  do while (degseq-dl .gt. oe)
    degseq = degseq-ts
  end do
  do while (degseq-dl .lt. -oe)
    degseq = ts+degseq
  end do
!
  degseqf = degseq
!
  return
!
end function degseqf
!
!     ==================================================================
!>    @brief adjust input angle degree from 0 up to 360.
!
!>    @param[in] adeg input angle given in degree
!>    @return given input angle in degree adusted from 0 up to 360 degre
!
real(lrk) function adjangf(adeg)
  use rd_kinds, only: lrk
  implicit none
!
  real(lrk) :: adeg
  real(lrk) :: deg, dhdeg
  parameter (dhdeg = 360.0_lrk)
  intrinsic :: abs
!
  deg = adeg
!     adjust
  do while (abs(deg) .gt. dhdeg .or. deg .lt. 0)
    if (deg .gt. 0) then
      deg = deg-dhdeg
    else
      deg = deg+dhdeg
    end if
  end do
!
  adjangf = deg
!
  return
!
end function adjangf
!
!     ==================================================================
!>    @brief convert angle from radian to degree up to 360.
!
!>    @param[in] rad angle given in radian
!>    @param[in] ipos if set angle will be always positive
!>    @return given input angle in radian converted to degree
!
real(lrk) function todegpf(rad,ipos)
  use rd_kinds, only: lrk
  implicit none
!
  integer :: ipos
  real(lrk) :: rad
!
  real(lrk) :: adjangf, todegjf, todegp
!
!     convert rad->deg
  todegp = todegjf(rad)
!
  if (ipos .ne. 0) then
!      only positive 0-360
    todegp = adjangf(todegp)
  end if
!
  todegpf = todegp
!
  return
!
end function todegpf
!
!     ==================================================================
!>    @brief convert angle from radian to degree.
!>    Convenience function. Only convert to degree.
!
!>    @param[in] rad angle given in radian
!>    @return given input angle in radian converted to degree
!
real(lrk) function todegf(rad)
  use rd_kinds, only: lrk
  implicit none
!
  real(lrk) :: rad
  real(lrk) :: todeg, todegpf
!
  todeg = todegpf(rad,0)
  todegf = todeg
!
  return
!
end function todegf
!
!     ==================================================================
!>    @brief convert angular speed from rad/s to rpm or 1/min
!
!>    @param[in] rad angular speed in rad/s
!>    @return given angular speed in rad/s converted to rpm or 1/min
!
real(lrk) function rad2rpmf(rad)
  use rd_kinds, only: lrk
  implicit none
!
  real(lrk) :: rad
  real(lrk) :: rpif, rad2rpm
!
  rad2rpm = rad*30.0_lrk/rpif()
  rad2rpmf = rad2rpm
!
  return
!
end function rad2rpmf
!
!     ==================================================================
!>    @brief convert angular speed from rad/s to Hz or 1/s
!
!>    @param[in] rad angular speed in rad/s
!>    @return given angular speed in rad/s converted to Hz or 1/s
!
real(lrk) function rad2hzf(rad)
  use rd_kinds, only: lrk
  implicit none
!
  real(lrk) :: rad
  real(lrk) :: rpif, rad2hz
!
  rad2hz = rad/2.0_lrk/rpif()
  rad2hzf = rad2hz
!
  return
!
end function rad2hzf
!
!     ==================================================================
!>    @brief convert angular speed from rpm or 1/min to rad/s
!
!>    @param[in] rpm angular speed in rpm or 1/min
!>    @return given angular speed in rpm or 1/min converted to rad/s
!
real(lrk) function rpm2radf(rpm)
  use rd_kinds, only: lrk
  implicit none
!
  real(lrk) :: rpm
  real(lrk) :: rpif, rpm2rad
!
  rpm2rad = rpm/30.0_lrk*rpif()
  rpm2radf = rpm2rad
!
  return
!
end function rpm2radf
!
!     ==================================================================
!>    @brief convert angular speed from rpm or 1/min to Hz
!
!>    @param[in] rpm angular speed in rpm or 1/min
!>    @return given angular speed in rpm or 1/min converted to Hz
!
real(lrk) function rpm2hzf(rpm)
  use rd_kinds, only: lrk
  implicit none
!
  real(lrk) :: rpm
  real(lrk) :: rpm2hz, rpm2radf, rad2hzf
!
  rpm2hz = rad2hzf(rpm2radf(rpm))
  rpm2hzf = rpm2hz
!
  return
!
end function rpm2hzf
!
!     ==================================================================
!>    @brief integer if function.
!
!>    @param[in] lcn logical clause
!>    @param[in] itr true value
!>    @param[in] ifl false value
!>    @return true value if clause if true false value otherwise
!
integer function iiff(lcn,itr,ifl)
  implicit none
!
  logical :: lcn
  integer :: itr, ifl
!
  integer :: iif
!
  if (lcn) then
    iif = itr
  else
    iif = ifl
  end if
!
  iiff = iif
!
  return
!
end function iiff
!
!     ==================================================================
!>    @brief real if function.
!
!>    @param[in] lcn logical clause
!>    @param[in] rtr true value
!>    @param[in] rfl false value
!>    @return true value if clause if true false value otherwise
!
real(lrk) function riff(lcn,rtr,rfl)
  use rd_kinds, only: lrk
  implicit none
!
  logical :: lcn
  real(lrk) :: rtr, rfl
!
  real(lrk) :: rif
!
  if (lcn) then
    rif = rtr
  else
    rif = rfl
  end if
!
  riff = rif
!
  return
!
end function riff
!
!     ==================================================================
!>    @brief character if function.
!
!>    @param[in] lcn logical clause
!>    @param[in] ctr true value
!>    @param[in] cfl false value
!>    @return true value if clause if true false value otherwise
!
!
!     ==================================================================
!>    @brief remove initial blanks from input text.
!>     returns given input text with desired size, but without initial
!>     blanks. may be truncated on return if no space left.
!>    @param[in] cin input text.
!>    @param[in] isz desired return size, may be truncated on return.
!>    @return given input text at desired size, without initial blanks.
!
!
!     ==================================================================
!>    @brief double complex number "n" argument.
!>    double polar angle, atan(imag("n")/real("n"))
!
!>    @param[in] dc input double complex number "n" to get its argument
!>    @param[in] ap argument always positive.
!>    @return given double complex number "n" argument polar angle,
!>     atan(imag(n)/real(n)) as double.
!
real(wp) function dargf(dc,ap)
  use rd_kinds, only: wp
  implicit none
!
  logical :: ap
  complex(wp) :: dc
!
  real(wp) :: dp, dr, di, darg, dpif
  intrinsic :: aimag, atan2, real, sign
!
!     double real part
  dr = real(dc)
!     check for zero real part
  if (dr .eq. 0._wp) then
!       pi as double precision
    dp = dpif()
!       real = zero, arg is pi/2 or -pi/2
    darg = dp/2._wp*sign(1._wp,dr)
  else
!       imaginary part
    di = aimag(dc)
!       argument
    darg = atan2(di,dr)
  end if
!     check for only positive values flag ap
  if (ap .and. darg .lt. 0) then
!       pi as double precision
    dp = dpif()
!       add 2*pi
    darg = darg+2._wp*dp
  end if
!
  dargf = darg
!
  return
!
end function dargf
!
!     ==================================================================
!>    @brief get the signal of given input number.
!>     returns 1d0 or -1d0 according to signal of given input number.
!>    @param[in] din number to check signal
!>    @return signal of given input number
!
real(wp) function dsgnf(din)
  use rd_kinds, only: wp
  implicit none
!
  real(wp) :: din
!
  intrinsic :: sign, real
!
  dsgnf = real(sign(1._wp,din), wp)
!
  return
!
end function dsgnf
!
!     ==================================================================
!>    @brief complex number "n" argument.
!>    real polar angle, atan(imag("n")/real("n"))
!
!>    @param[in] dc input double complex number "n" to get its argument
!>    @return given complex number "n" argument polar angle,
!>     atan(imag(n)/real(n)) as real.
!
real(lrk) function rargf(dc)
  use rd_kinds, only: lrk, wp
  implicit none
!
  complex(wp) :: dc
!
  real(wp) :: da, dargf
!
  intrinsic :: real
!
  da = dargf(dc,.false.)
  rargf = real(da, lrk)
!
  return
!
end function rargf
!
!     ==================================================================
!>    @brief number inside real precision tolerance.
!
!>    @param[in] rin number to check
!>    @param[in] rcp number to be compared to
!>    @param[in] rpr positive precision
!>    @return true if input is comparable in a precision range
!
logical function rpeqf(rin,rcp,rpr)
  use rd_kinds, only: lrk
  implicit none
!
  real(lrk) :: rin, rcp, rpr
!
  logical :: rpeq
!
  rpeq = rin .le. rcp+rpr .and. rin .ge. rcp-rpr
  rpeqf = rpeq
!
  return
!
end function rpeqf
!
!     ==================================================================
!>    @brief calculates fourth index on four basis indexing.
!
!>    @param[in] idx index desired index
!>    @param[in] iof index offset, default is one.
!>    @return fourth basis index for given index idx.
!
integer function ifidxof(idx,iof)
  implicit none
!
  integer :: idx, iof
!
  ifidxof = 4*(idx-1)+iof
!
  return
!
end function ifidxof
!
!     ==================================================================
!>    @brief calculates inverse coeficient a value.
!>    y = a*x^-1, a = sum(c(idx)*x^idx),idx = 0,2.
!
!>    @param[in] c coefficients vector
!>    @param[in] x input variable
!>    @return parameter inverse coeficient
!
real(lrk) function ivcoef(c,x)
  use rd_kinds, only: lrk
  implicit none
!
  real(lrk) :: c, x
  dimension c(3)
!
  real(lrk) :: a
  integer :: i
!
  a = 0
  do i = 0,2
    a = a+c(i+1)*x**i
  end do
!
  ivcoef = a
!
end function ivcoef
!
!     ==================================================================
!>    @brief copy a double vector v1 to double vector v
!
!>    @param[in] v1 input double vector
!>    @param[out] v output double vector
!>    @param[in] l number of lines of the input v1 vector
!>    @param[in] nl1 number of lines dimension of the input v1 vector
!>    @param[in] nl2 number of lines dimension of the output v vector
!
subroutine copv_dd(v1,v,l,nl1,nl2)
  use rd_kinds, only: wp
  implicit none
!
  integer :: l, nl1, nl2
  real(wp) :: v, v1
  dimension v1(nl1),v(nl2)
!
  integer :: ii
!
  intrinsic :: real
!
  do ii = 1,l
    v(ii) = v1(ii)
  end do
!
  return
!
end subroutine copv_dd
!
!     ==================================================================
!>    @brief copy a double vector v1 to real vector v
!
!>    @param[in] v1 input double vector
!>    @param[out] v output real vector
!>    @param[in] l number of lines of the input v1 vector
!>    @param[in] nl1 number of lines dimension of the input v1 vector
!>    @param[in] nl2 number of lines dimension of the output v vector
!
subroutine copv_dr(v1,v,l,nl1,nl2)
  use rd_kinds, only: lrk, wp
  implicit none
!
  integer :: l, nl1, nl2
  real(lrk) :: v
  real(wp) :: v1
  dimension v1(nl1),v(nl2)
!
  integer :: ii
!
  intrinsic :: real
!
  do ii = 1,l
    v(ii) = real(v1(ii), lrk)
  end do
!
  return
!
end subroutine copv_dr
!
!     ==================================================================
!>    @brief copy a integer vector v1 to integer vector v
!
!>    @param[in] v1 input integer vector
!>    @param[out] v output integer vector
!>    @param[in] l number of lines of the input v1 vector
!>    @param[in] nl1 number of lines dimension of the input v1 vector
!>    @param[in] nl2 number of lines dimension of the output v vector
!
subroutine copv_i(v1,v,l,nl1,nl2)
  implicit none
!
  integer :: v, v1, l, nl1, nl2
  dimension v1(nl1),v(nl2)
!
  integer :: ii
!
  do ii = 1,l
    v(ii) = v1(ii)
  end do
!
  return
!
end subroutine copv_i
!
!     ==================================================================
!>    @brief copy a real vector v1 to m double precision matrix.
!
!>    @param[in] v1 input real vector
!>    @param[out] m output double matrix
!>    @param[in] l number of lines of the input v1 vector
!>    @param[in] c column number of the output m matrix
!>    @param[in] nl1 number of lines dimension of the input v1 vector
!>    @param[in] nl2 number of lines dimension of the output m matrix
!>    @param[in] nc2 number of columns dimension of the output m matrix
!
subroutine copv2m_rd(v1,m,l,c,nl1,nl2,nc2)
  use rd_kinds, only: lrk, wp
  implicit none
!
  integer :: l, c, nl1, nl2, nc2
  real(lrk) :: v1
  real(wp) :: m
  dimension v1(nl1),m(nl2,nc2)
!
  integer :: ii
!
  do ii = 1,l
    m(ii,c) = real(v1(ii), wp)
  end do
!
  return
!
end subroutine copv2m_rd
!
!     ==================================================================
!>    @brief copy a real matrix m1 to m double precision.
!
!>    @param[in] m1 input real matrix
!>    @param[out] m output double matrix
!>    @param[in] l number of lines of the input m1 matrix
!>    @param[in] c number of columns of the input m1 matrix
!>    @param[in] nl1 number of lines dimension of the input m1 matrix
!>    @param[in] nc1 number of columns dimension of the input m1 matrix
!>    @param[in] nl2 number of lines dimension of the output m matrix
!>    @param[in] nc2 number of columns dimension of the output m matrix
!
subroutine copmat2_rd(m1,m,l,c,nl1,nc1,nl2,nc2)
  use rd_kinds, only: lrk, wp
  implicit none
!
!     arguments
  integer :: l, c, nl1, nc1, nl2, nc2
  real(lrk) :: m1
  real(wp) :: m
  dimension m1(nl1,nc1),m(nl2,nc2)
!
!     locals
  integer :: ii, jj
!
  intrinsic :: real
!
  do ii = 1,l
    do jj = 1,c
      m(ii,jj) = real(m1(ii,jj), wp)
    end do
  end do
!
  return
!
end subroutine copmat2_rd
!
!     ==================================================================
!>    @brief copy a real matrix m1 to m double complex with offset.
!
!>    @param[in] m1 input real matrix
!>    @param[out] m output complex matrix
!>    @param[in] l number of lines of the input m1 matrix
!>    @param[in] c number of columns of the input m1 matrix
!>    @param[in] nl1 number of lines dimension of the input m1 matrix
!>    @param[in] nc1 number of columns dimension of the input m1 matrix
!>    @param[in] nl2 number of lines dimension of the output m matrix
!>    @param[in] nc2 number of columns dimension of the output m matrix
!>    @param[in] lo line offset, start on line 1+offset, default = zero
!>    @param[in] co column offset, start on column 1+offset, default = z
!
subroutine copmat2_rct(m1,m,l,c,nl1,nc1,nl2,nc2,lo,co)
  use rd_kinds, only: lrk, wp
  implicit none
!
!     arguments
  integer :: l, c, nl1, nc1, nl2, nc2, lo, co
  real(lrk) :: m1
  complex(wp) :: m
  dimension m1(nl1,nc1),m(nl2,nc2)
!
!     locals
  integer :: i, j, ii, jj, ic
!
!     dcmplx -> double complex intrinsic
  intrinsic :: cmplx
!
  i = 0
  ic = 1+co
  do ii = 1+lo,l
    i = i+1
    j = 0
    do jj = ic,c
      j = j+1
      m(i,j) = cmplx(m1(ii,jj),0._wp, kind=wp)
    end do
  end do
!
  return
!
end subroutine copmat2_rct
!
!     ==================================================================
!>    @brief copy a real matrix m1 to m double complex.
!
!>    @param[in] m1 input real matrix
!>    @param[out] m output complex matrix
!>    @param[in] l number of lines of the input m1 matrix
!>    @param[in] c number of columns of the input m1 matrix
!>    @param[in] nl1 number of lines dimension of the input m1 matrix
!>    @param[in] nc1 number of columns dimension of the input m1 matrix
!>    @param[in] nl2 number of lines dimension of the output m matrix
!>    @param[in] nc2 number of columns dimension of the output m matrix
!
subroutine copmat2_rc(m1,m,l,c,nl1,nc1,nl2,nc2)
  use rd_kinds, only: lrk, wp
  implicit none
!
!     arguments
  integer :: l, c, nl1, nc1, nl2, nc2
  real(lrk) :: m1
  complex(wp) :: m
  dimension m1(nl1,nc1),m(nl2,nc2)
!
  call copmat2_rct(m1,m,l,c,nl1,nc1,nl2,nc2,0,0)
!
  return
!
end subroutine copmat2_rc
!
!     ==================================================================
!>    @brief copy two double precision matrices m1 to m.
!
!>    @param[in] m1 input matrix
!>    @param[out] m output matrix
!>    @param[in] l number of lines of the input m1 matrix
!>    @param[in] c number of columns of the input m1 matrix
!>    @param[in] nl1 number of lines dimension of the input m1 matrix
!>    @param[in] nc1 number of columns dimension of the input m1 matrix
!>    @param[in] nl2 number of lines dimension of the output m matrix
!>    @param[in] nc2  number of columns dimension of the output m matrix
!
subroutine copmat1_d(m1,m,l,c,nl1,nc1,nl2,nc2)
  use rd_kinds, only: wp
  implicit none
!
!     arguments
  integer :: l, c, nl1, nc1, nl2, nc2
  real(wp) :: m1, m
  dimension m1(nl1,nc1),m(nl2,nc2)
!
!     locais
  integer :: ii, jj
!
  do ii = 1,l
    do jj = 1,c
      m(ii,jj) = m1(ii,jj)
    end do
  end do
!
  return
!
end subroutine copmat1_d
!
!     ==================================================================
!>    @brief copy two real matrices m1 to m.
!
!>    @param[in] m1 input matrix
!>    @param[out] m output matrix
!>    @param[in] l number of lines of the input m1 matrix
!>    @param[in] c number of columns of the input m1 matrix
!>    @param[in] nl1 number of lines dimension of the input m1 matrix
!>    @param[in] nc1 number of columns dimension of the input m1 matrix
!>    @param[in] nl2 number of lines dimension of the output m matrix
!>    @param[in] nc2  number of columns dimension of the output m matrix
!
subroutine copmat1_r(m1,m,l,c,nl1,nc1,nl2,nc2)
  use rd_kinds, only: lrk
  implicit none
!
!     arguments
  integer :: l, c, nl1, nc1, nl2, nc2
  real(lrk) :: m1, m
  dimension m1(nl1,nc1),m(nl2,nc2)
!
!     locais
  integer :: ii, jj
!
  do ii = 1,l
    do jj = 1,c
      m(ii,jj) = m1(ii,jj)
    end do
  end do
!
  return
!
end subroutine copmat1_r
!
!     ==================================================================
!>    @brief copy two real square matrices m1 to m, convenience method.
!
!>    @param[in] m1 input matrix
!>    @param[out] m output matrix
!>    @param[in] lc number of lines and columns of the input m1 matrix
!>    @param[in] np1 dimension of the input m1 matrix
!>    @param[in] np dimension of the output m matrix
!
subroutine copmat_r(m1,m,lc,np1,np)
  use rd_kinds, only: lrk
  implicit none
!
!     arguments
  integer :: lc, np1, np
  real(lrk) :: m1, m
  dimension m1(np1,np1),m(np,np)
!
  call copmat1_r(m1,m,lc,lc,np1,np1,np,np)
!
  return
!
end subroutine copmat_r
!
!     ==================================================================
!>    @brief copy two double precision square matrices m1 to m,
!>     convenience method.
!
!>    @param[in] m1 input matrix
!>    @param[out] m output matrix
!>    @param[in] lc number of lines and columns of the input m1 matrix
!>    @param[in] np1 dimension of the input m1 matrix
!>    @param[in] np dimension of the output m matrix
!
subroutine copmat_d(m1,m,lc,np1,np)
  use rd_kinds, only: wp
  implicit none
!
!     arguments
  integer :: lc, np1, np
  real(wp) :: m1, m
  dimension m1(np1,np1),m(np,np)
!
  call copmat1_d(m1,m,lc,lc,np1,np1,np,np)
!
  return
!
end subroutine copmat_d
!
!     ==================================================================
!>    @brief copy a double complex matrix m1 to m double complex with of
!
!>    @param[in] m1 input real matrix
!>    @param[out] m output complex matrix
!>    @param[in] l number of lines of the input m1 matrix
!>    @param[in] c number of columns of the input m1 matrix
!>    @param[in] nl1 number of lines dimension of the input m1 matrix
!>    @param[in] nc1 number of columns dimension of the input m1 matrix
!>    @param[in] nl2 number of lines dimension of the output m matrix
!>    @param[in] nc2 number of columns dimension of the output m matrix
!>    @param[in] lo line offset, start on line 1+offset, default = zero
!>    @param[in] co column offset, start on column 1+offset, default = z
!
subroutine copmat2_cct(m1,m,l,c,nl1,nc1,nl2,nc2,lo,co)
  use rd_kinds, only: wp
  implicit none
!
!     arguments
  integer :: l, c, nl1, nc1, nl2, nc2, lo, co
  complex(wp) :: m, m1
  dimension m(nl2,nc2),m1(nl1,nc1)
!
!     locals
  integer :: i, j, ii, jj
!
  i = 0
  do ii = 1+lo,l
    i = i+1
    j = 0
    do jj = 1+co,c
      j = j+1
      m(i,j) = m1(ii,jj)
    end do
  end do
!
  return
!
end subroutine copmat2_cct
!
!     ==================================================================
!>    @brief copy two double complex square matrices m1 to m, convenienc
!
!>    @param[in] m1 input matrix
!>    @param[out] m output matrix
!>    @param[in] lc number of lines and columns of the input m1 matrix
!>    @param[in] np1 dimension of the input m1 matrix
!>    @param[in] np dimension of the output m matrix
!
subroutine copmat_c(m1,m,lc,np1,np)
  use rd_kinds, only: wp
  implicit none
!
!     arguments
  integer :: lc, np1, np
  complex(wp) :: m1, m
  dimension m1(np1,np1),m(np,np)
!
  call copmat2_cct(m1,m,lc,lc,np1,np1,np,np,0,0)
!
  return
!
end subroutine copmat_c
!
!     ==================================================================
!>    @brief copy a double complex matrix m1 to m double complex.
!
!>    @param[in] m1 input real matrix
!>    @param[out] m output complex matrix
!>    @param[in] l number of lines of the input m1 matrix
!>    @param[in] c number of columns of the input m1 matrix
!>    @param[in] nl1 number of lines dimension of the input m1 matrix
!>    @param[in] nc1 number of columns dimension of the input m1 matrix
!>    @param[in] nl2 number of lines dimension of the output m matrix
!>    @param[in] nc2 number of columns dimension of the output m matrix
!
subroutine copmat2_c(m1,m,l,c,nl1,nc1,nl2,nc2)
  use rd_kinds, only: wp
  implicit none
!
!     arguments
  integer :: l, c, nl1, nc1, nl2, nc2
  complex(wp) :: m, m1
  dimension m(nl2,nc2),m1(nl1,nc1)

  call copmat2_cct(m1,m,l,c,nl1,nc1,nl2,nc2,0,0)
!
  return
!
end subroutine copmat2_c
!
!     ==================================================================
!>    @brief get the double complex square input matrix diagonal
!>     elements, the output vector should be the same dimension as
!>     the input matrix.
!
!>    @param[in] m square input matrix
!>    @param[out] vout diagonal elements output vector.
!>    @param[in] np number of elements, the size.
!>    @param[in] dm matrix dimension
!
subroutine diag_c(m,vout,np,dm)
  use rd_kinds, only: wp
  implicit none
!
!     arguments
  integer :: dm, np
  complex(wp) :: m, vout
  dimension m(dm,dm),vout(dm)
!
!     locals
  integer :: ii
!
  do ii = 1,np
    vout(ii) = m(ii,ii)
  end do
!
  return
!
end subroutine diag_c
!
!     ==================================================================
!>    @brief scalar product double vector by a double number.
!
!>    @param[in] vi input vector
!>    @param[out] vo output vector
!>    @param[in] vl vector elements multiplier number
!>    @param[in] l number of elements of the input vector
!>    @param[in] nl dimension of input and output vectors
!
subroutine escv_d(vi,vo,vl,l,nl)
  use rd_kinds, only: wp
  implicit none
!
!     arguments
  integer :: l, nl
  real(wp) :: vi, vo, vl
  dimension vi(nl),vo(nl)
!
!     locals
  integer :: ii
!
  do ii = 1,l
    vo(ii) = vl*vi(ii)
  end do
!
  return
!
end subroutine escv_d
!
!     ==================================================================
!>    @brief scalar product real matrix by a real number.
!
!>    @param[in] m1 input matrix
!>    @param[out] m output matrix
!>    @param[in] v matrix elements multiplier number
!>    @param[in] l number of lines of the input matrix
!>    @param[in] c number of columns of the input matrix
!>    @param[in] nl number of lines dimension of the input AND output ma
!>    @param[in] nc number of columns dimension of the input AND output
!
subroutine escmat_r(m1,m,v,l,c,nl,nc)
  use rd_kinds, only: lrk
  implicit none
!
!     arguments
  integer :: l, c, nl, nc
  real(lrk) :: m1, m
  dimension m1(nl,nc),m(nl,nc)
  real(lrk) :: v
!
!     locals
  integer :: ii, jj
!
  do ii = 1,l
    do jj = 1,c
      m(ii,jj) = v*m1(ii,jj)
    end do
  end do
!
  return
!
end subroutine escmat_r
!
!     ==================================================================
!>    @brief scalar product double matrix by a double number.
!
!>    @param[in] m1 input matrix
!>    @param[out] m output matrix
!>    @param[in] v matrix elements multiplier number
!>    @param[in] l number of lines of the input matrix
!>    @param[in] c number of columns of the input matrix
!>    @param[in] nl number of lines dimension of the input AND output ma
!>    @param[in] nc number of columns dimension of the input AND output
!
subroutine escmat_d(m1,m,v,l,c,nl,nc)
  use rd_kinds, only: wp
  implicit none
!
!     arguments
  integer :: l, c, nl, nc
  real(wp) :: m1, m, v
  dimension m1(nl,nc),m(nl,nc)
!
!     locals
  integer :: ii, jj
!
  do ii = 1,l
    do jj = 1,c
      m(ii,jj) = v*m1(ii,jj)
    end do
  end do
!
  return
!
end subroutine escmat_d
!
!     ==================================================================
!>    @brief scalar product double complex matrix by a double complex nu
!
!>    @param[in] m1 input matrix
!>    @param[out] m output matrix
!>    @param[in] v matrix elements multiplier number
!>    @param[in] l number of lines of the input matrix
!>    @param[in] c number of columns of the input matrix
!>    @param[in] nl number of lines dimension of the input AND output ma
!>    @param[in] nc number of columns dimension of the input AND output
!
subroutine escmat_cc(m1,m,v,l,c,nl,nc)
  use rd_kinds, only: wp
  implicit none
!
!     arguments
  integer :: l, c, nl, nc
  complex(wp) :: m1, m
  dimension m1(nl,nc),m(nl,nc)
  complex(wp) :: v
!
!     locals
  integer :: ii, jj
!
  do ii = 1,l
    do jj = 1,c
      m(ii,jj) = v*m1(ii,jj)
    end do
  end do
!
  return
!
end subroutine escmat_cc
!
!     ==================================================================
!>    @brief scalar product real matrix by a real number returns complex
!
!>    @param[in] m1 input matrix
!>    @param[out] m output double complex matrix
!>    @param[in] v matrix elements multiplier number
!>    @param[in] l number of lines of the input matrix
!>    @param[in] c number of columns of the input matrix
!>    @param[in] nl number of lines dimension of the input AND output ma
!>    @param[in] nc number of columns dimension of the input AND output
!
subroutine escmat_rrc(m1,m,v,l,c,nl,nc)
  use rd_kinds, only: lrk, wp
  implicit none
!
!     arguments
  integer :: l, c, nl, nc
  real(lrk) :: m1
  complex(wp) :: m
  dimension m1(nl,nc),m(nl,nc)
  real(lrk) :: v
!
!     locals
  integer :: ii, jj
!
!     dcmplx -> double complex intrinsic
  intrinsic :: cmplx
!
  do ii = 1,l
    do jj = 1,c
      m(ii,jj) = cmplx(v*m1(ii,jj),0, kind=wp)
    end do
  end do
!
  return
!
end subroutine escmat_rrc
!
!     ==================================================================
!>    @brief scalar product real matrix by a complex number return compl
!
!>    @param[in] m1 input matrix
!>    @param[out] m output double complex matrix
!>    @param[in] v matrix elements multiplier number
!>    @param[in] l number of lines of the input matrix
!>    @param[in] c number of columns of the input matrix
!>    @param[in] nl number of lines dimension of the input AND output ma
!>    @param[in] nc number of columns dimension of the input AND output
!
subroutine escmat_rcc(m1,m,v,l,c,nl,nc)
  use rd_kinds, only: lrk, wp
  implicit none
!
!     arguments
  integer :: l, c, nl, nc
  real(lrk) :: m1
  complex(wp) :: v, m
  dimension m1(nl,nc),m(nl,nc)
!
!     locals
  integer :: ii, jj
!
  do ii = 1,l
    do jj = 1,c
      m(ii,jj) = v*m1(ii,jj)
    end do
  end do
!
  return
!
end subroutine escmat_rcc
!
!     ==================================================================
!>    @brief scalar product double matrix by a real number return real.
!
!>    @param[in] m1 input matrix
!>    @param[out] m output double complex matrix
!>    @param[in] v matrix elements multiplier number
!>    @param[in] l number of lines of the input matrix
!>    @param[in] c number of columns of the input matrix
!>    @param[in] nl number of lines dimension of the input AND output ma
!>    @param[in] nc number of columns dimension of the input AND output
!
subroutine escmat_drr(m1,m,v,l,c,nl,nc)
  use rd_kinds, only: lrk, wp
  implicit none
!
!     arguments
  integer :: l, c, nl, nc
  real(wp) :: m1
  real(lrk) :: v, m
  dimension m1(nl,nc),m(nl,nc)
!
!     locals
  integer :: ii, jj
!
  intrinsic :: real
!
  do ii = 1,l
    do jj = 1,c
      m(ii,jj) = v*real(m1(ii,jj), lrk)
    end do
  end do
!
  return
!
end subroutine escmat_drr
!
!     ==================================================================
!>    @brief inverts a diagonal double complex square matrix.
!
!>    @param[in] m input square matrix
!>    @param[out] m1 output matrix
!>    @param[in] n size of the matrices
!>    @param[in] dm dimension of the metrices
!>    @param[in] of offset, line and column starts with 1+of,
!>     default = 0
!
subroutine invmatd_ct(m,m1,n,dm,of)
  use rd_kinds, only: wp
  implicit none
!
!     arguments
  integer :: n, dm, of
  complex(wp) :: m, m1
  dimension m(dm,dm),m1(dm,dm)
!
!     locals
  integer :: i, ii
  complex(wp) :: r1
!
  intrinsic :: cmplx
!
  i = 0
  r1 = cmplx(1._wp,0._wp, kind=wp)
  do ii = 1+of,n
    i = i+1
    m1(i,i) = r1 / m(ii,ii)
  end do
!
  return
!
end subroutine invmatd_ct
!
!     ==================================================================
!>    @brief inverts a diagonal double complex square matrix.
!
!>    @param[in] m input square matrix
!>    @param[out] m1 output matrix
!>    @param[in] n size of the matrices
!>    @param[in] dm dimension of the metrices
!
subroutine invmatd_c(m,m1,n,dm)
  use rd_kinds, only: wp
  implicit none
!
!     arguments
  integer :: n, dm
  complex(wp) :: m, m1
  dimension m(dm,dm),m1(dm,dm)
!
  call invmatd_ct(m,m1,n,dm,0)
!
  return
!
end subroutine invmatd_c
!
!     ==================================================================
!>    @brief add two real vectors one in the another, the output vector
!>     must have the appropriate dimension.
!
!>    @param[in] vin1 first input vector
!>    @param[in] vin2 second input vector
!>    @param[out] vout output vector
!>    @param[in] id1 size of the first input vector
!>    @param[in] tm1 dimension of the first input vector
!>    @param[in] tm2 dimension of the second input vector
!>    @param[in] tou size of the output vector
!
subroutine joinv_r(vin1,vin2,vout,id1,tm1,tm2,tou)
  use rd_kinds, only: lrk
  implicit none
!
!     arguments
  integer :: id1, tm1, tm2, tou
  real(lrk) :: vin1, vin2, vout
  dimension vin1(tm1),vin2(tm2),vout(tou)
!
!     locals
  integer :: ii
!
  do ii = 1,tou
!
    if (ii .le. id1) then
      vout(ii) = vin1(ii)
    else if((ii - id1) .le. tm2) then
      vout(ii) = vin2(ii - id1)
    end if
!
  end do
!
  return
!
end subroutine joinv_r
!
!     ==================================================================
!>    @brief generates a diagonal matrix from an input vector.
!
!>    @param[in] v input vector
!>    @param[out] m output square diagonal matrix
!>    @param[in] n size of the vector and matrix
!>    @param[in] dv vector dimension
!>    @param[in] dm matrix dimension
!
subroutine matdia_c(v,m,n,dv,dm)
  use rd_kinds, only: wp
  implicit none
!
  integer :: n, dv, dm
  complex(wp) :: v, m
  dimension v(dv),m(dm,dm)
!     locals
  integer :: ii
!
!     initialize matrix with zeros
  call zermat_c(m,n,n,dm,dm)
!
!     diagonalize
  do ii = 1,n
    m(ii,ii) = v(ii)
  end do
!
  return
!
end subroutine matdia_c
!
!     ==================================================================
!>    @brief generates a diagonal double complex matrix from an input re
!
!>    @param[in] v input vector
!>    @param[out] m output square diagonal matrix
!>    @param[in] n size of the vector and matrix
!>    @param[in] dv vector dimension
!>    @param[in] dm matrix dimension
!
subroutine matdia_rc(v,m,n,dv,dm)
  use rd_kinds, only: lrk, wp
  implicit none
!
  integer :: n, dv, dm
  real(lrk) :: v
  complex(wp) :: m
  dimension v(dv),m(dm,dm)
!
!     locals
  integer :: ii
!
!     dcmplx -> double complex intrinsic
  intrinsic :: cmplx
!
!     initialize matrix with zeros
  call zermat_c(m,n,n,dm,dm)
!
!     diagonalize
  do ii = 1,n
    m(ii,ii) = cmplx(v(ii),0, kind=wp)
  end do
!
  return
!
end subroutine matdia_rc
!
!     ==================================================================
!>    @brief get a double complex eye matrix
!
!>    @param[out] m output double complex eye matrix
!>    @param[in] n size of the matrix
!>    @param[in] dm dimension of the matrix
!
subroutine matidn_c(m,n,dm)
  use rd_kinds, only: wp
  implicit none
!
!     arguments
  integer :: n, dm
  complex(wp) :: m
  dimension m(dm,dm)
!
!     locals
  integer :: i
  complex(wp) :: r1
!
!     dcmplx -> double complex intrinsic
  intrinsic :: cmplx
!
  r1 = cmplx(1._wp,0._wp, kind=wp)
!
!     initialize matrix with zeros
  call zermat_c(m,n,n,dm,dm)
!
!     double complex eye matrix
  do i = 1,n
    m(i,i) = r1
  end do
!
  return
!
end subroutine matidn_c
!
!     ==================================================================
!>    @brief get a real eye matrix
!
!>    @param[out] m output eye matrix
!>    @param[in] n size of the matrix
!>    @param[in] dm dimension of the matrix
!
subroutine matidn_r(m,n,dm)
  use rd_kinds, only: lrk
  implicit none
!
!     arguments
  integer :: n, dm
  real(lrk) :: m
  dimension m(dm,dm)
!
!     locals
  integer :: i
  real(lrk) :: r1
  parameter(r1 = 1)
!
!     initialize matrix with zeros
  call zermat_r(m,n,n,dm,dm)
!
!     double eye matrix
  do i = 1,n
    m(i,i) = r1
  end do
!
  return
!
end subroutine matidn_r
!
!     ==================================================================
!>    @brief order the indices of a real vector.
!>     vin and iout same dimension.
!
!>    @param[in] vin input vector
!>    @param[out] iout indices output vector
!>    @param[in] npo number of points
!>    @param[in] dm dimension input and order output vector
!>    @param[in] dec 0 ascendent, other descendent
!
subroutine ord_ir(vin,iout,npo,dm,dec)
  use rd_kinds, only: lrk
  implicit none
!
!     arguments
  integer :: dm, npo, dec, iout
  real(lrk) :: vin
  dimension iout(dm),vin(dm)
!
!     locals
  real(lrk) :: tmp, vout
  dimension vout(npo)
  integer :: i, j, k, itmp
  logical :: ok, chg, acd, islt, isgt
!
!     initial indices vector
  do i = 1,npo
    iout(i) = i
    vout(i) = vin(i)
  end do
!     flag ascendant
  acd = dec .eq. 0
!     order loop, no changes vector is ordered
  ok = .true.
  j = npo-1
  do while (ok)
    ok = .false.
    do i = 1,j
      k = i+1
!         less than
      islt = vout(k) .lt. vout(i)
!         greater than
      isgt = vout(k) .gt. vout(i)
!         ascendant or descendant change
      chg = (acd .and. islt) .or. (.not. acd .and. isgt)
!         must interchange
      if (chg) then
        itmp = iout(k)
        tmp = vout(k)
        iout(k) = iout(i)
        vout(k) = vout(i)
        iout(i) = itmp
        vout(i) = tmp
        ok = .true.
      end if
    end do
!       again if changed
  end do
!
  return
!
end subroutine ord_ir
!
!     ==================================================================
!>    @brief orders a real vector.
!>     vin and vout same dimension.
!
!>    @param[in] vin input vector
!>    @param[out] vout ordered output vector
!>    @param[in] npo number of points
!>    @param[in] dm dimension input and output vector
!>    @param[in] dec 0 ascendant, other descendant
!
subroutine ordena_r(vin,vout,npo,dm,dec)
  use rd_kinds, only: lrk
  implicit none
!     arguments
  integer :: npo, dm, dec
  real(lrk) :: vin, vout
  dimension vin(dm),vout(dm)
!
!     locals
  integer :: i, j, iout
  real(lrk) :: rin
  dimension iout(dm),rin(dm)

!     copy vin -> rin
  do i = 1,npo
    rin(i) = vin(i)
  end do
!     index order -> iout
  call ord_ir(vin,iout,npo,dm,dec)
!     order -> vout
  do i = 1,npo
    j = iout(i)
    vout(i) = rin(j)
  end do
!
  return
!
end subroutine ordena_r
!
!     ==================================================================
!>    @brief order a complex vector.
!>     based in absolute value and argument if needed.
!>     vin and vout same dimension.
!
!>    @param[in] vin input vector
!>    @param[out] vout ordered output vector
!>    @param[in] iout ordered index output vector
!>    @param[in] npo number of elements
!>    @param[in] dm input and output vectors dimensions
!>    @param[in] dec 0 ascendent, other descendent
!
subroutine ordena_c(vin,vout,iout,npo,dm,dec)
  use rd_kinds, only: lrk, wp
  implicit none
!
!     arguments
  integer :: dm, npo, dec, iout
  complex(wp) :: vin, vout
  dimension iout(dm),vin(dm),vout(dm)
!
!     locals
  complex(wp) :: tmp
  integer :: i, j, k, l
  logical :: chg, ok, acd, islt, isgt, iseq
  real(wp) :: a1, a2, dprec, dargf
  parameter (dprec = 1e-12_lrk)
!
  intrinsic :: abs
!
!     ascendant flag
  acd = dec .eq. 0
!     copy vin -> vout
  do i = 1,npo
    vout(i) = vin(i)
    iout(i) = i
  end do
!
  k = npo-1
!     order loop
!     if no changes vector is ordered
  ok = .true.
  do while(ok)
    ok = .false.
!       change flag
    do i = 1,k
      l = i+1
!         modulus
      a1 = abs(vout(l))
      a2 = abs(vout(i))
!         check equals on given precision
      iseq = abs(a1-a2) .lt. dprec
      if (iseq) then
!           very close modules, check arguments
        a1 = dargf(vout(l),.true.)
        a2 = dargf(vout(i),.true.)
      end if
!         is less than
      islt = a1 .lt. a2
!         is greater than
      isgt = a1 .gt. a2
!         positions changed
      chg = (acd .and. islt) .or. (.not. acd .and. isgt)
!         must change
      if (chg) then
        tmp = vout(l)
        j = iout(l)
        vout(l) = vout(i)
        iout(l) = iout(i)
        vout(i) = tmp
        iout(i) = j
        ok = .true.
      end if
    end do
!       again if changed
  end do
!
  return
!
end subroutine ordena_c
!
!     ==================================================================
!>    @brief sum of two real matrices m1 and m2.
!
!>    @param[in] m1 first real input matrix
!>    @param[in] m2 second real input matrix
!>    @param[out] m output real matrix m1+m2
!>    @param[in] l nr lines of the first matrix
!>    @param[in] c nr columns of the first matrix
!>    @param[in] nl dimension number of lines of the input matrices
!>    @param[in] nc dimension number of columns of the input matrices
!
subroutine sommat_r(m1,m2,m,l,c,nl,nc)
  use rd_kinds, only: lrk
  implicit none
!
!     arguments
  integer :: l, c, nl, nc
  real(lrk) :: m1, m2, m
  dimension m1(nl,nc),m2(nl,nc),m(nl,nc)
!
!     locals
  integer :: i, j
!
  do i = 1,l
    do j = 1,c
      m(i,j) = m1(i,j) + m2(i,j)
    end do
  end do
!
  return
!
end subroutine sommat_r
!
!     ==================================================================
!>    @brief sum or difference of two double matrices m1 and m2.
!
!>    @param[in] m1 first double input matrix
!>    @param[in] m2 second double input matrix
!>    @param[out] m output double matrix m1+m2
!>    @param[in] l nr lines of the first matrix
!>    @param[in] c nr columns of the first matrix
!>    @param[in] nl dimension number of lines of the input matrices
!>    @param[in] nc dimension number of columns of the input matrices
!>    @param[in] op operation 0 m1+m2, m1-m2 otherwise
!
subroutine opemat_d(m1,m2,m,l,c,nl,nc,op)
  use rd_kinds, only: wp
  implicit none
!
!     arguments
  integer :: l, c, nl, nc, op
  real(wp) :: m1, m2, m
  dimension m1(nl,nc),m2(nl,nc),m(nl,nc)
!
!     locals
  integer :: i, j
!
  do i = 1,l
    do j = 1,c
      if (op .eq. 0) then
        m(i,j) = m1(i,j)+m2(i,j)
      else
        m(i,j) = m1(i,j)-m2(i,j)
      end if
    end do
  end do
!
  return
!
end subroutine opemat_d
!
!     ==================================================================
!>    @brief sum of two double matrices m1 and m2, m1+m2.
!
!>    @param[in] m1 first double input matrix
!>    @param[in] m2 second double input matrix
!>    @param[out] m output double matrix m1+m2
!>    @param[in] l nr lines of the first matrix
!>    @param[in] c nr columns of the first matrix
!>    @param[in] nl dimension number of lines of the input matrices
!>    @param[in] nc dimension number of columns of the input matrices
!
subroutine sommat_d(m1,m2,m,l,c,nl,nc)
  use rd_kinds, only: wp
  implicit none
!
!     arguments
  integer :: l, c, nl, nc
  real(wp) :: m1, m2, m
  dimension m1(nl,nc),m2(nl,nc),m(nl,nc)
!
  call opemat_d(m1,m2,m,l,c,nl,nc,0)
!
  return
!
end subroutine sommat_d
!
!     ==================================================================
!>    @brief difference of two double matrices m1 and m2, m1-m2.
!
!>    @param[in] m1 first double input matrix
!>    @param[in] m2 second double input matrix
!>    @param[out] m output double matrix m1-m2
!>    @param[in] l nr lines of the first matrix
!>    @param[in] c nr columns of the first matrix
!>    @param[in] nl dimension number of lines of the input matrices
!>    @param[in] nc dimension number of columns of the input matrices
!
subroutine difmat_d(m1,m2,m,l,c,nl,nc)
  use rd_kinds, only: wp
  implicit none
!
!     arguments
  integer :: l, c, nl, nc
  real(wp) :: m1, m2, m
  dimension m1(nl,nc),m2(nl,nc),m(nl,nc)
!
  call opemat_d(m1,m2,m,l,c,nl,nc,1)
!
  return
!
end subroutine difmat_d
!
!     ==================================================================
!>    @brief sum of real and double matrices m1 and m2, return real.
!
!>    @param[in] m1 first rea input matrix
!>    @param[in] m2 second double input matrix
!>    @param[out] m output real matrix m1+m2
!>    @param[in] l nr lines of the first matrix
!>    @param[in] c nr columns of the first matrix
!>    @param[in] nl dimension number of lines of the input matrices
!>    @param[in] nc dimension number of columns of the input matrices
!
subroutine sommat_rd(m1,m2,m,l,c,nl,nc)
  use rd_kinds, only: lrk, wp
  implicit none
!
!     arguments
  integer :: l, c, nl, nc
  real(lrk) :: m1, m
  real(wp) :: m2
  dimension m1(nl,nc),m(nl,nc),m2(nl,nc)
!
!     locals
  integer :: i, j
!
  intrinsic :: real
!
  do i = 1,l
    do j = 1,c
      m(i,j) = m1(i,j)+real(m2(i,j), lrk)
    end do
  end do
!
  return
!
end subroutine sommat_rd
!
!     ==================================================================
!>    @brief sum of real matrix m1 and double complex m2,
!>     return double complex.
!
!>    @param[in] m1 first real input matrix
!>    @param[in] m2 second double complex input matrix
!>    @param[out] m output double complex  matrix m1+m2
!>    @param[in] l nr lines of the first matrix
!>    @param[in] c nr columns of the first matrix
!>    @param[in] nl dimension number of lines of the input matrices
!>    @param[in] nc dimension number of columns of the input matrices
!
subroutine sommat_rdc(m1,m2,m,l,c,nl,nc)
  use rd_kinds, only: lrk, wp
  implicit none
!
!     arguments
  integer :: l, c, nl, nc
  real(lrk) :: m1
  complex(wp) :: m2, m
  dimension m1(nl,nc),m2(nl,nc),m(nl,nc)
!
!     locals
  integer :: i, j
!
!     dcmplx -> double complex intrinsic
  intrinsic :: aimag, cmplx, real
!
  do i = 1,l
    do j = 1,c
      m(i,j) = cmplx((real(m1(i,j), wp)+real(m2(i,j), wp)),aimag(m2(i,j)), kind=wp)
    end do
  end do
!
  return
!
end subroutine sommat_rdc
!
!     ==================================================================
!>    @brief sum of double matrix m1 and real m2 return real.
!
!>    @param[in] m1 first double input matrix
!>    @param[in] m2 second real input matrix
!>    @param[out] m output real matrix m1+m2
!>    @param[in] l nr lines of the first matrix
!>    @param[in] c nr columns of the first matrix
!>    @param[in] nl dimension number of lines of the input matrices
!>    @param[in] nc dimension number of columns of the input matrices
!
subroutine sommat_drr(m1,m2,m,l,c,nl,nc)
  use rd_kinds, only: lrk, wp
  implicit none
!
!     arguments
  integer :: l, c, nl, nc
  real(wp) :: m1
  real(lrk) :: m2, m
  dimension m1(nl,nc),m2(nl,nc),m(nl,nc)
!
!     locals
  integer :: i, j
!
  intrinsic :: real
!
  do i = 1,l
    do j = 1,c
      m(i,j) = real(m1(i,j), lrk)+m2(i,j)
    end do
  end do
!
  return
!
end subroutine sommat_drr
!
!     ==================================================================
!>    @brief sum of double matrix m1 and real m2 return double.
!
!>    @param[in] m1 first double input matrix
!>    @param[in] m2 second real input matrix
!>    @param[out] m output double matrix m1+m2
!>    @param[in] l nr lines of the first matrix
!>    @param[in] c nr columns of the first matrix
!>    @param[in] nl dimension number of lines of the input matrices
!>    @param[in] nc dimension number of columns of the input matrices
!
subroutine sommat_drd(m1,m2,m,l,c,nl,nc)
  use rd_kinds, only: lrk, wp
  implicit none
!
!     arguments
  integer :: l, c, nl, nc
  real(wp) :: m1, m
  real(lrk) :: m2
  dimension m1(nl,nc),m2(nl,nc),m(nl,nc)
!
!     locals
  integer :: i, j
!
  do i = 1,l
    do j = 1,c
      m(i,j) = m1(i,j)+m2(i,j)
    end do
  end do
!
  return
!
end subroutine sommat_drd
!
!     ==================================================================
!>    @brief sum of complex matrices m1 and m2.
!
!>    @param[in] m1 first input matrix
!>    @param[in] m2 second input matrix
!>    @param[out] m output matrix m1+m2
!>    @param[in] l nr lines of the first matrix
!>    @param[in] c nr columns of the first matrix
!>    @param[in] nl dimension number of lines of the input matrices
!>    @param[in] nc dimension number of columns of the input matrices
!
subroutine sommat_c(m1,m2,m,l,c,nl,nc)
  use rd_kinds, only: wp
  implicit none
!
!     arguments
  integer :: l, c, nl, nc
  complex(wp) :: m1, m2, m
  dimension m1(nl,nc),m2(nl,nc),m(nl,nc)
!
!     locals
  integer :: i, j
!
  do i = 1,l
    do j = 1,c
      m(i,j) = m1(i,j) + m2(i,j)
    end do
  end do
!
  return
!
end subroutine sommat_c
!
!     ==================================================================
!>    @brief sum elements of a vector.
!
!>    @param[in] vin input vector
!>    @param[in] dm input vector dimension
!>    @param[in] np number of elements
!>    @return the sum of the vector elements
!
real(lrk) function sumv_r(vin,dm,np)
  use rd_kinds, only: lrk
  implicit none
!
!     arguments
  integer :: dm, np
  real(lrk) :: vin
  dimension vin(dm)
!
!     locals
  integer :: i, cnt
  real(lrk) :: rsum
!
!     number of elements
  cnt = np
!     if cnt = zero, undefinded
  if (cnt .eq. 0) cnt = dm
!     init sum
  rsum = 0._lrk
!
  do i = 1,cnt
    rsum = rsum + vin(i)
  end do
!
!     return
  sumv_r = rsum
!
  return
!
end function sumv_r
!
!     ==================================================================
!>    @brief put zeros in a real vector.
!
!>    @param[in,out] v input and output vector
!>    @param[in] l number of lines of the input vector
!>    @param[in] nl dimension number of lines of the input vector
!
subroutine zervec_r(v,l,nl)
  use rd_kinds, only: lrk
  implicit none
!
  integer :: l, nl
  real(lrk) :: v
  dimension v(nl)
!
  integer :: i
!
  do i = 1,l
    v(i) = 0._lrk
  end do
!
  return
!
end subroutine zervec_r
!
!     ==================================================================
!>    @brief put zeros in a double vector.
!
!>    @param[in,out] v input and output vector
!>    @param[in] l number of lines of the input vector
!>    @param[in] nl dimension number of lines of the input vector
!
subroutine zervec_d(v,l,nl)
  use rd_kinds, only: lrk, wp
  implicit none
!
  integer :: l, nl
  real(wp) :: v
  dimension v(nl)
!
  integer :: i
!
  do i = 1,l
    v(i) = 0._lrk
  end do
!
  return
!
end subroutine zervec_d
!
!     ==================================================================
!>    @brief put zeros in a integer vector.
!
!>    @param[in,out] v input and output vector
!>    @param[in] l number of lines of the input vector
!>    @param[in] nl dimension number of lines of the input vector
!
subroutine zervec_i(v,l,nl)
  implicit none
!
  integer :: l, nl
  integer :: v
  dimension v(nl)
!
  integer :: i
!
  do i = 1,l
    v(i) = 0
  end do
!
  return
!
end subroutine zervec_i
!
!     ==================================================================
!>    @brief put zeros in a complex vector.
!
!>    @param[in,out] v input and output vector
!>    @param[in] l number of lines of the input vector
!>    @param[in] nl dimension number of lines of the input vector
!
subroutine zervec_c(v,l,nl)
  use rd_kinds, only: wp
  implicit none
!
  integer :: l, nl
  complex(wp) :: v
  dimension v(nl)
!
  integer :: i
!
  do i = 1,l
    v(i) = 0
  end do
!
  return
!
end subroutine zervec_c
!
!     ==================================================================
!>    @brief put zeros in a real matrix.
!
!>    @param[in,out] m input and output matrix
!>    @param[in] l number of lines of the input matrix
!>    @param[in] c number of columns of the input matrix
!>    @param[in] nl dimension number of lines of the input matrix
!>    @param[in] nc dimension number of columns of the input matrix
!
subroutine zermat_r(m,l,c,nl,nc)
  use rd_kinds, only: lrk
  implicit none
!
  integer :: i, j
!
  integer :: l, c, nl, nc
  real(lrk) :: m
  dimension m(nl,nc)
!
  do i = 1,l
    do j = 1,c
      m(i,j) = 0._lrk
    end do
  end do
!
  return
!
end subroutine zermat_r
!
!     ==================================================================
!>    @brief put zeros in a double complex matrix.
!
!>    @param[in,out] m input and output matrix
!>    @param[in] l number of lines of the input matrix
!>    @param[in] c number of columns of the input matrix
!>    @param[in] nl dimension number of lines of the input matrix
!>    @param[in] nc dimension number of columns of the input matrix
!
subroutine zermat_c(m,l,c,nl,nc)
  use rd_kinds, only: wp
  implicit none
!
!     arguments
  integer :: l, c, nl, nc
  complex(wp) :: m
  dimension m(nl,nc)
!
!     locals
  integer :: i, j
!
  complex(wp) :: j0
!
!     dcmplx -> double complex intrinsic
  intrinsic :: cmplx
!
  j0 = cmplx(0._wp,0._wp, kind=wp)
  do i = 1,l
    do j = 1,c
      m(i,j) = j0
    end do
  end do
!
  return
!
end subroutine zermat_c
!
!     ==================================================================
!>    @brief put zeros in a double precision matrix.
!
!>    @param[in,out] m input and output matrix
!>    @param[in] l number of lines of the input matrix
!>    @param[in] c number of columns of the input matrix
!>    @param[in] nl dimension number of lines of the input matrix
!>    @param[in] nc dimension number of columns of the input matrix
!
subroutine zermat_d(m,l,c,nl,nc)
  use rd_kinds, only: lrk, wp
  implicit none
!
!     locals
  integer :: i, j
!
!     arguments
  integer :: l, c, nl, nc
  real(wp) :: m
  dimension m(nl,nc)
!
  real(wp) :: d0
  parameter (d0 = 0._lrk)
!
  intrinsic :: real
!
  do i = 1,l
    do j = 1,c
      m(i,j) = d0
    end do
  end do
!
  return
!
end subroutine zermat_d
!
!     ==================================================================
!>    @brief get a double part from a complex ones, either real or
!>     imaginary part.
!
!>    @param[out] m output matrix to be get
!>    @param[in] o input matrix to extract
!>    @param[in] job 0 get real, imaginary otherwise
!>    @param[in] l rows of m and o
!>    @param[in] c columns of m and o
!>    @param[in] nl row dimension for m and o
!>    @param[in] nc column dimension for m and o
!
subroutine cplxmat_d(m,o,job,l,c,nl,nc)
  use rd_kinds, only: wp
  implicit none
!
!     locals
  integer :: i, j
!
!     arguments
  integer :: job, l, c, nl, nc
  complex(wp) :: o
  real(wp) :: m
  dimension m(nl,nc),o(nl,nc)
!
  intrinsic :: aimag, real
!
  call zermat_d(m,l,c,nl,nc)
  do i = 1,l
    do j = 1,c
      if (job .eq. 0) then
        m(i,j) = real(o(i,j), wp)
      else
        m(i,j) = aimag(o(i,j))
      end if
    end do
  end do
!
  return
!
end subroutine cplxmat_d
!
!     ==================================================================
!>    @brief vetorial coordinates rotation, referece:<br>
!>     Bently, D E, Hatch, C T<br>
!>     "Fundamentals of Rotating Machinery Diagnostics (Design and Manuf
!>     Bently Pressurized Bearing Press, 2002.
!
!>    @param[in] rspx complex response x direction (horizontal)
!>    @param[in] rspz complex response z direction (vertical)
!>    @param[in] rad rotation angle (rad)
!>    @param[out] rx rotated complex response x direction (horizontal)
!>    @param[out] rz rotated complex response z direction (vertical)
!
subroutine vrotate(rspx,rspz,rad,rx,rz)
  use rd_kinds, only: lrk, wp
  implicit none
!
  real(lrk) :: rad
  complex(wp) :: rspx, rspz, rx, rz
!
  real(wp) :: xd, xq, zd, zq, xr, xi, zr, zi
!
!     dcmplx -> double complex intrinsic
  intrinsic :: aimag, cos, real, sin, cmplx
!
!     x real part
  xd = real(rspx)
!     x imag part
  xq = aimag(rspx)
!     z real part
  zd = real(rspz)
!     z imag part
  zq = aimag(rspz)
!     response offset angle (rad))
!     complex x result value
  xr = xd*cos(rad)+zd*sin(rad)
  xi = xq*cos(rad)+zq*sin(rad)
!     complex z result value
  zr = zd*cos(rad)-xd*sin(rad)
  zi = zq*cos(rad)-xq*sin(rad)
!     result
  rx = cmplx(xr,xi, kind=wp)
  rz = cmplx(zr,zi, kind=wp)
!
  return
!
end subroutine vrotate
!
!     ==================================================================
!>    @brief special Lagrange quadratic interpolation,
!>    * HEY * vx vector should be always in crescent order.
!
!>    @param[in] vx x points vector strictly crescent
!>    @param[in] vy y points vector
!>    @param[in] x x interpolation point
!>    @param[in] np number of points vx and vy
!>    @param[in] dm vectors dimension
!>    @param[out] errmsg return an error message
!>    @param[out] ok return flag. unsuccessful if <0.
!>    @return interpolated value
!
real(lrk) function intlag(vx,vy,x,np,dm,ok,errmsg)
  use rd_textfun, only: femsgf, fomsgf, fwmsgf
  use rd_kinds, only: lrk
  implicit none
!
!     arguments
  character(len=99) :: errmsg
  integer :: np, dm, ok
  real(lrk) :: x, vx, vy
  dimension vx(dm),vy(dm)
!
!     locals
  character(len=1) :: c1
  character(len=6) :: nmm
  integer :: ii, jj, kk
  real(lrk) :: ci, bg, ss, rt
  dimension c1(3)
!     precision limit  under / upper
  parameter(nmm = 'intlag',c1 = (/' ','<','>'/),ci = 1E-15_lrk,bg = 1E15_lrk)
!
  intrinsic :: abs
!
!     init return ok
  ok = 0
!     default return
  intlag = 0._lrk
!
!     check for at least two points
  if (np .lt. 2) then
    errmsg = fomsgf(99, nmm,7,c1(1),0)
    ok=-1
    return
  end if
!
!     special Lagrange interpolation
!     loop through points
  do ii = 2,np-1
!       default return
    rt = 0._lrk
!       same x point
    if (abs(x-vx(ii)) .le. ci) then
      intlag = vy(ii)
      return
    end if
!
    if(vx(ii) .gt. x) then
      do jj = ii-1,ii+1
        ss = vy(jj)
        do kk = ii-1,ii+1
          if (kk .ne. jj .and. abs(vx(kk)-vx(jj)) .ge. ci .and.&
          &abs(ss) .le. bg .and. abs(x-vx(kk)) .le. bg) then
!               division by zero check
            if (vx(jj)-vx(kk) .eq. 0._lrk) then
              errmsg = femsgf(99, nmm,8,3,0)
              ok = -1
              return
            end if
            ss = ss*(x-vx(kk))/(vx(jj)-vx(kk))
          end if
!             kk loop
        end do
        rt = rt+ss
!           jj loop
      end do
!         return
      intlag = rt
      return
    end if
!       fim loop ii
  end do
!     prepare linear extrapolation
  if (x .lt. vx(1)) then
!       before vector x warning
    errmsg = fwmsgf(99, nmm,27,c1(2),0)
    rt = (x-vx(1))*(vy(2)-vy(1))/(vx(2)-vx(1))+vy(1)
  else
    jj = np-1
!       after vector x warning
    errmsg = fwmsgf(99, nmm,27,c1(3),0)
    rt = (x-vx(jj))*(vy(np)-vy(jj))/(vx(np)-vx(jj))+vy(jj)
  end if
!
!     return
  intlag = rt
  return
!
end function intlag
!
!     ==================================================================
!>    @brief x intersection point betwen a line defined
!>    by two points and another with slope "a"
!>    passing trhought the origin (y = ax + b).
!
!>    @param[in] x1 point 1 x
!>    @param[in] y1 point 1 y
!>    @param[in] x2 point 2 x
!>    @param[in] y2 point 2 y
!>    @param[in] a angular coeficient of the line that crosses the origi
!>    @param[out] x return x intercection point
!>    @param[out] y return y intercection point
!
subroutine f1camp(x1,y1,x2,y2,a,x,y)
  use rd_textfun, only: femsgf
  use rd_kinds, only: lrk
  implicit none
!
!     arguments
  real(lrk) :: x1, y1, x2, y2, a, x, y
!
!     locals
  real(lrk) :: ai, bi, dl, prec
  parameter (prec = 1e-12_lrk)

  character(len=6) :: nmm
  parameter(nmm = 'f1camp')
  character(len=99) :: errmsg
!
  intrinsic :: abs
!
!     delta x
  dl = x2-x1
  if (abs(dl) .lt. prec) then
    errmsg = femsgf(99, nmm,8,3,0)
    call lmsg(1,errmsg)
  end if
!     angular coefficient
  ai = (y2-y1)/dl
!     y cross
  bi = y1-(ai*x1)
!     intersection point
  x = -bi/(ai-a)
  y = (ai*x)+bi
!
  return
!
end subroutine f1camp
!
!     ==================================================================
!>    @brief calculates the maximum point of a quadratic function.
!
!>    @param[out] x x value of the maximum point
!>    @param[out] y y value of the maximum point
!>    @param[in] c vector of the quadratic coefficients
!>          c(1)*x^2+c(2)*x+c(3)
!
subroutine mxqd(x,y,c)
  use rd_kinds, only: lrk, wp
  implicit none
!
!     arguments
  real(wp) :: x, y, c
  dimension c(3)

  character(len=4) :: nmm
  real(wp) :: prec
  parameter(nmm = 'mxqd',prec = 1e-18_lrk)
  intrinsic :: abs
!
!     check division zero
  if (abs(c(1)) .lt. prec) call elmsge(1,8,nmm)
!
  x = -c(2)/2/c(1)
  y = c(1)*x**2+c(2)*x+c(3)
!
  return
!
end subroutine mxqd
!
!     ==================================================================
!>    @brief calculates the maximum point of a quadratic function.
!>    Convenience real arguments.
!>    @see mxqd
!
!>    @param[out] x x value of the maximum point
!>    @param[out] y y value of the maximum point
!>    @param[in] c vector of the quadratic coefficients
!>          c(1)*x^2+c(2)*x+c(3)
!
subroutine mxqr(x,y,c)
  use rd_kinds, only: lrk, wp
  implicit none
!
!     arguments
  real(lrk) :: x, y
  real(lrk) :: c
  dimension c(3)
  !
  integer :: i
  real(wp) :: xd, yd, cd
  dimension cd(3)
!
  intrinsic :: real
!
  do i = 1,3
    cd(i) = real(c(i), wp)
  end do
  call mxqd(xd,yd,cd)
  x = real(xd, lrk)
  y = real(yd, lrk)
!
  return
!
end subroutine mxqr
!
!     ==================================================================
!>    @brief calculates quadratic coefficients polynom for given three
!>     x and y cartesian points, c(1)*x^2+c(2)*x+c(3)
!
!>    @param[in] x three x points vector
!>    @param[in] y three y points vector
!>    @param[out] c quadratic coefficients, c(1)*x^2+c(2)*x+c(3)
!
subroutine qcfd(x,y,c)
  use rd_kinds, only: lrk, wp
  implicit none
!
  real(wp) :: x, y, c
  dimension x(3),y(3),c(3)
!
  character(len=4) :: nmm
  real(wp) :: dx, dy, dx2, prec
  parameter(nmm = 'qcfd',prec = 1e-18_lrk)
!
  dx = x(2)-x(1)
  dy = y(2)-y(1)
!
!     delta x too small, stop
  if (abs(dx) .lt. prec&
  &.or. abs(x(3)-x(2)) .lt. prec&
!     'invalid data ' !8
  &.or. abs(x(3)-x(1)) .lt. prec) call elmsge(1,8,nmm)
!
  dx2 = x(2)**2-x(1)**2
!
  c(1) = (dy*(x(1)-x(3))+(y(3)-y(1))*dx) /&
  &((x(1)-x(3))*dx2+dx*(x(3)**2-x(1)**2))
  c(2) = (dy-c(1)*dx2)/dx
  c(3) = y(1)-c(1)*x(1)**2-c(2)*x(1)
!
  return
!
end subroutine qcfd
!
!     ==================================================================
!>    @brief calculates quadratic coefficients polynom for given three
!>     x and y cartesian points, c(1)*x^2+c(2)*x+c(3).
!>    Convenience real arguments.
!>    @see qcfd
!
!>    @param[in] x three x points vector
!>    @param[in] y three y points vector
!>    @param[out] c quadratic coefficients, c(1)*x^2+c(2)*x+c(3)
!
subroutine qcfr(x,y,c)
  use rd_kinds, only: lrk, wp
  implicit none
!
  real(lrk) :: x, y, c
  dimension x(3),y(3),c(3)
!
  integer :: i
  real(wp) :: xd, yd, cd
  dimension xd(3),yd(3),cd(3)
!
  intrinsic :: real
!
  do i = 1,3
    xd(i) = real(x(i), wp)
    yd(i) = real(y(i), wp)
  end do
  call qcfd(xd,yd,cd)
  do i = 1,3
    c(i) = real(cd(i), lrk)
  end do
!
  return
!
end subroutine qcfr
!
!     ==================================================================
!>    @brief calculates median value for a given input vector
!
!>    @param[in] nn number o element of input vector
!>    @param[in] dm dimension of input and output vectors
!>    @param[in] vi data input vector
!>    @return median value for a given input vector
!
real(lrk) function medianf(nn,dm,vi)
  use rd_kinds, only: lrk
  implicit none
!
  integer :: nn, dm
  real(lrk) :: vi
  dimension vi(dm)
!
  integer :: ii, jj
  real(lrk) :: vo, median
  dimension vo(dm)
!
  intrinsic :: mod
!
!     orders input vi -> vo
  call ordena_r(vi,vo,nn,dm,0)
!     index
  ii = nn/2+1
!     check for even or odd
  if (mod(nn,2) .eq. 0 ) then
!       even
    jj = nn/2
    median = (vo(ii)+vo(jj))/2.0_lrk
  else
!       odd
    median = vo(ii)
  end if
!
  medianf = median
!
  return
!
end function medianf
!
!     ==================================================================
!>    @brief implements a median filter.
!>     medfit1w is the one-dimensional median filter on given
!>     input data vector computed over a sliding window of width w.<br>
!>    Copyright (C) 1993-2011, by Peter I. Corke.<br>
!>    This is part of The Machine Vision Toolbox for Matlab (MVTB).
!
!>    @param[in] nn number o element of input vector
!>    @param[in] dm dimension of input and output vectors
!>    @param[in] ww window width
!>    @param[in] vi data input vector
!>    @param[out] vo data output vector
!
subroutine medfilt1w(nn,dm,ww,vi,vo)
  use rd_kinds, only: lrk
  implicit none
!
  integer :: nn, dm, ww
  real(lrk) :: vi, vo
  dimension vi(dm),vo(dm)
!
  integer :: ii, jj, w1, w2
  real(lrk) :: s0, sl, tv, medianf, tc
  dimension tv(ww+2,nn+ww),tc(ww+2)
  character(len=9) :: c9
  parameter (c9 = 'medfilt1w')
!
  intrinsic :: floor, real
!
  if (ww .lt. 1 .or. ww .gt. nn) then
!       invalid option message, stop.
    call elmsge(1,6,c9)
  end if
!     check for window value
  if (ww .eq. 1) then
!       no mean window, just copy
    do ii = 1,nn
      vo(ii) = vi(ii)
    end do
  else
    w2 = floor(real(ww, lrk)/2)
!       always odd
    w1 = 2*w2+1
!       first value
    s0 = vi(1)
!       last value
    sl = vi(nn)
!       prepare mean lines
    do ii = 0,w1-1
!         first part
      do jj = 1,ii
        tv(ii+1,jj) = s0
      end do
!         intermediate
      do jj = 1,nn
        tv(ii+1,ii+jj) = vi(jj)
      end do
!         last part
      do jj = 1,w1-ii-1
        tv(ii+1,ii+nn+jj) = sl
      end do
    end do
!       calculate mean column values
    do jj = 1,nn+w1-1
      do ii = 1,w1
        tc(ii) = tv(ii,jj)
      end do
      tv(ww+2,jj) = medianf(w1,ww+2,tc)
    end do
!       return -> vo
    do ii = 1,nn
      vo(ii) = tv(ww+2,w2+ii)
    end do
  end if
!
  return
!
end subroutine medfilt1w
!
