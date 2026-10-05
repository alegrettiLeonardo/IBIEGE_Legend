!     $Id$
!     ==================================================================
!
!>    @file linhael.f
!>    @author francisco
!>    @brief estatic elastic line, last changes:<br>
!>    new module - francisco - 03/03/2009<br>
!>    changed max_dl last point and signal follow - francisco 29/10/2014
!>    added dgetrf that was removed from dgetri.f - francisco - 16/04/20
!>    adeed HPGL plot support - francisco - jun/15<br>
!>    added central error messages - franciso - apr-19<br>
!>    changed mxb = 99 francisco - apr-20<br>
!>    updated matrix inversion for Cholesky- francisco - may-20<br>
!>    updated Cholesky to LU solver- francisco - oct-20<br>
!>    added bending stress calculation, bendst - francisco<br>
!>    removed quad_r, updated by qcfd on matfun - francisco mar-21<br>
!>    removed dmxint, updated by mxqd on matfun - francisco mar-21<br>
!>    added lgndsp, split calculation from linhael - francisco - jul-21<
!>    added bstress - francisco - jul-21<br>
!>    added dist. force on lgndsp - francisco - sep-21<br>
!>    added precision on derivative check, max_dl - francisco - sep-21<b
!>    added distributed force angle on coepes and decomp - francisco - d
!>     added fxstf fixed stiffnes for no displacement on bespst - franci
!
!     ==================================================================
!>    @brief calculates bending moment.
!>     Based on second derivative of interpolation polynom coefficients,
!>     q(y) = zz(1)*y^3+zz(2)*y^2+zz(3)*y+zz(4).
!
!>    @param[in] zz interpolation polynom coefficients
!>    @param[in] yy point to interpolate
!>    @return bending moment on given point

real(lrk) function ibmf(zz,yy)
  use rd_kinds, only: lrk
  implicit none
!
  real(lrk) :: yy, zz
  dimension zz(4)
!
  ibmf = 6*zz(1)*yy+2*zz(2)
!
  return
!
end function ibmf
!
!     ==================================================================
!>    @brief calculates shear force.
!>     Based on third derivative of interpolation polynom coefficients,
!>     q(y) = zz(1)*y^3+zz(2)*y^2+zz(3)*y+zz(4). Constant on element.
!
!>    @param[in] zz interpolation polynom coefficients
!>    @return shear force
!
real(lrk) function isff(zz)
  use rd_kinds, only: lrk
  implicit none
!
  real(lrk) :: zz
  dimension zz(4)
!
  isff = 6*zz(1)
!
  return
!
end function isff
!
!!     =================================================================
!!>    @brief calculates bending DOF interpolation polynom coefficients.
!!>    q(y) = a1*y^3+a2*y^2+a3*y+a4
!!
!!>    @param[in] l1 y first point
!!>    @param[in] l2 y second point
!!>    @param[in] v1 displacement on first point (m)
!!>    @param[in] v2 displacement on second point (m)
!!>    @param[in] t1 rotation on first point (rad)
!!>    @param[in] t2 rotation on second point (rad)
!!>    @param[out] aa interpolation polynom coefficients
!!
!      subroutine icoef(l1,l2,v1,v2,t1,t2,aa)
!      implicit none
!!
!      real l1,l2,v1,v2,t1,t2,aa
!      dimension aa(4)
!!
!      real d,n,prec
!      character*5 nmm
!      parameter (nmm = 'icoef',prec = 1e-15)
!!
!      intrinsic abs
!!
!      d = -l2**3+3.0*l1*l2**2-3.0*l1**2*l2+l1**3
!!     invalid data error, stop
!      if (abs(d) .lt. prec) call elmsge(1,8,nmm)
!!
!      n = -2.0*v2+2.0*v1+l2*t2+l1*(-t2-t1)+l2*t1
!      aa(1) = -n/d
!!
!      n = -3.0*l2*v2+l1*(-3.0*v2+l2*t2-l2*t1)+(3.0*l2+3*l1)*v1+
!     & l2**2*t2+l1**2*(-2.0*t2-t1)+2.0*l2**2*t1
!      aa(2) = n/d
!!
!      n = l1*(-6.0*l2*v2+2.0*l2**2*t2+l2**2*t1)+6.0*l1*l2*v1+
!     & l1**2*(-l2*t2-2.0*l2*t1)-l1**3*t2+l2**3*t1
!      aa(3) = -n/d
!!
!      n = l1**2*(-3.0*l2*v2+l2**2*t2-l2**2*t1)+l1**3*(v2-l2*t2)+
!     & (3.0*l1*l2**2-l2**3)*v1+l1*l2**3*t1
!      aa(4) = n/d
!!
!      return
!!
!      end subroutine icoef


!     New icoef subroutinr - alexh 19/02/2026
!     ==================================================================
!>    @brief calculates bending DOF interpolation polynom coefficients.
!>    Robust version: uses local coordinates and internal double precisi
!>    to avoid catastrophic cancellation and IEEE underflow on modern co
!>
!>    q(y) = a1*y^3+a2*y^2+a3*y+a4
!>
!>    @param[in] l1 y first point
!>    @param[in] l2 y second point
!>    @param[in] v1 displacement on first point (m)
!>    @param[in] v2 displacement on second point (m)
!>    @param[in] t1 rotation on first point (rad)
!>    @param[in] t2 rotation on second point (rad)
!>    @param[out] aa interpolation polynom coefficients
!
subroutine icoef(l1,l2,v1,v2,t1,t2,aa)
  use rd_kinds, only: lrk, wp
  implicit none
!
!     Input/Output arguments (keep REAL to maintain compatibility)
  real(lrk) :: l1, l2, v1, v2, t1, t2, aa
  dimension aa(4)
!
!     Internal variables using DOUBLE PRECISION for stability
  real(wp) :: dl, dy, dv, sum_t
  real(wp) :: c_loc(4) ! local coefficients: d, c, b, a
  real(wp) :: y0
  real(wp) :: prec
  character(len=5) :: nmm
  parameter (nmm = 'icoef', prec = 1e-14_wp)
!
  intrinsic :: abs, real
!
!     1. Calculate element length (Delta L)
!     Using double precision to capture small differences in large coord
  y0 = real(l1, wp)
  dl = real(l2, wp) - y0

!     Check for singularity (length too close to zero)
  if (abs(dl) .lt. prec) call elmsge(1,8,nmm)
!
!     2. Calculate coefficients in LOCAL coordinates (0 to L)
!     Polynomial: p(x) = a*x^3 + b*x^2 + c*x + d
!     where x = y - l1
!
!     Standard Hermite interpolation logic:
!     d = displacement at start
  c_loc(4) = real(v1, wp)

!     c = slope at start
  c_loc(3) = real(t1, wp)

!     Auxiliary terms for a and b
  dy = dl*dl       ! L^2
  dv = real(v2, wp) - real(v1, wp)
  sum_t = real(t1, wp) + real(t2, wp)

!     b = (3*dv/L^2) - (2*t1 + t2)/L
!     Multiplied by L*L to avoid early division:
!     b*L^2 = 3*dv - L*(2*t1 + t2)
  c_loc(2) = (3.0_wp * dv - dl * (2.0_wp * real(t1, wp) + real(t2, wp))) / dy

!     a = (2*(v1-v2)/L^3) + (t1+t2)/L^2
!     a = (-2*dv + L*(t1+t2)) / L^3
  c_loc(1) = (-2.0_wp * dv + dl * sum_t) / (dy * dl)
!
!     3. Convert Local to Global coefficients
!     We shift x back to y: x = (y - y0)
!     p(y) = a(y-y0)^3 + b(y-y0)^2 + c(y-y0) + d
!
!     Expanding and grouping by powers of y gives the global AA coeffici
!     We use nested multiplication (Horner-like) to minimize error.
!
!     Global a1 (y^3 term) is just local a
  aa(1) = real(c_loc(1), lrk)

!     Global a2 (y^2 term) = b - 3*a*y0
  aa(2) = real(c_loc(2) - 3.0_wp * c_loc(1) * y0, lrk)

!     Global a3 (y term) = c - 2*b*y0 + 3*a*y0^2
!     Factorized: c - y0*(2*b - 3*a*y0)
  aa(3) = real(c_loc(3) - y0 * (2.0_wp * c_loc(2)- 3.0_wp * c_loc(1) * y0), lrk)

!     Global a4 (constant term) = d - c*y0 + b*y0^2 - a*y0^3
!     Factorized: d - y0*(c - y0*(b - y0*a))
  aa(4) = real(c_loc(4) - y0 * (c_loc(3) - y0 * (c_loc(2)- y0 * c_loc(1))), lrk)
!
  return
!
end subroutine icoef
!
!
!     ==================================================================
!>    @brief calculates bend moment and shear force.
!
!>    @param[in] nn total number of divisions
!>    @param[in] mts divisions and properties dimension
!>    @param[in] mtg general coordinate dimension (xx,zz,tx,tz)
!>    @param[in] isf shear smoothin factor, 1 no smoothing
!>    @param[in] y section coordinates positions vector (m)
!>    @param[in] es section material Young modulus vector  (Pa)
!>    @param[in] ie section second order moment of inertia vector (m^4)
!>    @param[in] dl general coordinate displacements vector (m)
!>    @param[out] mi bending moment matrix on y section coordinates x an
!>    @param[out] ys (nn-1)*2 y positions,start and end section position
!>    @param[out] si shear on ys positions x and z (N)
!>    @param[out] errmsg return an error message
!>    @param[out] ok return flag. unsuccessful if <0.
!
subroutine bendst(nn,mts,mtg,isf,y,es,ie,dl,mi,ys,si,errmsg,ok)
  use rd_textfun, only: fomsgf
  use com_kasc, only: vkasc
  use rd_kinds, only: lrk, wp
  implicit none
!
!     arguments
  integer :: nn, mts, mtg, isf, ok
  real(lrk) :: y, es, ie, mi, ys, si
  real(wp) :: dl
  dimension y(mts),es(mts),ie(mts),dl(mtg),&
  &mi(mts,2),ys(mts),si(mts,2),isf(2)
  character(len=99) :: errmsg
!
!     common local lmts same as passed mts
  integer :: lmts
  parameter (lmts = 999)
!
!     locals
  integer :: ii, j1, j2
  real(lrk) :: yi, yf, iy, v1, v2, t1, t2, a1, mn
  real(lrk) :: ibmf, isff, a1rf, b1rf, a2rf, b2rf, f2rf, f1rf
  real(lrk) :: ym, aa, m1, m2, s1, s2, mo
  dimension ym(mts),aa(4),m1(mts),m2(mts),s1(mts),s2(mts),mo(2)
  character(len=3) :: cbd
  character(len=6) :: nmm
  parameter (cbd = 'isf',nmm = 'bendst')
!
  intrinsic :: real
!
!     initialize return
  ok = -1
!
!     loop on sections lengths
  do ii = 1,nn-1
!       initial position
    yi = y(ii)
!       final position
    yf = y(ii+1)
!       mean position
    ym(ii) = (yi+yf)/2
!       inertia times Young
    iy = ie(ii)*es(ii)
!       shear correction factor
    a1 = vkasc(ii)
!       section indices
    j1 = (ii-1)*4
    j2 = ii*4
!       intial node x displacement
    v1 = real(dl(j1+1), lrk)
!       initial node z rotation
    t1 = real(dl(j1+4), lrk)
!       final node x displacement
    v2 = real(dl(j2+1), lrk)
!       final node z rotation
    t2 = real(dl(j2+4), lrk)
!       interpolation polynom coefficients -> aa
    call icoef(yi,yf,v1,v2,t1,t2,aa)
!       x bending moment on half position of element
    m1(ii) = iy*ibmf(aa,ym(ii))
!       x shear force on element (constant)
    s1(ii) = iy/(1+a1)*isff(aa)
!
!       intial node z displacement
    v1 = real(dl(j1+2), lrk)
!       initial node x rotation
    t1 = real(dl(j1+3), lrk)
!       final node x displacement
    v2 = real(dl(j2+2), lrk)
!       final node z rotation
    t2 = real(dl(j2+3), lrk)
    call icoef(yi,yf,v1,v2,t1,t2,aa)
!       z bending moment on half position of element
    m2(ii) = iy*ibmf(aa,ym(ii))
!       z shear force on element (constant)
    s2(ii) = iy/(1+a1)*isff(aa)
!
  end do
!
!     three point spline interpolation/extrapolation
!     nodal coordinates, see tmatfun.f
!     loop on sections
  do ii = 1,nn-3
!       x direction
    v1 = a1rf(ym(ii),ym(ii+1),ym(ii+2),&
    &m1(ii),m1(ii+1),m1(ii+2))
    t1 = b1rf(ym(ii),ym(ii+1),ym(ii+2),&
    &m1(ii),m1(ii+1),m1(ii+2))
!       first point
    if (ii .eq. 1) then
!         extrapolate to first y coordinate
      mi(1,1) = f1rf(v1,t1,y(ii),m1(ii),y(1))
    end if
    mn = f1rf(v1,t1,ym(ii),m1(ii),y(ii+1))
!       check for mean
    if (ii .gt. 1) then
!         mean over left and right interpolation
      mi(ii+1,1) = (mn+mo(1))/2
    else
      mi(ii+1,1) = mn
    end if
    v2 = a2rf(v1,ym(ii),ym(ii+1),ym(ii+2))
    t2 = b2rf(ym(ii),ym(ii+1),ym(ii+2),&
    &m1(ii),m1(ii+1),m1(ii+2))
    mi(ii+2,1) = f2rf(v2,t2,ym(ii+2),m1(ii+2),y(ii+2))
    mo(1) = mi(ii+2,1)
!       last point
    if (ii .eq. nn-3) then
!         extrapolate to last y coordinate
      mi(ii+3,1) = f2rf(v2,t2,ym(ii+2),m1(ii+2),y(ii+3))
    end if
!       z direction
    v1 = a1rf(ym(ii),ym(ii+1),ym(ii+2),&
    &m2(ii),m2(ii+1),m2(ii+2))
    t1 = b1rf(ym(ii),ym(ii+1),ym(ii+2),&
    &m2(ii),m2(ii+1),m2(ii+2))
    if (ii .eq. 1) then
      mi(1,2)= f1rf(v1,t1,ym(ii),m2(ii),y(1))
    end if
    mn = f1rf(v1,t1,ym(ii),m2(ii),y(ii+1))
    if (ii .gt. 1) then
      mi(ii+1,2) = (mn+mo(2))/2
    else
      mi(ii+1,2) = mn
    end if
    v2 = a2rf(v1,ym(ii),ym(ii+1),ym(ii+2))
    t2 = b2rf(ym(ii),ym(ii+1),ym(ii+2),&
    &m2(ii),m2(ii+1),m2(ii+2))
    mi(ii+2,2) = f2rf(v2,t2,ym(ii+2),m2(ii+2),y(ii+2))
    mo(2) = mi(ii+2,2)
    if (ii .eq. nn-3) then
      mi(ii+3,2) = f2rf(v2,t2,ym(ii+2),m2(ii+2),y(ii+3))
    end if
!
  end do
!
!     will reuse m to generate shear
!
!     check smooth value
  if (isf(1) .lt. 1 .or. isf(2) .lt. 1 .or.&
!      have format i2, on saidas. Makes no sense use so big value.
  &isf(1) .gt. 99 .or. isf(2) .gt. 99) then
!       invalid argument
    errmsg = fomsgf(99, nmm,6,cbd,0)
    return
  else if (isf(1) .eq. 1 .or. isf(2) .eq. 1) then
!       no smoothing, just copy s -> m
    do ii = 1,nn-1
      if (isf(1) .eq. 1) m1(ii) = s1(ii)
      if (isf(2) .eq. 1) m2(ii) = s2(ii)
    end do
  else if (isf(1) .gt. 1 .or. isf(2) .gt. 1) then
!       smoothing s -> m, n-1 section lengths
    if (isf(1) .gt. 1) call medfilt1w(nn-1,mts,isf(1),s1,m1)
    if (isf(2) .gt. 1) call medfilt1w(nn-1,mts,isf(2),s2,m2)
  end if
!
!     generates ys sections shear positions -> ys
!     generates shear for ys positions m -> si
!
!     loop over sections
  do ii = 1,nn-1
!       index
    j1 = (ii-1)*2+1
    ys(j1) = y(ii)
    si(j1,1) = m1(ii)
    si(j1,2) = m2(ii)
    ys(j1+1) = y(ii+1)
    si(j1+1,1) = m1(ii)
    si(j1+1,2) = m2(ii)
!
  end do
!
  ok = 0
!
  return
!
end subroutine bendst
!
!     ==================================================================
!>    @brief calculates bending stress on all divisions.
!
!>    @param[in] nt total number of divisions
!>    @param[in] mts dimension of section related vectors
!>    @param[in] ie inertias of sections
!>    @param[in] ds section external diameters.
!>     ds has one extra diameter at index 1.
!>    @param[in] mm interpolated bending moments on section nodes
!>    @param[out] bs bending stress on divisions
!
subroutine bstress(nt,mts,ie,ds,mm,bs)
  use rd_kinds, only: lrk
  implicit none

  integer :: nt, mts
  real(lrk) :: ie, ds, mm, bs
  dimension ie(mts),ds(mts),mm(mts,2),bs(mts)
!
  integer :: kk
  real(lrk) :: mx, mz, bm, sr, ii, prec
  parameter(prec = 1e-18_lrk)
!
  intrinsic :: abs, sqrt
!
!      loop on divisions
  do kk = 1,nt
    bs(kk) = 0
!       total bending moment on division
    mx = mm(kk,1)
    mz = mm(kk,2)
!       bending moment
    bm = sqrt(mx**2+mz**2)
!       division radius
!       ds has one extra diameter at index 1
    sr = ds(kk)/2
    if (kk .gt. 1) then
!         second moment of area circular section
      ii = ie(kk-1)
    else
      ii = ie(1)
    end if
!       bending stress, void zero division
    if (abs(ii) .gt. prec) bs(kk) = bm*sr/ii
!        print *,kk,bm,sr,ii,bs(kk)
  end do
!
  return
!
end subroutine bstress
!
!     ==================================================================
!>    @brief decompose coordinates.
!
!>    @param[in] xin input x value
!>    @param[in] zin input z value
!>    @param[in] tht rotation angle
!>    @param[in] rot rotation coordinate flag
!>    @param[out] xou output x value
!>    @param[out] zou output z value
!
subroutine decomp(xin,zin,tht,rot,xou,zou)
  use rd_kinds, only: wp
  implicit none
!
  real(wp) :: xin, zin, tht, xou, zou
  logical :: rot
!
  intrinsic :: cos, sin
!
  if (.not. rot) then
    xou = xin*cos(tht)
    zou = zin*sin(tht)
  else
    xou = xin*sin(tht)
    zou = zin*cos(tht)
  end if
!
  return
!
end subroutine decomp
!
!     ==================================================================
!>    @brief weight sub-matrix coefficients.
!
!>    @param[in] dy section length
!>    @param[in] rho section density
!>    @param[in] s section area
!>    @param[in] sf scale factor 9.81 * cos(incl)
!>    @param[in] an application angle, 270 = weight
!>    @param[out] m return matrix 8 x 1
!
subroutine coepes(dy,rho,s,sf,an,m)
  use rd_kinds, only: lrk, wp
  implicit none
!
!     arguments
  real(lrk) :: dy, rho, s
  real(wp) :: sf, an, m
  dimension m(8)
!
!     locals
  real(wp) :: ln1, m1, mul, xou, zou
  dimension ln1(8),m1(8)
  parameter (ln1 =(/10._wp,10._wp,1.66666667_wp,-1.66666667_wp,10._wp,10._wp,-1.66666667_wp,1.66666667_wp/))
!
  intrinsic :: real
!
!     copy vector ln1 to m1
  call copv_dd(ln1,m1,8,8,8)
!
!     angle components->xou,zou
  call decomp(ln1(1),ln1(2),an,.false.,xou,zou)
!
!     force x1
  m1(1) = xou
!     force z1
  m1(2) = zou
!
!     rotational moment x1
  call decomp(ln1(3),ln1(4),an,.true.,xou,zou)
  m1(3) = xou*dy
!     rotational moment z1
  m1(4) = zou*dy
!
!     force x2
  call decomp(ln1(5),ln1(6),an,.false.,xou,zou)
  m1(5) = xou
!     force z2
  m1(6) = zou
!
!     rotational moment x2
  call decomp(ln1(7),ln1(8),an,.true.,xou,zou)
  m1(7) = xou*dy
!     rotational moment z2
  m1(8) = zou*dy
!
!     product -> mul
  mul = rho*s*dy/20._lrk*sf
!
!     matrix m1 by scalar mul -> m
  call escv_d(m1,m,mul,8,8)
!
  return
!
!     sub end
!
end subroutine coepes
!
!     ==================================================================
!>    calculates combined coordinate x and z
!
!>    @param[in] dl generalized coordinates displacement vector
!>    @param[in] ii coodinate offset
!>    @param[in] mtg generalized coordinates (dl) vector dimension
!>    @return (signal z)*sqrt(x^2+z^2)
!
real(wp) function cxzf(dl,ii,mtg)
  use rd_kinds, only: wp
  implicit none
!
  integer :: ii, mtg
  real(wp) :: dl
  dimension dl(mtg)
!
!     locals
  integer :: k, ifidxof
  real(wp) :: x, z, dsgnf
!
  intrinsic :: sqrt
!
!     position index
  k = ifidxof(ii,1)
  x = dl(k)
  z = dl(k+1)
!     x - z composing resultant amplitude
  cxzf = dsgnf(z-x)*sqrt((x*x)+(z*z))
!
  return
!
!     function end
!
end function cxzf
!
!     ==================================================================
!>    @brief search for maximum displacement
!
!>    @param[in] y sections coordinates vector, y
!>    @param[in] dl generalized coordinates displacement vector
!>    @param[in] ic generalized coordinate offset > 0 - combined = sqrt(
!>    @param[out] pl return maximum positions, y (m)
!>    @param[out] vl return maximum displacments values (m)
!>    @param[out] cl return coordinate ic
!>    @param[out] cm return the number of maximums found
!>    @param[in] nt number of sections, y
!>    @param[in] mts y vector dimension
!>    @param[in] mtg generalized coordinates (dl) vector dimension
!>    @param[in] mtm maximum positions (pl), maximum displacments (vl)
!>      and coordinate ic (cl) vector dimensions
!
subroutine max_dl(y,dl,ic,pl,vl,cl,cm,nt,mts,mtg,mtm)
  use rd_kinds, only: lrk, wp
  implicit none
!
!     locals
  integer :: ii, jj, k1, k2
  real(wp) :: dy, sg, v1, v2, xx, yy, cf, prec
  dimension xx(3),yy(3),cf(3)
  parameter (prec = 1e-18_lrk)
!
!     function
  integer :: ifidxof
  real(wp) :: cxzf, dsgnf
!
!     arguments
  integer :: ic, cm
  integer :: nt, mts, mtg, mtm
  real(lrk) :: y, pl, vl
  real(wp) :: dl
  integer :: cl
  dimension y(mts),dl(mtg)
  dimension pl(mtm),vl(mtm),cl(mtm)
!
  intrinsic :: abs, real
!
  sg = 0._lrk
!     divisions
  do ii = 1,nt-1
    jj = ii+1
!       get two consecutives points
    if (ic .gt. 0) then
      k1 = ifidxof(ii,ic)
      k2 = ifidxof(jj,ic)
      v1 = dl(k1)
      v2 = dl(k2)
    else
      v1 = cxzf(dl,ii,mtg)
      v2 = cxzf(dl,jj,mtg)
    end if
    dy = (v2-v1)/(y(jj)-y(ii))
!       check derivative signal (max/min search)
    if (ii .gt. 1) then
      if (abs(dy) .gt. prec .and. sg .ne. dsgnf(dy)) then
!           p1
        k1 = ii-1
        xx(1) = real(y(k1), wp)
        if (ic .gt. 0) then
          jj = ifidxof(k1,ic)
          yy(1) = dl(jj)
        else
          yy(1) = cxzf(dl,k1,mtg)
        end if
!           p2
        xx(2) = real(y(ii), wp)
        if (ic .gt. 0) then
          jj = ifidxof(ii,ic)
          yy(2) = dl(jj)
        else
          yy(2) = cxzf(dl,ii,mtg)
        end if
!           p3
        k1 = ii+1
        xx(3) = real(y(k1), wp)
        if (ic .gt. 0) then
          jj = ifidxof(k1,ic)
          yy(3) = dl(jj)
        else
          yy(3) = cxzf(dl,k1,mtg)
        end if
!           quadrature max point
        call qcfd(xx,yy,cf)
!           x value maximum->v1
!           value of maximum->v2
!           coefficients vector->cf
!           cf(1)*x^2+cf(2)*x+cf(3)
        call mxqd(v1,v2,cf)
        cm = cm+1
        pl(cm) = real(v1, lrk)
        vl(cm) = real(v2, lrk)
        cl(cm) = ic
!
      end if
    end if
    sg = dsgnf(dy)
!
  end do
!
  return
!
end subroutine max_dl
!
!     ==================================================================
!>    @brief get matrix with bearing and support total stiffnes.
!>     One line per bearing, bearing and support stiffness. For
!>     variable bearing parameters stiffness at nominal speed times
!>     bearing rks flag. If rks = 0 then bearing set at max stiffness.
!
!>    @param[in] pv bearing speed dependent param flag
!>    @param[in] nbrg number of bearings
!>    @param[in] rks pu of nominal speed. <0 use only support values.
!>    @param[in] mxp rows dimension of rkp matrix.
!>    @param[out] rkp bearing and support stiffness for a given speed
!>    @param[out] errmsg return an error message
!>    @param[out] ok return flag. unsuccessful if <0.
subroutine bespst(pv,nbrg,rks,mxp,rkp,errmsg,ok)
  use rd_textfun, only: fwmsgf
  use com_cpbn, only: spdn
  use com_fixstif, only: fxstf
  use com_knd, only: cknd, supr
  use com_sp1, only: ns, bn
  use com_sp2, only: sup, sps
  use rd_kinds, only: lrk
  implicit none
!
!     arguments
  logical :: pv
  integer :: mxp, nbrg, ok
  real(lrk) :: rks, rkp
!     bearing number, 1:x or 2:z, 1:bearing or 2:support
  dimension rkp(mxp,2,2)
  character(len=99) :: errmsg
!
!     nominal speed (rpm)
!
!     bearing support
  integer :: mxm
  parameter (mxm = 9)
!     number of parameters lines
!     bearing number
!     fixed stiffnes for no displacement
!     sps -> scale
!
!     bearing calculation kind
!
!     consider support
  character(len=1) :: cps
  character(len=6) :: nmm
  character(len=9) :: dsc
  integer :: ii, jj, kk, nbr, oki
!     rkf fixed stiffness same as rkm, see entrada
  real(lrk) :: rpm2radf, rad, rkm, rkf, par
  dimension par(mxm,10)
  parameter (rkf = 5e20_lrk,cps = 'S',nmm = 'bespst',dsc='nom.speed')
!
  ok = -1
!
!     check if nominal speed value is defined
!     when should use speed dependent bearing
!     stiffness value. Return if not.
  if (spdn .le. 0 .and. rks .gt. 0) then
!       invalid speed
    errmsg = fwmsgf(99, nmm,11,dsc,0)
    return
  end if
!
!     check fixed stiffness
!     see entrada
  if (fxstf .le. 0) fxstf = rkf
  rkm = fxstf
!
!     convert nominal speed rpm times bearing speed pu -> rad
  rad = rpm2radf(spdn*rks)
!
  kk = 0
!     bearing loop
  do ii = 1,nbrg
    kk = kk+1
!       default: no flexible support associated with this bearing.
!       A matching support may replace these values below.  Keeping the
!       fallback outside the support loop makes the result independent
!       of SUPPORT row order.
    rkp(kk,1,2) = rkm
    rkp(kk,2,2) = rkm
!       check for consider support
    if (supr .eq. cps) then
!         support loop. Input validation guarantees at most one match.
      do jj = 1,ns
!           check if bearing has support
        if (bn(jj) .eq. ii) then
!             rks = 0 preserves the historical rigid-support behavior.
!             rks <> 0 uses the associated support; for rks < 0 this
!             implements the historical "support only" static option.
          if (rks .ne. 0) then
!               kxx
            rkp(kk,1,2) = sup(jj,1)*sps
!               kzz
            rkp(kk,2,2) = sup(jj,4)*sps
          end if
!             association found: do not let unrelated later rows
!             overwrite the selected support.
          exit
        end if
      end do
    end if
!
!       check stiffness kind by value of rks
    if (rks .le. 0) then
!         no bearing stiffness displacement
      rkp(kk,1,1) = rkm
      rkp(kk,2,1) = rkm
    else if (rks .gt. 0) then
!         calculate bearing parameters at speed rad
      call parman(rad,pv,mxm,nbr,par,errmsg,oki)
      if (oki .ne. 0) return
      rkp(kk,1,1) = par(kk,1)
      rkp(kk,2,1) = par(kk,4)
    end if
!       bearing loop
  end do
!
  ok = 0
!
  return
!
end subroutine bespst
!
!>    @brief calculates generalized displacements.
!>     static elastic line on horizontal (x) and
!>     vertical (z) directions.
!
!>    @param[in] pv variable bearing time response
!>    @param[in] mx first dimension of rkp and rtk matrices (same as mxm
!>    @param[in] is dimension of dl vector (same as mtg)
!>    @param[in] smn number of divisions
!>    @param[in] dm size of matrices
!>    @param[out] isa slope angle given in radians flag
!>    @param[out] sf gravity+slope weight factor
!>    @param[out] im bearing index vector
!>    @param[out] rkp bearing and support stiffness for a given speed
!>    @param[out] rtk equivalent bearing and support stiffnes
!>    @param[out] dl generalized displacements (x,z)
!>    @param[out] errmsg return an error message
!>    @param[out] ok return flag. unsuccessful if <0.
!>    see also tflextor.f
!
subroutine lgndsp(pv,mx,is,smn,dm,&
&isa,sf,im,rkp,rtk,dl,errmsg,ok)
  use rd_textfun, only: cadjf, fomsgf
  use com_conc, only: nmic, psic, vlmc, ixic, iyic, izic
  use com_dis, only: pd, d_d, h_d, rho_d, r2, nd
  use com_doff, only: off_d
  use com_dsc, only: md, idx, idy
  use com_hang, only: hangle, accg, isf, ha
  use com_mbk, only: mkb
  use com_mfa, only: mam => ma, rmd => rm, au
  use com_sea, only: rs, es, gs, dely, s, ie, ump
  use com_seaf, only: dfc, dfa
  use com_sec, only: n, y, nt, ni => nn
  use com_thmfr, only: thfr
  use com_unb1, only: ndd, mu, ed, tpf, nb
  use com_ymc, only: nbrg, rks, pc
  use rd_kinds, only: lrk, wp
  implicit none
!
!     arguments
  logical :: pv, isa
!     note mx =mxm, is = mtg
  integer :: mx, is, smn, dm, im, ok
  real(lrk) :: rkp, rtk
  real(wp) :: sf, dl
  dimension im(mx),rkp(mx,2,2),rtk(mx,2),dl(is)
  character(len=99) :: errmsg
!
!     max global matrices number of elements
  integer :: mtg
  parameter (mtg = 500)
!
!     basic global stiffness matrix mkb
!     without bearing stiffness and UMP
!
!     disks
  integer :: mxd
  parameter (mxd = 99)
!
!      masses and concentrated inertias
  integer :: mxic
  parameter (mxic = 15)
!
!     unbalance
  integer :: mxb
  parameter (mxb = 99)
!
!      -1:transient torque,0:unbalance (default),1:concentrated,
!       2:harmonic torque,3:static torque,4:freq. response,5:dist.force
!     min amplification factor fator, modal rpm, angle unit block
!     angle unit r -> radians, default degree
!     torsion harmonic excitation frequency (rad/s)
!     or force offset for kind = 1 (m)
!
!     all sections (div)
  integer :: mts
  parameter (mts = 999)
!     in other commons ni = nn
!
!     sections additional data
!     distributed force - francisco sep-21
!     distributed force angle - francisco nov-21
!
!     slope and gravity
!     angle unit r -> radians, default degree
!     bending stress median filter width
!
!     bearings
  integer :: mxm
  parameter (mxm = 9)
!
!
!     locals
  logical :: isr
  real(lrk) :: m, dy, frc, gg, ag
!
  integer :: c, ii, jj, kk, ll, nn, r, oki, ipiv, inddis, ifidxof
  real(wp) :: an, vf, mk, sp, su, dd, toraddf
!
  character(len=1) :: c1, crd, crdv
  character(len=3) :: cbd, ci
  character(len=5) :: nmm
!
  dimension sp(8),su(8),isr(2),crdv(2),ipiv(dm),&
  &vf(mtg),mk(mtg,mtg)
!
  parameter (ag = 9.8066_lrk,c1 = 'N',cbd = 'mtg',nmm = 'ldisp',crdv = (/'r','R'/))
!
  intrinsic :: cos, int, real, sin
!
!     initialize return
  ok = -1
!
!     check dimension
  if (dm .gt. mtg) then
!       'out of bounds' !4
    errmsg = fomsgf(50, nmm,4,cbd,0)
    return
  end if
!
!     check for slope angle in radians
  crd = cadjf(1, ha,1)
  isr(1) = crd .eq. crdv(1) .or. crd .eq. crdv(2)
  isa = isr(1)
!     check for force angle in radians
  crd = cadjf(1, au,1)
  isr(2) = crd .eq. crdv(1) .or. crd .eq. crdv(2)
!
!     given gravity accel
  gg = accg
!     show a warning?
  if (gg .le. 0) gg = ag
!
!     z angle projection
!     double precision dd
  dd = real(hangle, wp)
  an = dd
!     if not angle in radians, deg -> rad
  if (.not. isr(1)) an = toraddf(dd)
!     double precision sf
  sf = real(gg, wp)*cos(an)
!
!     bearing and support stiffnes parameters -> rkp
  call bespst(pv,nbrg,rks,mxm,rkp,errmsg,oki)
  if (oki .ne. 0) return
!     initialize force vector -> vf
  call zervec_d(vf,dm,mtg)
!
!     section weight    ==> X
!                   -Z ||
!                      \/
!
!     n=div_menor,div_maior,div_menor
!     rotor divisions
!
  c = 1
  m = n(1)
!     division loop
  do r = 1,smn
!
    if (r .le. m) then
      dy = dely(c)
    else
      c = c+1
      m = m+n(c)
      dy = dely(c)
    end if
!
!       sections weight vector -> sp
!
!       dy - section length
!       rs - density
!       s - area
!       sf - factor 9.81 x cos(incl)
!       weight down force = 270 deg => 3*pi/2
    dd = 3._wp/2._wp*3.14159265358979_wp
!       df - down force angle = 270 deg
!       sp - returns matrix 8 x 1
    call coepes(dy,rs(r),s(r),sf,dd,sp)
!
!       sections dist. force -> su
!       dist. force => N
!
!       dy - section length
!       rs - density
!       frc  -> dfc/rs * sm * dy
!       sf - factor 9.81 x cos(incl) = 1
!       su - returns matrix 8 x 1
    frc = dfc(r)/rs(r)
!       distributed force angle (rad)
    dd = real(dfa(r), wp)
    call coepes(dy,rs(r),frc,1._wp,dd,su)
!
!       global weight force vector section weight + force
!
!       1 5 9 13 17 ...
    kk = ifidxof(r,1)
!       8 12 16 ...
    ll = 4*(r+1)
    do ii = kk,ll
!         sub-matrix indices
      nn = ii-kk+1
!         weight + force matrix
      vf(ii) = vf(ii)+sp(nn)+su(nn)
    end do
!       loop division
  end do
!
!     down force signal
  an = -1._wp
!
!     disk weight      ==> X
!                  -Z ||
!                     \/
  oki = -1
  do ii = 1,nd
!       search disk position
    kk = inddis(pd(ii),errmsg,oki)
    if (oki .gt. 0) then
!         add weight force
!         sf - scales 9.81 x cos(incl)
!         z force
      dd = real(md(ii), wp)*an*sf
      ll = ifidxof(kk,2)
      vf(ll) = vf(ll)+dd
!         overhung x moment
      vf(ll+1) = vf(ll+1)+dd*real(off_d(ii), wp)
    else
!         could not find
      return
    end if
!       end disk loop
  end do
!
!     concentrated mass      ==> X
!                        -Z ||
!                           \/
  oki = -1
  do ii = 1,nmic
!       search concentrated position
    kk = inddis(psic(ii),errmsg,oki)
    if (oki .gt. 0) then
!         add weight force
!         sf - scale 9.81 x cos(incl)
!         z force
      dd = real(vlmc(ii), wp)*an*sf
      ll = ifidxof(kk,2)
      vf(ll) = vf(ll)+dd
    else
!         could not find
      return
    end if
!       end loop concentrated
  end do
!
!     forces       ==> X
!              -Z ||
!                 \/
  oki = -1
  do ii=1,nb
!       check force kind 1=concentrated
    if (tpf(ii) .eq. 1) then
!         force position
      kk = inddis(ndd(ii),errmsg,oki)
      if (oki .gt. 0) then
!           add force
        ll = ifidxof(kk,1)
!           force angle
        dd = real(ed(ii), wp)
        an = dd
!           check unit, deg -> rad
        if (.not. isr(2)) an = toraddf(dd)
!           x force
        dd = real(mu(ii), wp)*cos(an)
        vf(ll) = vf(ll)+dd
!           moment z
        vf(ll+3) = vf(ll+3)-dd*real(thfr(ii), wp)
!           z force
        dd = real(mu(ii), wp)*sin(an)
        vf(ll+1) = vf(ll+1)+dd
!           moment x
        vf(ll+2) = vf(ll+2)+dd*real(thfr(ii), wp)
      else
!           could not find
        return
      end if
    end if
!       end loop force
  end do
!
!     bearing fixed stiffness matrix km
!
!     copy matrix mkb -> mk
  call copmat2_rd(mkb,mk,dm,dm,mtg,mtg,mtg,mtg)
!
!     todo check
!
  jj = 0
  do ii = 1,nbrg
    jj = jj+1
    kk = inddis(pc(jj),errmsg,oki)
    if (oki .gt. 0) then
!         adds stiffness
!         x
      ll = ifidxof(kk,1)
!         bearing index
      im(jj) = ll
      !        x = bearing + support
      rtk(jj,1) = 1/(1/rkp(jj,1,1)+1/rkp(jj,1,2))
      mk(ll,ll) = mkb(ll,ll)+rtk(jj,1)
!         z
      rtk(jj,2) = 1/(1/rkp(jj,2,1)+1/rkp(jj,2,2))
      mk(ll+1,ll+1) = mkb(ll+1,ll+1)+rtk(jj,2)
!         gx
      mk(ll+2,ll+2) = mkb(ll+2,ll+2)+1
!         gz
      mk(ll+3,ll+3) = mkb(ll+3,ll+3)+1
!
    else
!         could not find
      return
    end if
!       end loop bearings
  end do
!
!     lapack solver -> mk
  call dgetrf(dm,dm,mk,mtg,ipiv,ii)
  if(ii .ne. 0) then
    write(ci,5) ii
    errmsg = fomsgf(50, nmm,28,ci,0)
    return
  end if
!
!     copy force vector to solution vf->dl
  call copv_dd(vf,dl,dm,mtg,mtg)
!
!     lapack system solver -> dl
!     displacement calculation {x} = [k]^-1 * {f}
  call dgetrs(c1,dm,1,mk,mtg,ipiv,dl,dm,ii)
  if(ii .ne. 0) then
    write(ci,5) ii
    errmsg = fomsgf(50, nmm,32,ci,0)
    return
  end if
!
  ok = 0
!
  return
!
5 format(i3)
!
end subroutine lgndsp
!
!     ==================================================================
!     @brief calculates static elastic line
!
!>    @param[in] pv variable bearing time response
!>    @param[in] std standard input output
!>    @param[in] plt generate plot output
!>    @param[out] errmsg return an error message
!>    @param[out] ok return flag. unsuccessful if <0.
!
subroutine linhael(pv,std,plt,errmsg,ok)
  use com_cpbn, only: spdn
  use com_hang, only: hangle, accg, isf, ha
  use com_sea, only: rs, es, gs, dely, s, ie, ump
  use com_sec, only: n, y, nt, ni => nn
  use com_sed, only: ds, di_d, di_s
  use com_ymc, only: nbrg, rks, pc
  use rd_kinds, only: lrk, wp
  implicit none
!
!     arguments
  integer :: ok
  character(len=99) :: errmsg
  logical :: pv, std, plt
!
!     all sections (div)
  integer :: mts
  parameter (mts = 999)
!     in other commons ni = nn
!
!     sections additional data
!
!     section diameters
!
!     bearings
  integer :: mxm, mxr, mtg
  parameter (mxm = 9,mxr = 4*mxm,mtg = 500)
!
!
!     slope and gravity
!     angle unit r -> radians, default degree
!     bending stress median filter width
!
!     nominal speed (rpm)
!
!     locals
  logical :: isr
  character(len=3) :: cbd
  character(len=7) :: nmm
  real(lrk) :: tm, rm, vl, pl, rkp, rtk, mi, ys, si, bs, sumv_r
  integer :: cm, ii, nn, oo, oki, im, cl, ifidxof, rotating_ndof_f
  real(wp) :: sf, dl
!     bearing index max coord (z manda)
  dimension rm(mxr),vl(mxm),pl(mxm),im(mxm),cl(mxm),&
  &rtk(mxm,2),rkp(mxm,2,2),mi(mts,2),ys(mts),si(mts,2),dl(mtg),&
  &bs(mts)
  parameter (tm = 1,cbd = 'mtg',nmm = 'linhael')
!
  intrinsic :: nint, real
!
!     initialize return
  ok = -1
  call progress_begin('STATIC_ELASTIC_LINE',4,&
  &'solve static deflection, reactions and stress')
  call progress_stage('STATIC_ELASTIC_LINE',&
  &'solve generalized static displacements')
!     sum all divisions
  ii = nint(sumv_r(n,mts,nt))
!     static solve dimension: shaft plus flexible-disk rotational DOFs.
!     Support/foundation are still handled by the historical equivalent
!     static-support path, but flexible-disk springs must be condensed b
!     solving their added coordinates instead of truncating mkb.
  oo = rotating_ndof_f()
  if (oo .le. 0) oo = 4*(ii+1)
!     calculate generalized displacements -> isr,sf,im,rkp,rtk,dl
  call lgndsp(pv,mxm,mtg,ii,oo,&
  &isr,sf,im,rkp,rtk,dl,errmsg,oki)
  if (oki .ne. 0) then
    call progress_end('STATIC_ELASTIC_LINE','FAILED')
    return
  end if
  call progress_update('STATIC_ELASTIC_LINE',1,4,real(oo, lrk),'DOF')
  call progress_stage('STATIC_ELASTIC_LINE',&
  &'calculate reactions and maximum displacement')
!     isr slope angle given in radians flag
!     sf gravity+slope weight factor
!     im bearing index vector
!     rkp bearing and support stiffness for a given speed
!     rtk equivalent bearing and support stiffnes
!     dl generalized displacements (x,z)
!
!                              /\
!     calculates z reaction +Z ||
!     bearings                  ==> X
!
  do ii = 1,nbrg
    nn = ifidxof(ii,1)
    oo = im(ii)
    rm(nn) = real(-dl(oo)*rtk(ii,1), lrk)
    rm(nn+1) = real(-dl(oo+1)*rtk(ii,2), lrk)
    rm(nn+2) = real(-dl(oo+2)*tm, lrk)
    rm(nn+3) = real(-dl(oo+3)*tm, lrk)
  end do
!
!     maximum displacements
!
!     y -  y coordinates array
!     dl - general displacment coordinates vector
!     pl - return maximum positions (m, y)
!     ic - coordinates index 1 x, 2z, 3gx, 4gz
!     vl - returns maximum displacement value (m)
!     cl - returns coordinate vetcor
!     nt - total number of sections
!     cm - returns number of found maximums
!     mts - y vector dimension
!     mtg - dl dimension
!     mxm - dimension pm e vm
!
!     init max counter
  cm = 0
!     x
  call max_dl(y,dl,1,pl,vl,cl,cm,nt,mts,mtg,mxm)
!     z
  call max_dl(y,dl,2,pl,vl,cl,cm,nt,mts,mtg,mxm)
!     composed
  call max_dl(y,dl,0,pl,vl,cl,cm,nt,mts,mtg,mxm)
  call progress_update('STATIC_ELASTIC_LINE',2,4,real(cm, lrk),'MAXIMA')
  call progress_stage('STATIC_ELASTIC_LINE',&
  &'calculate bending, shear and stress')
!
!     bending -> mi,ys,si
  call bendst(nt,mts,mtg,isf,y,es,ie,dl,mi,ys,si,errmsg,oki)
  if (oki .ne. 0) then
    call progress_end('STATIC_ELASTIC_LINE','FAILED')
    return
  end if
!     mi bending moment on section y coordinates, x and z
!     ys section coordinates for shear force constant on element
!     si shear force on ys section coordinates, x and z
!
!     bending stress on all sections -> bs
  call bstress(nt,mts,ie,ds,mi,bs)
  call progress_update('STATIC_ELASTIC_LINE',3,4,real(nt, lrk),'SECTIONS')
  call progress_stage('STATIC_ELASTIC_LINE',&
  &'write elastic-line output')
!
!     y section positions (m)
!     dl general displacement vector x,z,fi,theta
!     rm bearing reactions vector
!     pl maximum positions (m, y)
!     vl maximum displacement value (m)
!     cl coordinate value
!     bs bending stress on all sections
!     mi bending moment on section coordinates y (Nm)
!     ys shear force on section coordinates (m)
!     si constant shear force on ys section coordinates (m)
!     rkp bearing and support stiffness xx e zz
!     rks pu of nominal speed. <0 use only support values.
!     spdn rated speed (rpm)
!     nt total number of sections
!     nbrg number of bearings
!     cm number of maximums
!     mts y dimension
!     mtg dl dimension
!     mxr rm dimension
!     mxm vm and pm dimension
!     std standard input output
!     plt plot flag
!     sf gravity+slope weight factor
!     isr slope given in radians flag
!     errmsg error menssage
!     ok return flag
!

  call s_lin_el(y,dl,rm,pl,vl,cl,bs,&
  &mi,ys,si,&
  &rkp,rks,spdn,nt,nbrg,&
  &cm,mts,mtg,mxr,mxm,std,plt,sf,isr,errmsg,ok)
  if (ok .lt. 0) then
    call progress_end('STATIC_ELASTIC_LINE','FAILED')
    return
  end if
  call progress_update('STATIC_ELASTIC_LINE',4,4,real(nt, lrk),'SECTIONS')
  call progress_end('STATIC_ELASTIC_LINE','OK')
!
  return
!
end subroutine linhael
!
