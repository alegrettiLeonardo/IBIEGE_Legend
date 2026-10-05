!     $Id: tmatfun.f 6451 2020-01-23 18:50:16Z francisco $
!     ==================================================================
!
!>    @file tmatfun.f
!>    @author francisco
!>    @date 27-jan-20
!>    @brief torsion math routines, last changes:<br>
!>    new file - 27-jan-20<br>
!>    added complex argument (angle) rargf - apr-20 - f<br>
!>    added real version of three point spline - francisco - dec-20<br>
!>    moved splitf from tentrada - franciso - apr-21<br>
!>    moved sjoinrf from tsaidas - franciso - apr-21<br>
!>    moved sjoinif from tsaidas - franciso - may-21.
!
!     ==================================================================
!>    @brief get maximum value real vector
!
!>    @param[in] in input vector
!>    @param[in] nin nin number of valid elements
!>    @param[in] ndm ndm input vector dimension
!>    @param[in] ine inicial index, defaults to 1 if <=0
!>    @return maximum value real vector
!
real(lrk) function mxrof(in,nin,ndm,ine)
  use rd_kinds, only: lrk
  implicit none
!
  integer :: nin, ndm, ine
  real(lrk) :: in
  dimension in(ndm)
!
  integer :: i, j, nl, inl
  real(lrk) :: mx
  character(len=5) :: nmm
  parameter(nmm = 'mxrof')
!
  intrinsic :: int, max, mod
!
!     check start
  inl = ine
  if (inl .le. 0) inl = 1
!     out of bounds error, stop
  if (inl+nin-1 .gt. ndm) call elmsge(1,5,nmm)
!
  nl = int(nin/2)
  mx = in(inl)
  do i = 1,nl
    j = inl+i*2-1
    mx = max(in(j-1),in(j),mx)
  end do
!     last if needed
  if (mod(nin,2) .gt. 0) then
    j = inl+nin-1
    mx = max(in(j),mx)
  end if
  mxrof = mx
!
  return
!
end function mxrof
!
!     ==================================================================
!>    @brief get maximum value real vector
!>     convenience function starting at first element.
!>    @see mxrof
!
!>    @param[in] in input vector
!>    @param[in] nin nin number of valid elements
!>    @param[in] ndm ndm input vector dimension
!>    @return maximum value real vector
!
real(lrk) function mxrf(in,nin,ndm)
  use rd_kinds, only: lrk
  implicit none
!
  integer :: nin, ndm
  real(lrk) :: in
  dimension in(ndm)
!
  real(lrk) :: mxrof
!
  mxrf = mxrof(in,nin,ndm,1)
!
  return
!
end function mxrf
!
!     ==================================================================
!>    @brief get minimum value real vector
!
!>    @param[in] in input vector
!>    @param[in] nin nin number of valid elements
!>    @param[in] ndm ndm input vector dimension
!>    @param[in] ine inicial index, defaults to 1 if <=0
!>    @return minimum value real vector
!
real(lrk) function mnrof(in,nin,ndm,ine)
  use rd_kinds, only: lrk
  implicit none
!
  integer :: nin, ndm, ine
  real(lrk) :: in
  dimension in(ndm)
!
  integer :: i, j, nl
  real(lrk) :: mn
  character(len=5) :: nmm
  parameter(nmm = 'mnrof')
!
  intrinsic :: int, min, mod
!
!     check start
  if (ine .le. 0) ine = 1
!     out of bounds error, stop
  if (ine+nin-1 .gt. ndm) call elmsge(1,5,nmm)
!
  nl = int(nin/2)
  mn = in(ine)
  do i = 1,nl
    j = ine+i*2-1
    mn = min(in(j-1),in(j),mn)
  end do
!     last if needed
  if (mod(nin,2) .gt. 0) then
    j = ine+nin-1
    mn = min(in(j),mn)
  end if
  mnrof = mn
!
  return
!
end function mnrof
!
!     ==================================================================
!>    @brief get minimum value real vector.
!>     convenience function starting at first element.
!>    @see mnrof
!
!>    @param[in] in input vector
!>    @param[in] nin nin number of valid elements
!>    @param[in] ndm ndm input vector dimension
!>    @return minimum value real vector
!
real(lrk) function mnrf(in,nin,ndm)
  use rd_kinds, only: lrk
  implicit none
!
  integer :: nin, ndm
  real(lrk) :: in
  dimension in(ndm)
!
  integer :: i
  real(lrk) :: mnrof
!
  i = 1
  mnrf = mnrof(in,nin,ndm,i)
!
  return
!
end function mnrf
!
!     ==================================================================
!>    @brief join real input vector.
!>    will join given input vector on a single text, separated by comma.
!>    will remove "at right" zeros if present. will truncate if not
!>    enough size. return blank if error on format.
!
!>    @param[in] rin real type input vector
!>    @param[in] sfm format to apply
!>    @param[in] nin number of input elements
!>    @param[in] ndm dimension of input vector
!>    @return input vector elements at one text spearetd by comma.
!
!
!     ==================================================================
!>    @brief join integer input vector.
!>    will join given input vector on a single text, separated by comma.
!
!>    @param[in] iin input vector
!>    @param[in] sfm format to apply
!>    @param[in] nin number of input elements
!>    @param[in] ndm dimension of input vector
!>    @return input vector elements at one text spearetd by comma.
!
!
!     ==================================================================
!>    @brief get value by spliting an input text using a separator
!>     one character long.
!>    @param[in] cin input text
!>    @param[in] csep one character long separator
!>    @param[in] icodm output vector dimension
!>    @param[out] coutv output separated character vector
!>    @return values array
!
integer function splitf(cin,csep,icodm,coutv)
  implicit none
!
  integer :: icodm
  character :: csep
  character(len=*) :: cin, coutv
  dimension coutv(icodm)
!
  integer :: j, k, l, m, n, o
!
  character(len=6) :: nmm
!     internal loop limit
  parameter (o = 1000,nmm = 'splitf')
!
  intrinsic :: index, len, len_trim
!
!     check if passed dimension is bigger than internal limit
  if (icodm .gt. o) call elmsge(1,75,nmm)
  m = len(coutv(1))
  l = 0
  k = 1
!     search for separator
  j = index(cin,csep)
  do while (j .gt. 0)
    l = l+1
    if (l .gt. o) exit
    if (l .gt. icodm) then
!         out of bounds
      call elmsge(1,5,nmm)
    end if
    n = j-2
    if (m .ge. n) then
      coutv(l) = cin(k:k+j-2)
    else
!         truncation warning
      call elmsgw(0,2,nmm)
      coutv(l) = cin(k:k+m)
    end if
    k = k+j
    j = index(cin(k:),csep,.false.)
  end do
  if (l .gt. o) then
!       out of internal max
    call elmsge(1,5,nmm)
  end if
!     last or single value
  j = len(cin)
  if (l .ge. 0 .and. k .le. j) then
!       check for non blank data
    if (len_trim(cin(k:)) .gt. 0) then
!         some data at last
      l = l+1
      n = j-k
      if (m .ge. n) then
        coutv(l) = cin(k:)
      else
        coutv(l) = cin(k:k+m)
      end if
    end if
  end if
!     number of elements found
  splitf = l
!
  return
!
end function splitf
!
!     ==================================================================
!>    @brief unique values in a vector using precision comparison.
!>     returns number of unique values, vector of number of each unique
!>     value , matrix of position indices of each unique value,ordered
!>     as found. Example:in={1,1,2,2,3,1},nin=6,ndm=6.
!>     nui=3,ni={3,2,1},ui={{1,2,6},{3,4},{5}}.
!
!>    @param[in] in input values vector
!>    @param[out] ui unique values indices
!>    @param[out] ni number of occurrences for each unique number
!>    @param[out] nui number of unique values
!>    @param[in] nin number of values in input vector
!>    @param[in] ndm dimension input and output vectors and matrix
!>    @param[in] eprec equals precision, 0=defaults to 1e-12
!
subroutine unique(in,ui,ni,nui,nin,ndm,eprec)
  use rd_kinds, only: lrk
  implicit none
!
  integer :: ui, ni, nui, nin, ndm
  real(lrk) :: in, eprec
  dimension in(ndm),ui(ndm,ndm),ni(ndm)
!
  integer :: ii, jj
  real(lrk) :: vl, vc, prec, riff
!
  logical :: eql, rpeqf
!
!     precision
  prec = riff(eprec .le. 0,1e-12_lrk,eprec)
!
!     first is unique value
  nui = 1
  eql = .false.
  ni(nui) = 1
  ui(nui,ni(nui)) = 1
!
!     input loop
  do ii = 2,nin
    vl = in(ii)
!       found loop
    do jj = 1,nui
!         found unique value
      vc = in(ui(jj,1))
!         check equals within precision
      eql = rpeqf(vl,vc,prec)
      if (eql) then
!           repeating value found
        ni(jj) = ni(jj)+1
!           keep index
        ui(jj,ni(jj)) = ii
        exit
      end if
    end do
    if (.not. eql) then
!         add new unique value
      nui = nui+1
      ni(nui) = 1
      ui(nui,ni(nui)) = ii
    end if
  end do
!
  return
!
end subroutine unique
!
!=======================================================================
!>    @brief yet another two point linear interpolation
!
!>    @param[in] x1 first x point
!>    @param[in] y1 first y point
!>    @param[in] x2 second x point
!>    @param[in] y2 second y point
!>    @param[in] x desired x point
!
!>    @return y interpolated for given x or zero
!
real(wp) function lin2pf(x1,y1,x2,y2,x)
  use rd_kinds, only: lrk, wp
  implicit none
!
  real(wp) :: x1, y1, x2, y2, x
!
  character(len=6) :: nmm
  real(wp) :: eps, dx, dy, lin2p
!     precision, not exact zero
  parameter (nmm = 'lin2pf',eps = 1E-12_lrk)
!
  lin2p = 0
  dx = x2-x1
!     check delta x
  if (dx .gt. -eps .and. dx .lt. eps) then
!       delta x too small, stop
    call elmsge(1,8,nmm)
  else
    dy = y2-y1
    lin2p = y1+(x-x1)*dy/dx
  end if
!
  lin2pf = lin2p
!
  return
!
end function lin2pf
!
!=======================================================================
!
!>    \verbatim
!>    Three point cubic spline interpolation implementation.
!>    \endverbatim
!>    follow this link:
!>    <a href="https://www.desmos.com/calculator/yvgfpq2prf">yvgfpq2prf<
!>    \verbatim
!>    Coefficients a and b are defined by a system of linear equations,
!>    determined by the coordinates of the points and the requirement
!>    that first and second derivatives match at (x3,y3).
!>    The condition that second derivatives match at (x2,y2) establishes
!>    the relationship between a1 and a2. Remaining coefficients can be
!>    solved by a system of linear equations;
!>    see computations at http://wolfr.am/1fkEvSL
!>    \endverbatim
!
!     ==================================================================
!>    @brief calculate spline a1 function parameter.
!
!>    @param[in] x1 first point x coordinate
!>    @param[in] x2 second point x coordinate
!>    @param[in] x3 third point x coordinate
!>    @param[in] y1 first point y coordinate
!>    @param[in] y2 second point y coordinate
!>    @param[in] y3 third point y coordinate
!>    @return spline a1 function parameter
!
real(wp) function a1f(x1,x2,x3,y1,y2,y3)
  use rd_kinds, only: wp
  implicit none
!
  real(wp) :: x1, x2, x3, y1, y2, y3
!
  real(wp) :: dy23, dy31, dy12, dx12, dx13, dx23, num, den
!
  dy23 = y2-y3
  dy31 = y3-y1
  dy12 = y1-y2
  dx12 = x1-x2
  dx13 = x1-x3
  dx23 = x2-x3
  num = x1*dy23+x2*dy31+x3*dy12
  den = 2*dx12**2*dx13*dx23
  a1f = num/den
!
  return
!
end function a1f
!
!     ==================================================================
!>    @brief calculate real spline a1 function parameter.
!>     convenience real type.
!
!>    @param[in] x1 first point x coordinate
!>    @param[in] x2 second point x coordinate
!>    @param[in] x3 third point x coordinate
!>    @param[in] y1 first point y coordinate
!>    @param[in] y2 second point y coordinate
!>    @param[in] y3 third point y coordinate
!>    @return spline a1 function parameter
!
real(lrk) function a1rf(x1,x2,x3,y1,y2,y3)
  use rd_kinds, only: lrk, wp
  implicit none
!
  real(lrk) :: x1, x2, x3, y1, y2, y3
!
  real(wp) :: a1f, a1d
!
  intrinsic :: real
!
  a1d = a1f(real(x1, wp),real(x2, wp),real(x3, wp),real(y1, wp),real(y2, wp),real(y3, wp))
!
  a1rf = real(a1d, lrk)
!
  return
!
end function a1rf
!
!     ==================================================================
!>    @brief calculate spline b1 function parameter.
!
!>    @param[in] x1 first point x coordinate
!>    @param[in] x2 second point x coordinate
!>    @param[in] x3 third point x coordinate
!>    @param[in] y1 first point y coordinate
!>    @param[in] y2 second point y coordinate
!>    @param[in] y3 third point y coordinate
!>    @return spline b1 function parameter
!
real(wp) function b1f(x1,x2,x3,y1,y2,y3)
  use rd_kinds, only: wp
  implicit none
!
  real(wp) :: x1, x2, x3, y1, y2, y3
!
  real(wp) :: dy32, dy31, dy21, dy12, dx12, dx13, dx23, num1, num2, den
!
  dy32 = y3-y2
  dy31 = y3-y1
  dy21 = y2-y1
  dy12 = y1-y2
  dx12 = x1-x2
  dx13 = x1-x3
  dx23 = x2-x3
  num1 = x1**2*dy32-x1*(x2*(-3*y1+y2+2*y3)+3*x3*dy12)
  num2 = x2**2*dy31+x2*x3*dy21+2*x3**2*dy12
  den = 2*dx12*dx13*dx23
  b1f = (num1+num2)/den
!
  return
!
end function b1f
!
!     ==================================================================
!>    @brief calculate real spline b1 function parameter.
!>     convenience real type.
!
!>    @param[in] x1 first point x coordinate
!>    @param[in] x2 second point x coordinate
!>    @param[in] x3 third point x coordinate
!>    @param[in] y1 first point y coordinate
!>    @param[in] y2 second point y coordinate
!>    @param[in] y3 third point y coordinate
!>    @return spline b1 function parameter
!
real(lrk) function b1rf(x1,x2,x3,y1,y2,y3)
  use rd_kinds, only: lrk, wp
  implicit none
!
  real(lrk) :: x1, x2, x3, y1, y2, y3
!
  real(wp) :: b1f, b1d
!
  intrinsic :: real
!
  b1d = b1f(real(x1, wp),real(x2, wp),real(x3, wp),real(y1, wp),real(y2, wp),real(y3, wp))
!
  b1rf = real(b1d, lrk)
!
  return
!
end function b1rf
!
!     ==================================================================
!>    @brief calculate spline a2 function parameter.
!
!>    @param[in] a1 spline a1 function parameter
!>    @param[in] x1 first point x coordinate
!>    @param[in] x2 second point x coordinate
!>    @param[in] x3 third point x coordinate
!>    @return spline a2 function parameter
!
real(wp) function a2f(a1,x1,x2,x3)
  use rd_kinds, only: wp
  implicit none
!
  real(wp) :: a1, x1, x2, x3
!
  real(wp) :: dx21, dx23
!
  dx21 = x2-x1
  dx23 = x2-x3
  a2f = dx21/dx23*a1
!
  return
!
end function a2f
!
!     ==================================================================
!>    @brief calculate real spline a2 function parameter.
!>     convenience real type.
!
!>    @param[in] a1 spline a1 function parameter
!>    @param[in] x1 first point x coordinate
!>    @param[in] x2 second point x coordinate
!>    @param[in] x3 third point x coordinate
!>    @return spline a2 function parameter
!
real(lrk) function a2rf(a1,x1,x2,x3)
  use rd_kinds, only: lrk, wp
  implicit none
!
  real(lrk) :: a1, x1, x2, x3
!
  real(wp) :: a2f, a2d
!
  intrinsic :: real
!
  a2d = a2f(real(a1, wp),real(x1, wp),real(x2, wp),real(x3, wp))
  a2rf = real(a2d, lrk)
!
  return
!
end function a2rf
!
!     ==================================================================
!>    @brief calculate spline b2 function parameter.
!
!>    @param[in] x1 first point x coordinate
!>    @param[in] x2 second point x coordinate
!>    @param[in] x3 third point x coordinate
!>    @param[in] y1 first point y coordinate
!>    @param[in] y2 second point y coordinate
!>    @param[in] y3 third point y coordinate
!>    @return spline b2 function parameter
!
real(wp) function b2f(x1,x2,x3,y1,y2,y3)
  use rd_kinds, only: wp
  implicit none
!
  real(wp) :: x1, x2, x3, y1, y2, y3
!
  real(wp) :: dy23, dy32, dy21, dy31, dx12, dx13, dx23, num1, num2, den
!
  dy23 = y2-y3
  dy32 = y3-y2
  dy21 = y2-y1
  dy31 = y3-y1
  dx12 = x1-x2
  dx13 = x1-x3
  dx23 = x2-x3
  num1 = 2*x1**2*dy23+x2*(x1*dy32+x3*(2*y1+y2-3*y3))
  num2 = 3*x1*x3*dy32+x2**2*dy31+x3**2*dy21
  den = 2*dx12*dx13*dx23
  b2f = (num1+num2)/den
!
  return
!
end function b2f
!
!     ==================================================================
!>    @brief calculate real spline b2 function parameter.
!>     convenience real type.
!
!>    @param[in] x1 first point x coordinate
!>    @param[in] x2 second point x coordinate
!>    @param[in] x3 third point x coordinate
!>    @param[in] y1 first point y coordinate
!>    @param[in] y2 second point y coordinate
!>    @param[in] y3 third point y coordinate
!>    @return spline b2 function parameter
!
real(lrk) function b2rf(x1,x2,x3,y1,y2,y3)
  use rd_kinds, only: lrk, wp
  implicit none
!
  real(lrk) :: x1, x2, x3, y1, y2, y3
  real(wp) :: b2f, b2d
!
  intrinsic :: real

  b2d = b2f(real(x1, wp),real(x2, wp),real(x3, wp),real(y1, wp),real(y2, wp),real(y3, wp))
  b2rf = real(b2d, lrk)
!
  return
!
end function b2rf
!
!     ==================================================================
!>    @brief calculate spline interpolation x between first and second p
!
!>    @param[in] a1 spline a1 function parameter.
!>    @param[in] b1 spline b1 function parameter.
!>    @param[in] x1 first point x coordinate
!>    @param[in] y1 first point y coordinate
!>    @param[in] x point to interpolate
!>    @return spline interpolated value for x point between first and se
!
real(wp) function f1f(a1,b1,x1,y1,x)
  use rd_kinds, only: wp
  implicit none
!
  real(wp) :: a1, b1, x1, y1, x
!
  real(wp) :: dx1
!
  dx1 = x-x1
  f1f = a1*dx1**3+b1*dx1+y1
!
  return
!
end function f1f
!
!     ==================================================================
!>    @brief calculate real spline interpolation x between first and sec
!>     convenience real type.
!
!>    @param[in] a1 spline a1 function parameter.
!>    @param[in] b1 spline b1 function parameter.
!>    @param[in] x1 first point x coordinate
!>    @param[in] y1 first point y coordinate
!>    @param[in] x point to interpolate
!>    @return spline interpolated value for x point between first and se
!
real(lrk) function f1rf(a1,b1,x1,y1,x)
  use rd_kinds, only: lrk, wp
  implicit none
!
  real(lrk) :: a1, b1, x1, y1, x
  real(wp) :: f1f, f1d
!
  intrinsic :: real
!
  f1d = f1f(real(a1, wp),real(b1, wp),real(x1, wp),real(y1, wp),real(x, wp))
  f1rf = real(f1d, lrk)
!
  return
!
end function f1rf
!
!     ==================================================================
!>    @brief calculate spline interpolation x between second and third p
!
!>    @param[in] a2 spline a2 function parameter.
!>    @param[in] b2 spline b2 function parameter.
!>    @param[in] x3 third point x coordinate
!>    @param[in] y3 third point y coordinate
!>    @param[in] x point to interpolate
!>    @return spline interpolated value for x point between second and t
!
real(wp) function f2f(a2,b2,x3,y3,x)
  use rd_kinds, only: wp
  implicit none
!
  real(wp) :: a2, b2, x3, y3, x
!
  real(wp) :: dx3
!
  dx3 = x-x3
  f2f = a2*dx3**3+b2*dx3+y3
!
  return
!
end function f2f
!
!     ==================================================================
!>    @brief calculate real spline interpolation x between second and th
!>     convenience real type.
!
!>    @param[in] a2 spline a2 function parameter.
!>    @param[in] b2 spline b2 function parameter.
!>    @param[in] x3 third point x coordinate
!>    @param[in] y3 third point y coordinate
!>    @param[in] x point to interpolate
!>    @return spline interpolated value for x point between second and t
!
real(lrk) function f2rf(a2,b2,x3,y3,x)
  use rd_kinds, only: lrk, wp
  implicit none
!
  real(lrk) :: a2, b2, x3, y3, x
!
  intrinsic :: real
!
  real(wp) :: f2f, f2d
!
  f2d = f2f(real(a2, wp),real(b2, wp),real(x3, wp),real(y3, wp),real(x, wp))
  f2rf = real(f2d, lrk)
!
  return
!
end function f2rf
!
!     ==================================================================
!>    @brief standard deviation and mean
!
!>    @param[in] vin input data vector
!>    @param[in] nvin number of data to consider on input vector
!>    @param[in] mxd dimension of input vector
!>    @param[out] mean mean of input data vector
!>    @param[out] var variation of input data vector
!>    @return standard deviation of input data vector
!
real(lrk) function stddvf(vin,nvin,mxd,mean,var)
  use rd_kinds, only: lrk
  implicit none
!
  integer :: nvin, mxd
  real(lrk) :: vin, mean, var
  dimension vin(mxd)
!
  integer :: ii, nel
  real(lrk) :: ex, ex2, ri, m, v, s, stddv
!
  intrinsic :: abs, min, real, sign, sqrt
!
  m = 0
  ex = 0
  ex2 = 0
  stddv = 0
  nel = min(nvin,mxd)
!
  do ii = 1,nel
    ex = vin(ii)+ex
    ex2 = vin(ii)**2+ex2
    ri = real(ii, lrk)
    m = ex/ri
    v = ex2/ri-(ex/ri)**2
    s = sign(1.0_lrk,v)
    stddv = sqrt(abs(v))*s
  end do
!     return
  mean = m
  var = v
  stddvf = stddv
!
  return
!
end function stddvf
!
