!     $Id$
!     ==================================================================
!
!>    @file rdfra.f
!>    @brief unbalance response analysis, last changes:<br>
!>    new analise resposta desbalaceamento - francisco - 03/12/2008<br>
!>    changed maxamp, ld=0 dentro loop pontos - francisco - 26/02/2010<b
!>    changed getbds, verificacao de limite - francisco - 16/06/2011<br>
!>    add st em getbds, fatamo e maxamp - francisco - 16/06/2011<br>
!>    removed ma argument and af check on maxamp - francisco - oct-15<br
!>    added ok, errmsg arguments on maxamp - francisco - oct-15<br>
!>    added central messages - francisco - francisco - apr-19<br>
!>    updated quad, call qcfr from matfun - franciso - mar-21<br>
!>    removed mxint, updated call to mxqr on matfun - francisco - mar-21
!>    added precision on maxamp, changed name to fatamof - francisco - o
!
!     ==================================================================
!>    @brief linear interpolation for amplitude and speed.
!
!>    @param[in] ix speed and aplitude x position index
!>    @param[in] iy speed and aplitude y position index
!>    @param[in] ip number of the desired position
!>    @param[in] y y value to get the interpolated x value
!>    @param[in] rpg speed vector rpm
!>    @param[in] amp complex amplitude matrix
!>    @param[in] mxp position, first dimension of amplitude matrix
!>    @param[in] mte speed, second dimension of amplitude matrix
!>    @return linear interpolated values
!
real(lrk) function linintx(ix,iy,ip,y,rpg,amp,mxp,mte)
  use rd_textfun, only: fwmsgf
  use rd_kinds, only: lrk, wp
  implicit none
!
!     arguments
  integer :: ix, iy, ip, mxp, mte
  real(lrk) :: y
  real(lrk) :: rpg(mte)
  complex(wp) :: amp(mxp,mte)
!
!     locals
  real(lrk) :: dx, dy, a, b, mn
  character(len=1) :: blank
  character(len=7) :: nmm
  parameter(mn = 1e-12_lrk,blank = ' ',nmm = 'linintx')
  character(len=99) :: errmsg
!
  intrinsic :: abs, real
!
  dx = rpg(ix)-rpg(iy)
!     delta x = zero message
  if (dx .eq. 0) then
    errmsg = fwmsgf(99, nmm,3,blank,0)
    call lmsg(0,errmsg)
  end if
  dy = real(abs(amp(ip,ix))-abs(amp(ip,iy)), lrk)
  a = dy/(dx+mn)
  b = real(abs(amp(ip,ix)), lrk)-a*rpg(ix)
!
  linintx = (y-b)/a
!
  return
!
end function linintx
!
!     ==================================================================
!>    @brief get the x value for a given y with a starting point,
!>      x is the speed and y the amplitude.
!
!>    @param[in] ym value for the required amplitude
!>    @param[in] ix speed and aplitude x position index
!>    @param[in] ip number of the desired position
!>    @param[in] lf search direction 1=left, -1=right
!>    @param[in] rpg speed vector rpm
!>    @param[in] amp complex amplitude matrix
!>    @param[in] np number of output points
!>    @param[in] mxp position, first dimension of amplitude matrix
!>    @param[in] mte speed, second dimension of amplitude matrix
!>    @return x value for the given y
!
real(lrk) function getbds(ym,ix,ip,lf,rpg,amp,np,mxp,mte)
  use rd_kinds, only: lrk, wp
  implicit none
!
!     arguments
  integer :: mxp, mte, ix, ip, lf, np
  real(lrk) :: ym
  real(lrk) :: rpg(mte)
  complex(wp) :: amp(mxp,mte)
!
!     locals
  integer :: iy, px
  real(lrk) :: p2, dy
  real(lrk) :: linintx
!
  intrinsic :: abs, real
!
  px = ix
  iy = px-lf
  p2 = real(abs(amp(ip,iy)), lrk)
  dy = p2-real(abs(amp(ip,px)), lrk)
!
  do while ((p2 .gt. ym) .and. ((iy .gt. 1)) .and.&
  &(dy .lt. 0) .and. (iy .lt. np))
    dy = p2-real(abs(amp(ip,px)), lrk)
    px = iy
    iy = iy-lf
!       can calculate only iy>=1 and iy<=np
    p2 = real(abs(amp(ip,iy)), lrk)
  end do
!     check for loop out
  if ((dy .ge. 0) .or. (iy .eq. 1) .or. (iy .eq. np)) Then
!       take equals to the other side, mirror.
    getbds = 0
  else
!       linear interpolation
    getbds = linintx(px,iy,ip,ym,rpg,amp,mxp,mte)
  end if
!
  return
!
end function getbds
!
!     ==================================================================
!>    @brief calculates de amplification factor foe a aplitute peak,
!>    searches for a value of 0.707 x peak value and
!>    calculates the amplification factor, AF (like API 541, 4th Ed).
!
!>    @param[in] ix speed and aplitude x position index
!>    @param[in] ip number of the desired position
!>    @param[in] rpg speed vector rpm
!>    @param[in] amp complex amplitude matrix
!>    @param[in] np number of output points
!>    @param[in] mxp position, first dimension of amplitude matrix
!>    @param[in] mte speed, second dimension of amplitude matrix
!>    @param[out] st status (n)ormal or assumes the same speed on
!>     the side (i)nitial (f)inal (F)ail if nothing could be got
!>    @return the amplification factor AF for an amplitude peak.
!
real(lrk) function fatamof(ix,ip,rpg,amp,np,mxp,mte,st)
  use rd_kinds, only: lrk, wp
  implicit none
!
!     arguments
  integer :: mxp, mte, ix, ip, np
  real(lrk) :: rpg
  complex(wp) :: amp
  character(len=1) :: st
  dimension rpg(mte),amp(mxp,mte)
!
!     locals
  real(lrk) :: xc, ym, xi, xf, sq2
  parameter (sq2 = 0.70710678_lrk)
  character(len=1) :: sa(4)
  parameter (sa = (/'F','n','i','f'/))
!     function
  real(lrk) :: getbds
!
  intrinsic :: abs, real
!
!     start with fail (F)
  st = sa(1)
  xc = rpg(ix)
  ym = sq2 * real(abs(amp(ip,ix)), lrk)
  xi = getbds(ym,ix,ip,1,rpg,amp,np,mxp,mte)
  xf = getbds(ym,ix,ip,-1,rpg,amp,np,mxp,mte)
  if (xi .eq. 0 .and. xf .eq. 0) then
!       not found at any side
    fatamof = -1
  else
!       status ok, not (F)ail
    st = sa(2)
    if (xi .eq. 0) then
!         mirror the same distance to the other side, (i)nitial
      xi = xc-(xf-xc)
      st = sa(3)
    end if
    if (xf .eq. 0) then
!         mirror the same distance to the other side, (f)inal
      xf = xc+(xc-xi)
      st = sa(4)
    end if
    fatamof = xc/(xf-xi)
!
  end if
!
  return
!
end function fatamof
!
!     ==================================================================
!>    @brief calculates the quadratic coefficients of a three points cur
!
!>    @param[in] i1 first position of the points in the rpg e amp matric
!>    @param[in] i2 second position of the points in the rpg e amp matri
!>    @param[in] i3 third position of the points in the rpg e amp matric
!>    @param[in] ip number of the desired position
!>    @param[out] ret return the quadratic coefficients
!>           ret(1)*x^2+ret(2)*x+ret(3)
!>    @param[in] rpg speed vector rpm
!>    @param[in] amp complex amplitude matrix
!>    @param[in] mxp position, first dimension of amplitude matrix
!>    @param[in] mte speed, second dimension of amplitude matrix
!
subroutine quad(i1,i2,i3,ip,ret,rpg,amp,mxp,mte)
  use rd_kinds, only: lrk, wp
  implicit none
!
!     arguments
  integer :: i1, i2, i3, ip, mxp, mte
  real(lrk) :: rpg, ret
  complex(wp) :: amp
  dimension ret(3),rpg(mte),amp(mxp,mte)
!
  real(lrk) :: xx, yy
  dimension xx(3),yy(3)
!
  intrinsic :: abs, real
!
  xx(1) = rpg(i1)
  yy(1) = real(abs(amp(ip,i1)), lrk)
  xx(2) = rpg(i2)
  yy(2) = real(abs(amp(ip,i2)), lrk)
  xx(3) = rpg(i3)
  yy(3) = real(abs(amp(ip,i3)), lrk)
!     calculate quadratic coefficients -> ret
  call qcfr(xx,yy,ret)
!
  return
!
end subroutine quad
!
!     ==================================================================
!>    @brief makes the analysis of the unbalance response, searching
!>    for amplification peaks above a certain amplification
!>    factor AF (much like API 541 4th Ed).
!
!>    @param[in] np number of output points
!>    @param[in] nr number of speed vector values
!>    @param[out] cn returns number of found maximums
!>    @param[in] rpg speed vector (rpm)
!>    @param[in] amp complex amplitude matrix
!>    @param[out] vf amplification factor vector
!>    @param[out] vr maximum speeds vector
!>    @param[out] vm maximums values
!>    @param[out] st status (n)ormal or assumes the same speed on
!>     the side (i)nitial (f)inal (F)ail if nothing could be got
!>    @param[in] mxp position, first dimension of amplitude matrix
!>    @param[in] mte speed, second dimension of amplitude matrix
!>    @param[out] errmsg return an error message
!>    @param[out] ok return flag. unsuccessful if <0.
!
subroutine maxamp(np,nr,cn,rpg,amp,vf,vr,vm,st,mxp,mte,errmsg,ok)
  use rd_textfun, only: fomsgf
  use rd_kinds, only: lrk, wp
  implicit none
!
!     arguments
  integer :: np, nr, mxp, mte, ok, cn
  real(lrk) :: rpg, vf, vm, vr
  character(len=1) :: st
  character(len=99) :: errmsg
  complex(wp) :: amp
  dimension cn(mxp),rpg(mte),vf(mxp,2*mxp),&
  &vm(mxp,2*mxp),vr(mxp,2*mxp),amp(mxp,mte),&
  &st(mxp,2*mxp)
!
!     locals
  character(len=1) :: stl
  character(len=3) :: cbd
  character(len=6) :: nmm
  integer :: ii, jj, i1, i2, i3
  real(lrk) :: ld, dx, dy, fa, dv, vx, vy, mn, prec, cf, fatamof
  dimension cf(3)
!     minimal
  parameter(prec = 1e-12_lrk,mn = 1e-15_lrk,cbd = 'mxp',nmm = 'maxamp')
!
  intrinsic :: abs, real
!
!     initialize return
  ok = -1
!     initial position amplitude points vector
  i1 = 1
!     ld = 0. derivative
!     number of analysis points
  do ii = 1,np
!       derivative
    ld = 0._lrk
!       init maximum counter
    cn(ii) = 0
!       number of calculated points
    do jj = 2,nr
      i2 = jj
!         delta x
      dx = rpg(jj)-rpg(jj-1)
!         delta y
      dy = real(abs(amp(ii,jj))-abs(amp(ii,(jj-1))), lrk)
      dv = dy / (dx + mn)
      if (dv .le. 0 .and. ld .gt. 0 .and. abs(dy) .gt. prec) then
        cn(ii) = cn(ii)+1
!           amplification factor
        fa = fatamof((jj-1),ii,rpg,amp,nr,mxp,mte,stl)
!           check if found
!           three points to calculate quadratic maximum
        if (jj .gt. 2) then
          i3 = jj-2
        else
          if (jj .lt. np) then
            i3 = jj+1
          else
            i3 = jj
          end if
        end if
!           calculates a parabola with 2 points and maximum
        call quad(i1,i2,i3,ii,cf,rpg,amp,mxp,mte)
!           calculates maximum presumed parabola
        call mxqr(vx,vy,cf)
        if (cn(ii) .le. (2*mxp)) then
!             amplification factor
          vf(ii,cn(ii)) = fa
!             speed
          vr(ii,cn(ii)) = vx
!             maximum amplitude
          vm(ii,cn(ii)) = vy
!             amplification status
          st(ii,cn(ii)) = stl
        else
!             'out of bounds' !4
          errmsg = fomsgf(99, nmm,4,cbd,0)
          return
        end if
!           derivative if
      end if
!
      ld = dv
      i1 = i2
!
    end do
!
  end do
!
!     return ok
  ok = 0
!
  return
!
end subroutine maxamp
!
