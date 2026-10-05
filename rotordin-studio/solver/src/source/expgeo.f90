!     $Id$
!     ==================================================================
!
!>    @file expgeo.f
!>    @author francisco
!>    @date 27-jan-20
!>    @brief geometry export, last changes:<br>
!>    new file, code moved from saidas.f - francisco - jan-20<br>
!>    changed mxb = 99 francisco - apr-20<br>
!>    added excitation position duplicity avoidance - 28-apr-20<br>
!>    updated efrsp, check for resp. support (< 0) - francisco - aug-21<
!>    added dist. force excitation - francisco - sep-21<br>
!>    updated eexc, added offset for distributed force - francisco - dec
!
!     ==================================================================
!>    @brief generate nodal coordinates for concentrated 1,2,3,4,5,6 ccw
!
!>    @param[in] xs x section position
!>    @param[in] ds section diameter
!>    @param[in] ts side size
!>    @param[out] x four x positions vector
!>    @param[out] y four y positions vector
!>    @param[in] dm coordinate vectors dimension
!
subroutine grncon(xs,ds,ts,x,y,dm)
  use rd_kinds, only: lrk
  implicit none
!
  integer :: dm
  real(lrk) :: xs, ds, ts, x, y
  dimension x(dm),y(dm)
!     size scaling
  real(lrk) :: sc, ld
  parameter (sc = 2.67_lrk,ld = 1.2_lrk)
!
  real(lrk) :: ls, riff
!
!        4
!     5 / \ 3
!      |   |
!     6 \ / 2
!        *1
!
  ls = riff(ts .le. 0,1.5_lrk*ds,ts)
!
  x(1) = xs
  y(1) = ds/2
  x(2) = x(1)+ls/ld/sc
  y(2) = y(1)+ls/sc
  x(3) = x(2)
  y(3) = y(2)+ls*ld/sc
  x(4) = x(1)
  y(4) = y(3)+ls/sc
  x(5) = x(1)-ls/ld/sc
  y(5) = y(3)
  x(6) = x(5)
  y(6) = y(2)
!
  return
!
end subroutine grncon
!
!     ==================================================================
!>    @brief generate nodal coordinates for torsion response frequency
!>     excitation  1,2,3,4,5,6 ccw
!
!>    @param[in] xs x section position
!>    @param[in] ds section diameter
!>    @param[in] ts side size
!>    @param[out] x four x positions vector
!>    @param[out] y four y positions vector
!>    @param[in] dm coordinate vectors dimension
!
subroutine grntex(xs,ds,ts,x,y,dm)
  use rd_kinds, only: lrk
  implicit none
!
  integer :: dm
  real(lrk) :: xs, ds, ts, x, y
  dimension x(dm),y(dm)
!     size scaling
  real(lrk) :: sc, ld
  parameter (sc = 2.67_lrk,ld = 1.2_lrk)
!
  real(lrk) :: ls, riff
!
!
!     5     3
!      |\4/|
!     6 \ / 2
!        *1
!
  ls = riff(ts .le. 0,1.5_lrk*ds,ts)
!
  x(1) = xs
  y(1) = ds/2
  x(2) = x(1)+ls/ld/sc
  y(2) = y(1)+ls/sc
  x(3) = x(2)
  y(3) = y(2)+ls*ld/sc
  x(4) = x(1)
  y(4) = y(3)-ls/sc
  x(5) = x(1)-ls/ld/sc
  y(5) = y(3)
  x(6) = x(5)
  y(6) = y(2)
!
  return
!
end subroutine grntex
!
!     ==================================================================
!>    @brief generate nodal coordinates for unbalance 1,2,3,4,5 ccw
!
!>    @param[in] xs x section position
!>    @param[in] ds section diameter
!>    @param[in] ts side size
!>    @param[out] x four x positions vector
!>    @param[out] y four y positions vector
!>    @param[in] dm coordinate vectors dimension
!
subroutine grnunb(xs,ds,ts,x,y,dm)
  use rd_kinds, only: lrk
  implicit none
!
  integer :: dm
  real(lrk) :: xs, ds, ts, x, y
  dimension x(dm),y(dm)
!     size scaling
  real(lrk) :: sc
  parameter (sc = 2.67_lrk)
!
  real(lrk) :: ls, riff
!
!      4_3
!     5| |2
!      \1/
!       *
!
  ls = riff(ts .le. 0,1.5_lrk*ds,ts)
!
  x(1) = xs
  y(1) = ds/2
  x(2) = x(1)+ls/sc
  y(2) = y(1)+ls/sc
  x(3) = x(2)
  y(3) = y(2)+1.5_lrk*ls/sc
  x(4) = x(1)-ls/sc
  y(4) = y(3)
  x(5) = x(4)
  y(5) = y(2)
!
  return
!
end subroutine grnunb
!
!     ==================================================================
!>    @brief generate nodal coordinates for sections 1,2,3,4 cw
!
!>    @param[in] xs x section end position
!>    @param[in] ds external diameter
!>    @param[in] ls section length
!>    @param[out] x four x positions vector
!>    @param[out] y four y positions vector
!>    @param[in] dm coordinate vectors dimension
!
subroutine gensec(xs,ds,ls,x,y,dm)
  use rd_kinds, only: lrk
  implicit none
!
!     arguments
  integer :: dm
  real(lrk) :: xs, ds, ls, x, y
  dimension x(dm),y(dm)
!
!     3 _ 2
!     4|_|1
!
  x(1) = xs
  y(1) = -ds/2
  x(2) = x(1)
  y(2) = ds/2
  x(3) = x(1)-ls
  y(3) = y(2)
  x(4) = x(3)
  y(4) = y(1)
!
  return
!
end subroutine gensec
!
!     ==================================================================
!>    @brief generate nodal coordinates for divisions and disks 1,2,3,4
!
!>    @param[in] xs x section position
!>    @param[in] ds external diameter
!>    @param[in] di internal diameter (disk)
!>    @param[in] ls divisions/disk length
!>    @param[out] x four x positions vector
!>    @param[out] y four y positions vector
!>    @param[in] dm coordinate vectors dimension
!
subroutine grndre(xs,ds,di,ls,x,y,dm)
  use rd_kinds, only: lrk
  implicit none
!
!     locals
  real(lrk) :: sig, dil, riff
!
!     arguments
  integer :: dm
  real(lrk) :: xs, ds, di, ls, x, y
  dimension x(dm),y(dm)
!
!     2 _ 3    1 _ 4
!     1|_|4 or 2|_|3
!
  sig = riff (di .lt. 0._lrk,-1.0_lrk,1.0_lrk)
  dil = riff (di .eq. 0._lrk,-ds,di)
!
  x(1) = xs
  y(1) = sig * ds / 2.0_lrk
  x(2) = x(1)
  y(2) = dil / 2.0_lrk
  x(3) = x(1) + ls
  y(3) = y(2)
  x(4) = x(3)
  y(4) = y(1)
!
  return
!
end subroutine grndre
!
!     ==================================================================
!>    @brief generate nodal coordinates for frequency response 1,2,3 cw
!
!>    @param[in] xs x section position
!>    @param[in] ds section diameter
!>    @param[in] ts hex side
!>    @param[in] an response angle
!>    @param[out] x x positions vector
!>    @param[out] y y positions vector
!>    @param[in] dm coordinate vectors dimension
!
subroutine grnfrs(xs,ds,ts,an,x,y,dm)
  use rd_kinds, only: lrk
  implicit none
!
!     arguments
  integer :: dm
  real(lrk) :: xs, ds, ts, an, x, y
  dimension x(dm),y(dm)
!
  real(lrk) :: ls, riff
  intrinsic :: cos, sin
!
!     2 _3
!     1|
!      *
!
  ls = riff(ts .le. 0,0.75_lrk*ds,ts)
!
  x(1) = xs
  y(1) = ds/2
  x(2) = x(1)
  y(2) = y(1)+ls*1.25_lrk
  x(3) = x(2)+ls/2*cos(an)
  y(3) = y(2)+ls/2*sin(an)
!
  return
!
end subroutine grnfrs
!
!     ==================================================================
!>    @brief generate nodal coordinates for time response 1,2,3 cw
!
!>    @param[in] xs x section position
!>    @param[in] ds section diameter
!>    @param[in] ts side length
!>    @param[in] an response angle
!>    @param[out] x x positions vector
!>    @param[out] y y positions vector
!>    @param[in] dm coordinate vectors dimension
!
subroutine grntrs(xs,ds,ts,an,x,y,dm)
  use rd_kinds, only: lrk
  implicit none
!
!     arguments
  integer :: dm
  real(lrk) :: xs, ds, ts, an, x, y
  dimension x(dm),y(dm)
!
  real(lrk) :: ls, riff
  intrinsic :: cos, sin
!
!     *1
!      |_
!     2  3
!
  ls = riff(ts .le. 0,0.75_lrk*ds,ts)
!
  x(1) = xs
  y(1) = -ds/2
  x(2) = x(1)
  y(2) = y(1)-ls*1.25_lrk
  x(3) = x(2)+ls/2*cos(an)
  y(3) = y(2)-ls/2*sin(an)
!
  return
!
end subroutine grntrs
!
!     ==================================================================
!>    @brief generate nodal coordinates for torque excitation 1,2,3,4 cc
!
!>    @param[in] xs x section position
!>    @param[in] ds section diameter
!>    @param[in] ts diamond side
!>    @param[out] x x positions vector
!>    @param[out] y y positions vector
!>    @param[in] dm coordinate vectors dimension
!
subroutine grndia(xs,ds,ts,x,y,dm)
  use rd_kinds, only: lrk
  implicit none
!
!     arguments
  integer :: dm
  real(lrk) :: xs, ds, ts, x, y
  dimension x(dm),y(dm)
!
  real(lrk) :: ls, riff
!
!      3
!     4/\2
!      \/
!       *1
!
  ls = riff(ts .le. 0,3.0_lrk/4.0_lrk*ds,ts)
!
  x(1) = xs
  y(1) = ds/2
  x(2) = xs+ls/2.5_lrk
  y(2) = y(1)+ls/2
  x(3) = xs
  y(3) = y(1)+ls
  x(4) = xs-ls/2.5_lrk
  y(4) = y(2)
!
  return
!
end subroutine grndia
!
!     ==================================================================
!>    @brief generate nodal coordinates for static force 1,2,3 ccw.
!
!>    @param[in] xs static force x position (m)
!>    @param[in] ds section diameter
!>    @param[in] ts triangle side
!>    @param[in] x four x positions vector
!>    @param[in] y four y positions vector
!>    @param[in] dm coordinate vectors dimension
!
subroutine grnutr(xs,ds,ts,x,y,dm)
  use rd_kinds, only: lrk
  implicit none
!
!     arguments
  integer :: dm
  real(lrk) :: xs, ds, ts
  real(lrk) :: x, y
  dimension x(dm),y(dm)
!
  real(lrk) :: ht, ls, riff
!
!     3 _ 2
!      \ /
!       *1
!
  ls = riff (ts .le. 0,3.0_lrk/4.0_lrk*ds,ts)
  ht = 0.866_lrk*ls
!
  x(1) = xs
  y(1) = ds/2
  x(2) = x(1)+ls/2
  y(2) = y(1)+ht
  x(3) = x(1)-ls/2
  y(3) = y(2)
!
  return
!
end subroutine grnutr
!
!     ==================================================================
!>    @brief generate nodal coordinates for static force offset 1,2,3 cc
!
!>    @param[in] xs static force x position (m)
!>    @param[in] of static force x offset (m)
!>    @param[in] ds section diameter
!>    @param[in] ts triangle side
!>    @param[in] x four x positions vector
!>    @param[in] y four y positions vector
!>    @param[in] dm coordinate vectors dimension
!
subroutine grnrso(xs,of,ds,ts,x,y,dm)
  use rd_kinds, only: lrk
  implicit none
!
!     arguments
  integer :: dm
  real(lrk) :: xs, of, ds, ts
  real(lrk) :: x, y
  dimension x(dm),y(dm)
!
  real(lrk) :: ht, ls, riff
!
!      _ _
!      \+/--+2
!       3   |
!           *1
!
  ls = riff (ts .le. 0,3.0_lrk/4.0_lrk*ds,ts)
  ht = 0.866_lrk*ls
!
  x(1) = xs
  y(1) = ds/2
  x(2) = x(1)
  y(2) = y(1)+ht/2
  x(3) = x(1)+of
  y(3) = y(2)
!
  return
!
end subroutine grnrso
!     ==================================================================
!>    @brief generate nodal coordinates for lateral bearing 1,2,3 ccw.
!
!>    @param[in] xs bearing x position (m)
!>    @param[in] ds section diameter
!>    @param[in] ts triangle side
!>    @param[in] x four x positions vector
!>    @param[in] y four y positions vector
!>    @param[in] dm coordinate vectors dimension
!
subroutine grndtr(xs,ds,ts,x,y,dm)
  use rd_kinds, only: lrk
  implicit none
!
!     arguments
  integer :: dm
  real(lrk) :: xs, ds, ts
  real(lrk) :: x, y
  dimension x(dm),y(dm)
!
!     locals
  real(lrk) :: ht, ls, riff
!
  intrinsic :: sqrt
!
!       *1
!     2/_\3
!
  ls = riff(ts .le. 0,3.0_lrk/4.0_lrk*ds,ts)
  ht = 0.866_lrk*ls
!
  x(1) = xs
  y(1) = -ds/2
  x(2) = x(1)-ls/2
  y(2) = y(1)-ht
  x(3) = x(1)+ls/2
  y(3) = y(2)
!
  return
!
end subroutine grndtr
!
!     ==================================================================
!>    @brief generate nodal coordinates for harmonic torque 1,2,3 ccw
!
!>    @param[in] xs x section position
!>    @param[in] ds section diameter
!>    @param[in] ts triangle side
!>    @param[out] x x positions vector
!>    @param[out] y y positions vector
!>    @param[in] dm coordinate vectors dimension
!
subroutine grntrr(xs,ds,ts,x,y,dm)
  use rd_kinds, only: lrk
  implicit none
!
!     arguments
  integer :: dm
  real(lrk) :: xs, ds, ts
  real(lrk) :: x, y
  dimension x(dm),y(dm)
!
  real(lrk) :: ls, fac, riff
  parameter(fac = 0.55_lrk)
!
!     3
!     |>2
!     *1
!
  ls = riff(ts .le. 0,3.0_lrk/4.0_lrk*ds,ts)
  x(1) = xs
  y(1) = ds/2
  x(2) = x(1)+ls*fac
  y(2) = y(1)+ls*fac
  x(3) = x(1)
  y(3) = y(1)+2*ls*fac
!
  return
!
end subroutine grntrr
!
!     ==================================================================
!>    @brief generate nodal coordinates for harmonic excitation 1,2,3 cc
!
!>    @param[in] xs x section position
!>    @param[in] ds section diameter
!>    @param[in] ts triangle side
!>    @param[out] x x positions vector
!>    @param[out] y y positions vector
!>    @param[in] dm coordinate vectors dimension
!
subroutine grntrl(xs,ds,ts,x,y,dm)
  use rd_kinds, only: lrk
  implicit none
!
!     arguments
  integer :: dm
  real(lrk) :: xs, ds, ts
  real(lrk) :: x, y
  dimension x(dm),y(dm)
!
  real(lrk) :: ls, fac, riff
  parameter(fac = 0.55_lrk)
!
!       2
!     3<|
!       *1
!
  ls = riff(ts .le. 0,3.0_lrk/4.0_lrk*ds,ts)
  x(1) = xs
  y(1) = ds/2
  x(2) = x(1)
  y(2) = y(1)+2*ls*fac
  x(3) = x(1)-ls*fac
  y(3) = y(1)+ls*fac
!
  return
!
end subroutine grntrl
!
!     ==================================================================
!>    @brief generate nodal coordinates for torsion restriction 1,2,3 cc
!
!>    @param[in] xs x section position
!>    @param[in] ds section diameter
!>    @param[in] ts triangle side
!>    @param[out] x x positions vector
!>    @param[out] y y positions vector
!>    @param[in] dm coordinate vectors dimension
!
subroutine grnltr(xs,ds,ts,x,y,dm)
  use rd_kinds, only: lrk
  implicit none
!
!     arguments
  integer :: dm
  real(lrk) :: xs, ds, ts
  real(lrk) :: x, y
  dimension x(dm),y(dm)
!
  real(lrk) :: ls, fac, riff
  parameter(fac = 0.55_lrk)
!
!     2
!     |>*1
!     3
!
  ls = riff(ts .le. 0,3.0_lrk/4.0_lrk*ds,ts)
  x(1) = xs
  y(1) = ds/2
  x(2) = x(1)-ls*fac
  y(2) = y(1)+ls*fac
  x(3) = x(1)-ls*fac
  y(3) = y(1)-ls*fac
!
  return
!
end subroutine grnltr
!
!     ==================================================================
!>    @brief generate nodal coordinates for torsion restriction 1,2,3 cc
!
!>    @param[in] xs1 x section initial position
!>    @param[in] xs2 x section final position
!>    @param[in] ds section diameter
!>    @param[in] ts triangle side
!>    @param[out] x x positions vector
!>    @param[out] y y positions vector
!>    @param[in] dm coordinate vectors dimension
!
subroutine grndfr(xs1,xs2,ds,ts,x,y,dm)
  use rd_kinds, only: lrk
  implicit none
!
!     arguments
  integer :: dm
  real(lrk) :: xs1, xs2, ds, ts
  real(lrk) :: x, y
  dimension x(dm),y(dm)
!
  real(lrk) :: ls, fac, riff
  parameter(fac = 0.66_lrk)
!
!     *1    4
!     |     !
!     2 ----3
!
  ls = riff(ts .le. 0,3.0_lrk/4.0_lrk*ds,ts)
  x(1) = xs1
  y(1) = -ds/2
  x(2) = x(1)
  y(2) = y(1)-ls*fac
  x(3) = xs2
  y(3) = y(2)
  x(4) = x(3)
  y(4) = y(1)
!
  return
!
end subroutine grndfr
!
!     ==================================================================
!>    @brief write down element nodes positions
!
!>    @param[in] iu output unit handler number
!>    @param[in] nn number of nodes
!>    @param[in] dm node position vectors dimension
!>    @param[in] xg node x position vector
!>    @param[in] yg node y position vector
!
subroutine wgeo(iu,nn,dm,xg,yg)
  use rd_kinds, only: lrk
  implicit none
!
  integer :: iu, nn, dm
  real(lrk) :: xg, yg
  dimension xg(dm),yg(dm)
!
  integer :: j
!
  do j = 1,nn
!       void upper bound error
    if (nn .gt. dm) exit
    write(iu,300,err=100)xg(j),yg(j),0._lrk
  end do
!
  return
!
100 call elmsge(1,13,'wgeo')
!
300 format(3f10.6)
!
end subroutine wgeo
!
!     ==================================================================
!>    @brief sections geometry export
!
!>    @param[in] iu output unit handle
!>    @param[in] plt plot output flag
!>    @param[in] txt output text
!>    @param[in] cs total number of sections
!>    @param[in] mxs section vector dimension
!>    @param[in,out] cnt output index
!>    @param[in] ps section positions vector
!>    @param[in] d sections external diameter
!
subroutine esec(iu,plt,txt,cs,mxs,cnt,ps,d)
  use rd_kinds, only: lrk
  implicit none
!
  integer :: iu, cs, mxs, cnt
  real(lrk) :: ps, d
  dimension ps(mxs),d(mxs)
  logical :: plt, txt
!
  integer :: i
  real(lrk) :: ls, xg, yg
  dimension xg(4),yg(4)
!
  ls = ps(1)
  do i = 1,cs
    if (i .gt. 1) ls = ps(i)-ps(i-1)
    call gensec(ps(i),d(i),ls,xg,yg,4)
    if (txt) call wgeo(iu,4,4,xg,yg)
    cnt = cnt+1
    if (plt) call pgeom(cnt,1,4,4,xg,yg)
  end do
!
  return
!
end subroutine esec
!
!     ==================================================================
!>    @brief divisions geometry export
!
!>    @param[in] iu output unit handle
!>    @param[in] plt plot output flag
!>    @param[in] txt output text
!>    @param[in] nt total number of divisions
!>    @param[in] mts divisions vector dimension
!>    @param[in,out] cnt output index
!>    @param[in,out] ci divisions with internal diameter counter
!>    @param[in] y division positions vector
!>    @param[in] ds divisions external diameter vector
!>    @param[in] di_s divisions internal diameter vector
!
subroutine ediv(iu,plt,txt,nt,mts,cnt,ci,y,ds,di_s)
  use rd_kinds, only: lrk
  implicit none
!
  integer :: iu, cnt, ci, nt, mts
  real(lrk) :: y, ds, di_s
  dimension y(mts),ds(mts+1),di_s(mts)
  logical :: plt, txt
!
  integer :: i
  real(lrk) :: de, ls, xg, yg
  dimension xg(4),yg(4)
!
!     sections with external diameter
  do i = 1,nt-1
!       section length
    ls = y(i+1)-y(i)
    de = ds(i+1)
    call grndre(y(i),de,0._lrk,ls,xg,yg,4)
    if (txt) call wgeo(iu,4,4,xg,yg)
    cnt = cnt + 1
    if (plt) call pgeom(cnt,2,4,4,xg,yg)
  end do
!
  ci = 0
!     sections with internal diameter
  do i = 1,nt-1
!       section lengths
    ls = y(i+1)-y(i)
    de = di_s(i+1)
    if (de .gt. 0) then
      call grndre(y(i),de,0._lrk,ls,xg,yg,4)
      if (txt) call wgeo(iu,4,4,xg,yg)
      ci = ci+1
      cnt = cnt+1
      if (plt) call pgeom(cnt,2,4,4,xg,yg)
    end if
  end do
!
  return
!
end subroutine ediv
!
!     ==================================================================
!>    @brief torsion coupling geometry export.
!>    draws a "x" on the coupling division |x|.
!
!>    @param[in] iu output unit handle
!>    @param[in] plt plot output flag
!>    @param[in] txt output text
!>    @param[in] ncplg total number of torsion couplings
!>    @param[in] cplgid coupling section indices vector
!>    @param[in] mxcplg coupling indices vector dimension
!>    @param[in] mxs section vector dimension
!>    @param[in,out] cnt output index
!>    @param[in] ps section positions vector
!>    @param[in] d sections external diameter
!
subroutine etcp(iu,plt,txt,ncplg,cplgid,mxcplg,&
&mxs,cnt,ps,d)
  use rd_kinds, only: lrk
  implicit none
!
  integer :: iu, ncplg, cplgid, mxcplg, mxs, cnt
  real(lrk) :: ps, d
  dimension cplgid(mxcplg),ps(mxs),d(mxs)
  logical :: plt, txt
!
  integer :: i, j
  real(lrk) :: xg, yg
  dimension xg(2),yg(2)
!
  do i = 1,ncplg
!       coupling section index
    j = cplgid(i)
!       first section init position is zero
    if (j .eq. 1) then
      xg(1) = 0
    else
      xg(1) = ps(j-1)
    end if
!       section end position
    xg(2) = ps(j)
    yg(1) = -d(j)/2
    yg(2) = d(j)/2
!       from down to up
    if (txt) call wgeo(iu,2,2,xg,yg)
    cnt = cnt + 1
    if (plt) call pgeom(cnt,14,-2,2,xg,yg)
    yg(1) = d(j)/2
    yg(2) = -d(j)/2
!       from up to down
    if (txt) call wgeo(iu,2,2,xg,yg)
    cnt = cnt + 1
    if (plt) call pgeom(cnt,14,-2,2,xg,yg)
  end do
!
  return
!
end subroutine etcp
!
!     ==================================================================
!>    @brief disk geometry export
!
!>    @param[in] iu output unit handle
!>    @param[in] plt plot output flag
!>    @param[in] txt output text
!>    @param[in] nd total number of disks
!>    @param[in] mxd disk vector dimension
!>    @param[in] mts section vector dimension
!>    @param[in,out] cnt output index
!>    @param[out] no number of disk offset
!>    @param[in] y section positions vector
!>    @param[in] ds sections external diameter
!>    @param[in] pd disk positions vector
!>    @param[in] d_d disk external diameter vector
!>    @param[in] h_d disk length vector
!>    @param[in] d_i disk internal diameter vector
!>    @param[in] off_d disk offset vector
!>    @param[out] errmsg return an error message
!>    @param[out] ok return flag. unsuccessful if <0.
!
subroutine edsk(iu,plt,txt,nd,mxd,mts,cnt,no,y,ds,&
&pd,d_d,h_d,d_i,off_d,errmsg,ok)
  use rd_kinds, only: lrk
  implicit none
!
  integer :: iu, nd, mxd, mts, cnt, no
  real(lrk) :: y, ds, pd, d_d, h_d, d_i, off_d
  dimension y(mts),ds(mts+1),&
  &pd(mxd),d_d(mxd),h_d(mxd),d_i(mxd),off_d(mxd)
  integer :: ok
  character(len=99) :: errmsg
  logical :: plt, txt
!
  integer :: i, jd, inddis, oki
  real(lrk) :: de, ls, xg, yg
  dimension xg(4),yg(4)
!
  ok = -1
!     disks
  no = 0
  if (nd .gt. 0) then
    do i = 1,nd
!         disk position index
      jd = inddis(pd(i),errmsg,oki)
      if (oki .lt. 0) return
!         added check internal diameter - francisco - feb-19
      if (d_i(i) .le. 0) then
!           section diameter
        de = ds(jd)
      else
        de = d_i(i)
      end if
!         added ls - francisco - feb-19
      ls = y(jd)-h_d(i)/2._lrk+off_d(i)
      call grndre(ls,d_d(i),de,h_d(i),xg,yg,4)
      if (txt) call wgeo(iu,4,4,xg,yg)
      cnt = cnt + 1
      if (plt) call pgeom(cnt,3,4,4,xg,yg)
!
      call grndre(ls,d_d(i),-de,h_d(i),xg,yg,4)
      if (txt) call wgeo(iu,4,4,xg,yg)
      cnt = cnt + 1
      if (plt) call pgeom(cnt,3,4,4,xg,yg)
!         added offset support - francisco - feb-19
      if (off_d(i) .ne. 0) then
!           number of disks with offset
        no = no+1
!           x1 position
        xg(1) = y(jd)+off_d(i)
!           y1 position
        yg(1) = -(d_d(i)/2._lrk+de/2._lrk)/2._lrk
        xg(2) = y(jd)
        yg(2) = yg(1)
        xg(3) = xg(2)
        yg(3) = -yg(1)
        xg(4) = xg(1)
        yg(4) = yg(3)
        if (txt) call wgeo(iu,4,4,xg,yg)
        cnt = cnt + 1
        if (plt) call pgeom(cnt,3,-4,4,xg,yg)
      end if
    end do
  end if
!
  ok = 0
!
  return
!
end subroutine edsk
!
!     ==================================================================
!>    @brief lateral bearing geometry export
!
!>    @param[in] iu output unit handle
!>    @param[in] plt plot output flag
!>    @param[in] txt output text
!>    @param[in] nbrg number of lateral bearings
!>    @param[in] ntbr  number of torsion restricntion
!>    @param[in] mxm bearing vector dimension
!>    @param[in] mts section vector dimension
!>    @param[in,out] cnt output index
!>    @param[in] tlen geometry total length (size)
!>    @param[in] y section positions vector
!>    @param[in] ds sections external diameter
!>    @param[in] pc bearing position vector
!>    @param[in] tpc torsion resctriction position vector
!>    @param[out] errmsg return an error message
!>    @param[out] ok return flag. unsuccessful if <0.
!
subroutine ebrg(iu,plt,txt,nbrg,ntbr,mxm,mts,cnt,tlen,&
&y,ds,pc,tpc,errmsg,ok)
  use rd_kinds, only: lrk
  implicit none
!
  integer :: iu, nbrg, ntbr, mxm, mts, cnt
  real(lrk) :: tlen, y, ds, pc, tpc
  dimension y(mts),ds(mts+1),pc(mxm),tpc(mxm)
  integer :: ok
  character(len=99) :: errmsg
  logical :: plt, txt
!
  integer :: i, jd, inddis, oki
  real(lrk) :: ls, xg, yg
  dimension xg(4),yg(4)
!
  ok = -1
!     bearing size
  ls = tlen/30
!     lateral standard bearings
  do i = 1,nbrg
!       bearing positions index
    jd = inddis(pc(i),errmsg,oki)
    if (oki .lt. 0) return
    call grndtr(y(jd),ds(jd),ls,xg,yg,4)
    if(txt) call wgeo(iu,3,4,xg,yg)
    cnt = cnt+1
    if (plt) call pgeom(cnt,4,3,4,xg,yg)
  end do
!     torsion restriction
  do i = 1,ntbr
!       bearing positions index
    jd = inddis(tpc(i),errmsg,oki)
    if (oki .lt. 0) return
    call grnltr(y(jd),0._lrk,ls,xg,yg,4)
    if(txt) call wgeo(iu,3,4,xg,yg)
    cnt = cnt+1
    if (plt) call pgeom(cnt,5,3,4,xg,yg)
  end do
!
  ok = 0
!
  return
!
end subroutine ebrg
!
!     ==================================================================
!>    @brief check if position was set for type tp on vector xp.
!>    returns true if passed cx position was not set yet on vector xp.
!
!>    @param[in] cx current position
!>    @param[in,out] xp positions set per type
!>    @param[in,out] np type index count
!>    @param[in] tp number of positions set per type
!>    @param[in] nk types dimension
!>    @param[in] mxb positions dimension
!>    @return true if passed cx position was not set yet on vector xp
!
logical function ckposf(cx,xp,np,tp,nk,mxb)
  use rd_kinds, only: lrk
  implicit none
!
  integer :: np, tp, nk, mxb
  real(lrk) :: cx, xp
  dimension xp(nk,mxb),np(nk)
!
!     position precision = 1 mm
  real(lrk) :: prec
  parameter (prec = 1e-3_lrk)
  integer :: ii
  logical :: ckpos, rpeqf
!
!     init true for first position always new
  ckpos = .true.
!     check if position was set for type tp
  do ii = 1,np(tp)
!       ckpos is true if cx was not set wihtin precision
    ckpos = .not. rpeqf(cx,xp(tp,ii),prec)
!       ckpos false if cx was already set, exit
    if (.not. ckpos) exit
  end do
!
!     check for add new position
  if (ckpos) then
!       add new position count
    np(tp) = ii
!       hold position set
    xp(tp,ii) = cx
  end if
!
  ckposf = ckpos
!
  return
!
end function ckposf
!
!     ==================================================================
!>    @brief excitations export
!
!>    @param[in] iu output unit handle
!>    @param[in] plt plot output flag
!>    @param[in] txt output text
!>    @param[in] nb total number of unbalances
!>    @param[in] mxb unbalance vector dimension
!>    @param[in,out] cnt output index
!>    @param[in] tlen geometry total length (size)
!>    @param[in] tpf type of excitation force
!>     -1:transient torque,0:unbalance (default),1:concentrated,
!>     2:harmonic torque,3:static torque,4:freq. response,5:dist. force
!>    @param[in] thfr torsion harmonic excitation frequency (rad/s)
!>     or force offset for kind = 1 (m)
!>    @param[in] ndd unbalance position
!>    @param[in] dps dist. force initial,final positions
!
subroutine eexc(iu,plt,txt,nb,mxb,cnt,&
&tlen,tpf,thfr,ndd,dps)
  use rd_kinds, only: lrk
  implicit none
!
  integer :: iu, nb, mxb, cnt, tpf
  real(lrk) :: tlen, ndd, thfr, dps
  dimension ndd(mxb),tpf(mxb),thfr(mxb),dps(mxb,2)
  logical :: plt, txt
!
  integer :: i, j, n, ik, np, dm, nk
  parameter (dm = 6,nk = 8)
  real(lrk) :: cx, ls, de, dx, xg, yg, xp, rextf, rextof
  dimension xg(dm),yg(dm),np(nk),xp(nk,mxb)
  data np/nk*0/,xg/dm*0/,yg/dm*0/
  logical :: ok, ckposf
!
  do i = 1,nk
    do n = 1,mxb
      xp(i,n) = 0
    end do
  end do
  j = 0
  n = 0
!     number excitations
  if (nb .gt. 0) then
    ls = tlen/30
    do i = 1,nb
!         exception
      if (tpf(i) .lt. -1 .or. tpf(1) .gt. 5) cycle
!         position
      if (tpf(i) .ne. 5) then
!           all except dist. force
        cx = ndd(i)
!           radius on position
        de = rextf(cx)
      else
!           dist. force
        j = j+1
!           check bound
        if (j .gt. mxb) cycle
        cx = dps(j,1)
        de = rextof(cx,1)
      end if
!         kind
      if (tpf(i) .eq. -1) then
!           transient torque
        n = 3
        ik = 9
!           check for repeat position and type
        ok = ckposf(cx,xp,np,1,nk,mxb)
        if (ok) call grntrl(cx,de*2,ls,xg,yg,dm)
      else if (tpf(i) .eq. 0) then
!           unbalance
        n = 5
        ik = 6
        ok = ckposf(cx,xp,np,2,nk,mxb)
        if (ok) call grnunb(cx,de*2,ls,xg,yg,dm)
      else if (tpf(i) .eq. 1) then
!           static force
        n = 3
        ik = 7
!           apply offset
        dx = cx+thfr(i)
        ok = ckposf(dx,xp,np,3,nk,mxb)
        if (ok) call grnutr(dx,de*2,ls,xg,yg,dm)
      else if (tpf(i) .eq. 2) then
!           harmonicc torque
        n = 3
        ik = 10
        ok = ckposf(cx,xp,np,4,nk,mxb)
        if (ok) call grntrr(cx,de*2,ls,xg,yg,dm)
      else if (tpf(i) .eq. 3) then
!           static torque -> diamond <>
        n = 4
        ik = 8
        ok = ckposf(cx,xp,np,5,nk,mxb)
        if (ok) call grndia(cx,de*2,ls,xg,yg,dm)
      else if (tpf(i) .eq. 4) then
!           frequency response
        n = 6
        ik = 11
        ok = ckposf(cx,xp,np,6,nk,mxb)
        if (ok) call grntex(cx,de*2,ls,xg,yg,dm)
      else if(tpf(i) .eq. 5) then
!           dist. force
        n = 4
        ik = 15
        ok = ckposf(cx,xp,np,8,nk,mxb)
        if (ok) call grndfr(dps(j,1),dps(j,2),de*2,ls,xg,yg,dm)
!     other kinds of excitation
      end if
!         save excitation geometry points
      if (txt .and. ok) call wgeo(iu,n,dm,xg,yg)
      if (ok) cnt = cnt+1
      if (plt .and. ok) call pgeom(cnt,ik,n,dm,xg,yg)
!
!         check static force
      if (tpf(i) .eq. 1) then
!           check static force offset
        if (thfr(i) .ne. 0) then
          ok = ckposf(cx,xp,np,7,nk,mxb)
          if (ok) call grnrso(cx,thfr(i),de*2,ls,xg,yg,dm)
          if (txt .and. ok) call wgeo(iu,3,dm,xg,yg)
          if (ok) cnt = cnt+1
          if (plt .and. ok) call pgeom(cnt,7,-3,dm,xg,yg)
        end if
      end if
!         exctitations do
    end do
!       number of excitations
  end if
!
  return
!
end subroutine eexc
!
!     ==================================================================
!>    @brief frequency response export
!
!>    @param[in] iu output unit handle
!>    @param[in] plt plot output flag
!>    @param[in] txt output text
!>    @param[in] np total number of frequency responses
!>    @param[in] mxr freq. response,5:dist. force vector dimension
!>    @param[in,out] cnt output index
!>    @param[in] tlen geometry total length (size)
!>    @param[in] pr response position vector
!>    @param[in] desp coordinate vector, 1=horizontal, 2=vertical
!>    @param[in] ori coordinate rotation angle vector (radians)
!>    @param[in] isd true if ori is given in degree
!>    @return number of unbalance response on shaft
!
integer function efrspf(iu,plt,txt,np,mxr,&
&cnt,tlen,pr,desp,ori,isd)
  use rd_kinds, only: lrk
  implicit none
!
  integer :: iu, np, mxr, cnt
  real(lrk) :: tlen, pr, desp, ori
  dimension pr(mxr),desp(mxr),ori(mxr)
  logical :: plt, txt, isd
!
  integer :: i, nu
  real(lrk) :: ls, de, aa, an, rad, riff, rpif, rextf, toradf, xg, yg
  dimension xg(3),yg(3)
  real(lrk) :: prec
  parameter (prec = 1e-12_lrk)
  logical :: a0, rpeqf
!
!     count unbalance response output on shaft,
!      position >= 0, on support < 0.
  nu = 0
  do i = 1,np
    if (.not. pr(i) .lt. 0) nu = nu+1
  end do

!     frequency response
  if (nu .gt. 0) then
!       response size
    ls = tlen/30
    do i = 1,np
!         check for unbalance response on shaft,
!         position >= 0, on support < 0.
      if (.not. pr(i) .lt. 0) then
!           response positions index
!           radius on response position
        de = rextf(pr(i))
!           coodinate angle 1->0, 2->90 deg
        a0 = rpeqf(desp(i),1.0_lrk,prec)
        aa = riff(a0,0._lrk,rpif()/2)
!           check in radian (default) deg -> rad
        rad = riff(.not. isd,ori(i),toradf(ori(i)))
        an = aa+rad
        call grnfrs(pr(i),2*de,ls,an,xg,yg,3)
        if (txt) call wgeo(iu,3,3,xg,yg)
        cnt = cnt + 1
        if (plt) call pgeom(cnt,11,-3,3,xg,yg)
      end if
    end do
  end if
!
  efrspf = nu
!
  return
!
end function efrspf
!
!     ==================================================================
!>    @brief time response export
!
!>    @param[in] iu output unit handle
!>    @param[in] plt plot output flag
!>    @param[in] txt output text
!>    @param[in] no total number of time responses
!>    @param[in] mxo time response vector dimension
!>    @param[in,out] cnt output index
!>    @param[in] tlen geometry total length (size)
!>    @param[in] po response position vector
!>    @param[in] ao response angle vector
!>    @param[in] isr true response angle in radian, false degree
!
subroutine etrsp(iu,plt,txt,no,mxo,cnt,tlen,po,ao,isr)
  use rd_kinds, only: lrk
  implicit none
!
  integer :: iu, no, mxo, cnt
  real(lrk) :: tlen, po, ao
  dimension po(mxo),ao(mxo)
  logical :: plt, txt, isr
!
  integer :: i
  real(lrk) :: ls, de, rad, rextf, toradf, riff, xg, yg
  dimension xg(3),yg(3)
!
!     time response
  if (no .gt. 0) then
!       response size
    ls = tlen/30
    do i = 1,no
!         time response on shaft not support
      if (.not. po(i) .lt. 0) then
!           response positions index
!           radius on response position
        de = rextf(po(i))
!           check in radian (default) deg -> rad
        rad = riff(.not. isr,toradf(ao(i)),ao(i))
        call grntrs(po(i),2*de,ls,rad,xg,yg,3)
        if (txt) call wgeo(iu,3,3,xg,yg)
        cnt = cnt + 1
        if (plt) call pgeom(cnt,11,-3,3,xg,yg)
      end if
    end do
  end if
!
  return
!
end subroutine etrsp
!
!     ==================================================================
!>    @brief concentrated geometry export
!
!>    @param[in] iu output unit handle
!>    @param[in] plt plot output flag
!>    @param[in] txt output text
!>    @param[in] nmic total number of concentrated
!>    @param[in] mxic concentrated vector dimension
!>    @param[in] mts section vector dimension
!>    @param[in,out] cnt output index
!>    @param[in] tlen geometry total length (size)
!>    @param[in] y section positions vector
!>    @param[in] ds sections external diameter
!>    @param[in] psic concentrated position vector
!>    @param[out] errmsg return an error message
!>    @param[out] ok return flag. unsuccessful if <0.
!
subroutine econ(iu,plt,txt,nmic,mxic,mts,cnt,tlen,&
&y,ds,psic,errmsg,ok)
  use rd_kinds, only: lrk
  implicit none
!
  integer :: iu, nmic, mxic, mts, cnt
  real(lrk) :: tlen, y, ds, psic
  dimension y(mts),ds(mts),psic(mxic)
  integer :: ok
  character(len=99) :: errmsg
  logical :: plt, txt
!
  integer :: i, jd, inddis, oki
  real(lrk) :: ls, xg, yg
  dimension xg(6),yg(6)
!
  ok =-1
!     concentrated
  if (nmic .gt. 0) then
!       bearing size
    ls = tlen/30
    do i = 1,nmic
!         concentrated positions index
      jd = inddis(psic(i),errmsg,oki)
      if (oki .lt. 0) return
      call grncon(y(jd),ds(jd),ls,xg,yg,6)
      if (txt) call wgeo(iu,6,6,xg,yg)
      cnt = cnt + 1
      if (plt) call pgeom(cnt,12,6,6,xg,yg)
    end do
  end if
!
  ok = 0
!
  return
!
end subroutine econ
!
!     ==================================================================
!>    @brief export elements.
!>     a list of node coordinate indices. connect nodes with lines.
!>     pass zero on count to omit plot.
!
!>    @param[in] std standard input output
!>    @param[in] nt total number of sections
!>    @param[in] ci number of sections with internal diameter
!>    @param[in] cs total number of sections
!>    @param[in] ncplg total number of torsion couplings
!>    @param[in] nd total number of disks
!>    @param[in] nbrg number of lateral bearings
!>    @param[in] ntbr number of torsion restriction
!>    @param[in] no number of disk offset
!>    @param[in] nb number of unbalances
!>    @param[in] np number of frequency responses
!>    @param[in] ni number of time responses
!>    @param[in] nmic number of concentrated
!>    @param[in] mxd disk vector dimension
!>    @param[in] mxb unbalance vector dimension
!>    @param[in] off_d disk offset vector
!>    @param[in] tpf type on excitation force
!>     -1:transient torque,0:unbalance (default),1:concentrated,
!>     2:harmonic torque,3:static torque,4:freq. response,5:dist. force
!>    @param[in] thfr torsion harmonic excitation frequency (rad/s)
!>     or force offset for kind = 1 (m)
!>    @param[out] errmsg return an error message
!>    @param[out] ok return flag. unsuccessful if <0.
!
subroutine expel(std,nt,ci,cs,ncplg,&
&nd,nbrg,ntbr,no,nb,np,ni,nmic,mxd,mxb,&
&off_d,tpf,thfr,errmsg,ok)
  use rd_textfun, only: fomsgf
  use com_elkind, only: ekid
  use com_sta, only: stm
  use rd_kinds, only: lrk
  implicit none

  integer :: nt, ci, cs, ncplg, nd, nbrg, ntbr, no, nb, np, ni, nmic, mxd, mxb, tpf
  real(lrk) :: off_d, thfr
  dimension off_d(mxd),tpf(mxb),thfr(mxb)
  integer :: ok
  character(len=99) :: errmsg
  logical :: std
!
  integer :: iu, i, j, jd, k, m, nm, nl, nr, ny, ns, nf, ie, iiff
  data nr/0/
!
!     excitation kind counters
  integer :: nnt(-1:5)
  data nnt/7*0/
!
  character(len=5) :: nmm
  character(len=8) :: csc
  dimension nmm(2),csc(2)
  parameter(nmm = (/'expel','stdio'/),&
  &csc=(/'elements','elemkind'/))
  character(len=10) :: sct
  character(len=255) :: ofn
!
!     element kind, see blockd.f
  integer :: ekmx
  parameter (ekmx = 16)
!     1=section,2=division,3=disk,4=lateral bearing,5=torsion restrictio
!     6=unbalance exc,7=static force exc,8=static torque exc,
!     9=transitory torque exc,10=harmonic torque exc,
!     11=frequency response exc,12=frequency response,13=concentrated.
!     0=line,14=time response,15 dist. force
!
!     local element kind id
  integer :: mkd
  parameter (mkd = 999)
  integer :: elkd(mkd)
!
!     stamp
!
  intrinsic :: mod
!
  ok = -1
!
!     elements
!
  if (.not. std) then
    iu = 12
!       open elements output
    call mntfnm(7,m,ofn)
    open(iu,file=ofn(1:m),err=100)
  else
    iu = 6
    m = 5
    ofn = nmm(2)
    sct = csc(1)
    call marksec(sct,0)
  end if
!
!     first line stamp
  write(iu,15,err=200) stm
!
!     element id
  ie = 0
!
!     position offset
  jd = 1
!
!     total divisions, start at zero, remove the first
  ny = iiff(nt .gt. 1,nt-1,0)

!     number of torsion couplings
  nl = 0
!
!     divisions with external diameter (if ny>0)
!     divisions closed - francisco - feb-19
  do i = 1,ny
    write(iu,400,err=200) (j,j=jd,jd+3),jd
    jd = jd+4
!       division element
    ie = ie+1
    if (ie .gt. mkd)call elmsge(1,4,nmm(1))
    elkd(ie) = ekid(2)
  end do
!
!     divisions with internal diameter (if ci>0)
  jd = ny*4+1
  do i = 1,ci
    write(iu,400,err=200) (j,j=jd,jd+3),jd
    jd = jd+4
!       division element
    ie = ie+1
    if (ie .gt. mkd)call elmsge(1,4,nmm(1))
    elkd(ie) = ekid(2)
  end do
!
!     total divisions for offset
  ns = ny+ci
!
!     sections (if cs>0)
  jd = ns*4+1
  do i = 1,cs
    write(iu,400,err=200) (j,j=jd,jd+3),jd
    jd = jd+4
!       section element
    ie = ie+1
    if (ie .gt. mkd)call elmsge(1,4,nmm(1))
    elkd(ie) = ekid(1)
  end do
!
!     torsion couplings
  if (ncplg .gt. 0) then
    nl = ncplg
    jd = ns*4+nl*4+1
    do i = 1,nl
      write(iu,700,err=200) (j,j=jd,jd+1)
      jd = jd+2
!         line element
      ie = ie+1
      if (ie .gt. mkd)call elmsge(1,4,nmm(1))
      elkd(ie) = ekid(14)
      write(iu,700,err=200) (j,j=jd,jd+1)
      jd = jd+2
      ie = ie+1
      if (ie .gt. mkd)call elmsge(1,4,nmm(1))
      elkd(ie) = ekid(14)
    end do
  end if
!
!     total offset,may have:
!     nt=>0,ci>=0 and cs=0 or
!     nt=0,ci=0 and cs>0
  ns = ns+cs
!
!     disks
!     added k disk offset  - francisco - feb-19
  if (nd .gt. 0) then
    k = 0
    jd = ns*4+nl*4+1
    do i = 1,nd*2
      write(iu,400,err=200) (j,j=jd,jd+3),jd
!         disk element
      ie = ie+1
      if (ie .gt. mkd)call elmsge(1,4,nmm(1))
      elkd(ie) = ekid(3)
!         added offset - francisco  - feb-19
      if (mod(i,2) .eq. 0) then
        k = k+1
        if (off_d(k) .ne. 0) then
          jd = jd+4
          write(iu,500,err=200) (j,j=jd,jd+3)
!             offset geometry is a third disk export element
          ie = ie+1
          if (ie .gt. mkd)call elmsge(1,4,nmm(1))
          elkd(ie) = ekid(3)
        end if
      end if
      jd = jd+4
    end do
  end if
!
!     bearings.
  nm = nbrg+ntbr
!     bearing mass < 0, torsion restriction
  if (nm .gt. 0) then
    jd = ns*4+nl*4+nd*8+no*4+1
!       lateral bearing
    do i = 1,nbrg
      write(iu,500,err=200) (j,j=jd,jd+2),jd
      jd = jd+3
!         bearing element
      ie = ie+1
      if (ie .gt. mkd) call elmsge(1,4,nmm(1))
      elkd(ie) = ekid(4)
    end do
!       torsion restriction
    do i = 1,ntbr
      write(iu,500,err=200) (j,j=jd,jd+2),jd
      jd = jd+3
!         bearing element
      ie = ie+1
      if (ie .gt. mkd) call elmsge(1,4,nmm(1))
      elkd(ie) = ekid(5)
    end do
  end if
!
!     excitations
  if (nb .gt. 0) then
    jd = ns*4+nl*4+nd*8+no*4+nm*3+1
!       number of excitations with offset
    nf = 0
    do i = 1,nb
!         except dist. force
      if (tpf(i) .lt. -1 .or. tpf(i) .gt. 5) cycle
!         excitation element
      ie = ie+1
      if (ie .gt. mkd) call elmsge(1,4,nmm(1))
!         kind
      if (tpf(i) .eq. 0) then
!           unbalance
        nnt(0) = nnt(0)+1
        write(iu,300,err=200) (j,j=jd,jd+4),jd
        jd = jd+5
        elkd(ie) = ekid(6)
      else if (tpf(i) .eq. -1 .or. tpf(i) .eq. 1&
      &.or. tpf(i) .eq. 2) then
        if (tpf(i) .eq. -1) then
!             transitory torque
          nnt(-1) = nnt(-1)+1
          elkd(ie) = ekid(9)
        else if (tpf(i) .eq. 1) then
!             static force
          nnt(1) = nnt(1)+1
          elkd(ie) = ekid(7)
        else
!             harmonic torque
          nnt(2) = nnt(2)+1
          elkd(ie) = ekid(10)
        end if
        write(iu,300,err=200) (j,j=jd,jd+2),jd
        jd = jd+3
      else if (tpf(i) .eq. 3) then
!           static torque -> diamond <>
        nnt(3) = nnt(3)+1
        write(iu,400,err=200) (j,j=jd,jd+3),jd
        jd = jd+4
        elkd(ie) = ekid(8)
      else if (tpf(i) .eq. 4) then
!           frequency response
        nnt(4) = nnt(4)+1
        write(iu,300,err=200) (j,j=jd,jd+4),jd
        jd = jd+6
        elkd(ie) = ekid(11)
      else if (tpf(i) .eq. 5) then
!           dist. force
        nnt(5) = nnt(5)+1
        write(iu,300,err=200) (j,j=jd,jd+3),jd
        jd = jd+4
        elkd(ie) = ekid(16)
!     other kinds of excitation
      end if
!
!         check static force
      if (tpf(i) .eq. 1) then
!           check static force with offset
        if (thfr(i) .ne. 0) then
!             static force with offset
          nf = nf+1
          ie = ie+1
          if (ie .gt. mkd) call elmsge(1,4,nmm(1))
          elkd(ie) = ekid(7)
          write(iu,600,err=200) (j,j=jd,jd+2)
          jd = jd+3
        end if
      end if
!         excitations do
    end do
!       total of excitations
    nr = nnt(-1)*3+nnt(0)*5+nnt(1)*3+nnt(2)*3&
    &+nnt(3)*4+nnt(4)*6+nnt(5)*4
!       static offset
    nr = nr+nf*3
  end if
!
!     frequency response
  if (np .gt. 0) then
    jd = ns*4+nl*4+nd*8+no*4+nm*3+nr+1
    do i = 1,np
      write(iu,600,err=200) (j,j=jd,jd+2)
      jd = jd+3
!         frequency response element
      ie = ie+1
      if (ie .gt. mkd)call elmsge(1,4,nmm(1))
      elkd(ie) = ekid(12)
    end do
  end if
!
!     time response
  if (ni .gt. 0) then
    jd = ns*4+nl*4+nd*8+no*4+nm*3+nr+np*3+1
    do i = 1,ni
      write(iu,600,err=200) (j,j=jd,jd+2)
      jd = jd+3
!         time response element
      ie = ie+1
      if (ie .gt. mkd)call elmsge(1,4,nmm(1))
      elkd(ie) = ekid(15)
    end do
  end if
!
!     concentrated
  if (nmic .gt. 0) then
    jd = ns*4+nl*4+nd*8+no*4+nm*3+nr+np*3+ni*3+1
    do i = 1,nmic
      write(iu,250,err=200) (j,j=jd,jd+5),jd
      jd = jd+6
!         concentrated element
      ie = ie+1
      if (ie .gt. mkd)call elmsge(1,4,nmm(1))
      elkd(ie) = ekid(13)
    end do
  end if
!
  write(iu,5)
!
  if (.not. std) then
!       close output file
    close(iu)
!       open element kind output
    call mntfnm(19,m,ofn)
    open(iu,file=ofn(1:m),err=100)
  else
    call marksec(sct,1)
    sct = csc(2)
    call marksec(sct,0)
  end if
!
!     element kind
!
!     first line stamp
  write(iu,15,err=200)stm
  do i = 1,ie
    write(iu,25,err=200)elkd(i)
  end do
  write(iu,5)
!
  if (.not. std) then
!       close output file
    close(iu)
  else
    call marksec(sct,1)
  end if
!
  ok = 0
  return
!
100 errmsg = fomsgf(99, nmm(1),1,ofn,1)
  return
!
200 errmsg = fomsgf(99, nmm(1),13,ofn,1)
  if (.not. std) close(iu)
  return
!
5 format()
15 format(a)
25 format(i2)
250 format(7i6)
300 format(6i6)
400 format(5i6)
500 format(4i6)
600 format(3i6)
700 format(2i6)
!
end subroutine expel
!
!     ==================================================================
!>    @brief geometry nodal and elements coordinates.
!
!>    @param[in] iu output file handle
!>    @param[in] ofn output file name
!>    @param[in] scn previous section block name
!>    @param[in] std standard input output
!>    @param[in] plt HPGL plot flag
!>    @param[in] txt output text, include elements output
!>    @param[in] sec true output sections else divisions
!>    @param[out] errmsg return an error message
!>    @param[out] ok return flag. unsuccessful if <0.
!
subroutine gmexp(iu,ofn,scn,std,plt,txt,sec,errmsg,ok)
  use rd_textfun, only: cadjf, fomsgf
  use com_conc, only: nmic, psic, vlmc, ixic, iyic, izic
  use com_couplings, only: cplgid, cplgst, cplgdp, cplgin, cplgsr, ncplg
  use com_dis, only: pd, d_d, h_d, rho_d, r2, nd
  use com_disa, only: d_i, i_x, i_y, m_d
  use com_doff, only: off_d
  use com_eix, only: l, d, di, ps, e, nu, rho_e, g_e, r1, cs
  use com_man, only: kxx, kxz, kzz, kzx, cxx, cxz, czz, czx, mm, scl => sc
  use com_mdd, only: qtd_modos, porb, rorb, rang
  use com_mdd1, only: ru
  use com_sec, only: n, y, nt, nn
  use com_sed, only: ds, di_d, di_s
  use com_sta, only: stm
  use com_thmfr, only: thfr
  use com_tman, only: ntbr, tpc, tkk, tcc, tjj, sct
  use com_unb0, only: pr, desp, ori, np, nm
  use com_unb1, only: ndd, mu, ed, tpf, nb
  use com_ymc, only: nbrg, rks, pc
  use rd_kinds, only: lrk
  implicit none
!
!     arguments
  integer :: iu, ok
  logical :: std, plt, txt, sec
  character(len=*) :: ofn, scn
  character(len=99) :: errmsg
!
!     locals
!     added k,no offset counter - francisco - feb-19
  integer :: ii, jj, no, nr, ci, cnt, oki, efrspf
  data no/0/
  real(lrk) :: tlen
  logical :: isd, isr
  character(len=3) :: cbl
  character(len=5) :: nmm
  parameter(nmm = 'gmexp')
  character(len=1) :: crd, cbs
  dimension cbs(4)
  parameter (cbs = (/'d','D','r','R'/),cbl = 'mxb')
!
!     parameters
  integer :: mxd, mxm
  parameter (mxd = 99,mxm = 9)
!
!     total number of sections
  integer :: mts
  parameter (mts = 999)
!
!     bearings
!     torsion restriction bearings
!
!     disks
!     added - franciso - feb-19
!
  integer :: mxs
  parameter (mxs = 99)
!
!     shaft data
!
!     all secctions (div)
!
!     section diameters
!
!     unbalance
  integer :: mxb
  parameter (mxb = 99)
!
!     position, value, angle
!     kind of exc
!     -1:transient torque,0:unbalance (default),1:concentrated,
!     2:harmonic torque,3:static torque,4:freq. response,5:dist. force
!     response angle unit 'd' or 'D' for degree, default radian
!     torsion harmonic excitation frequency (rad/s)
!     or force offset for kind = 1 (m)
!
  integer :: mxr
  parameter (mxr = 9)
!
!     frequency response
!
!     time orbit position
!
  integer :: ni, mxo
  parameter (mxo = 1)
  real(lrk) :: po, ao, dp, dps
  dimension po(mxo),ao(mxo),dp(mxb,2),dps(2)
!
!     concentrated masses and inertias
  integer :: mxic
  parameter (mxic = 15)
!
!     torsion couplings
  integer :: mxcplg
  parameter (mxcplg = 20)
!     number of couplings
!     section ids
!     stiffness, damping,inertia,speed ratio
!
!     stamp
!
!     added - francisco - feb-19
  intrinsic :: len_trim, mod
!
!     return init
  ok = -1
!
!     geometry length -> scale purpose
  tlen = y(nt)
!
!     first line stamp
  if (txt) write(iu,5,err=200) stm
!      call outdsc(iu)
!
!     internal diameter count
  ci = 0
!     element count
  cnt = 0
!
  if (sec) then
!       sections
    call esec(iu,plt,txt,cs,mxs,cnt,ps,d)
  else
!       divisions
    call ediv(iu,plt,txt,nt,mts,cnt,ci,y,ds,di_s)
  end if
!
!     torsion couplings
  call etcp(iu,plt,txt,ncplg,cplgid,mxcplg,&
  &mxs,cnt,ps,d)
!
!     disks
  call edsk(iu,plt,txt,nd,mxd,mts,cnt,no,y,ds,&
  &pd,d_d,h_d,d_i,off_d,errmsg,oki)
  if (oki .lt. 0) return
!
!     bearings
  call ebrg(iu,plt,txt,nbrg,ntbr,mxm,mts,cnt,tlen,&
  &y,ds,pc,tpc,errmsg,ok)
  if (oki .lt. 0) return
!
!     excitation
!
!     check dist. force
  jj = 0
  do ii = 1,nb
    if (tpf(ii) .eq. 5) then
      jj = jj+1
!         check position vector bound
      if (jj .gt. mxb) then
!           'out of bounds' !4
        errmsg = fomsgf(99, nmm,4,cbl,1)
        return
      end if
!         get initial,final positions->dps
      call indfd(mxb,mxs,nb,cs,ii,tpf,ndd,ps,dps,errmsg,oki)
      if (oki .lt. 0) return
      dp(jj,1) = dps(1)
      dp(jj,2) = dps(2)
    end if
  end do
!
  call eexc(iu,plt,txt,nb,mxb,cnt,&
  &tlen,tpf,thfr,ndd,dp)
!
!     frequency response
!     check for orient response angle in degree, default radian
  crd = cadjf(1, ru(1),1)
  isd = crd .eq. cbs(1) .or. crd .eq. cbs(2)
  nr = efrspf(iu,plt,txt,np,mxr,&
  &cnt,tlen,pr,desp,ori,isd)
!
!     time response
!     todo signal meaning?
  if (.not. porb .lt. 0) then
    ni = 1
    ao(1) = rang
    po(1) = porb
!       check for time response angle unit, default degree
    crd = cadjf(1, ru(2),1)
    isr = crd .eq. cbs(3) .or. crd .eq. cbs(4)
    call etrsp(iu,plt,txt,ni,mxo,cnt,tlen,po,ao,isr)
  else
    ni = 0
  end if
!
!     concentrated
  call econ(iu,plt,txt,nmic,mxic,mts,cnt,tlen,&
  &y,ds,psic,errmsg,ok)
!
  if (txt) then
!       blank
    write(iu,15,err=200)
!
!       export elements
    if (sec) then
!         sections
      call expel(std,0,0,cs,ncplg,&
      &nd,nbrg,ntbr,no,nb,nr,ni,nmic,mxd,mxb,&
      &off_d,tpf,thfr,errmsg,oki)
    else
!         close previous begin block scn on expgeom
      if (std) call marksec(scn,1)
!         divisions
      call expel(std,nt,ci,0,ncplg,&
      &nd,nbrg,ntbr,no,nb,nr,ni,nmic,mxd,mxb,&
      &off_d,tpf,thfr,errmsg,oki)
    end if
    if (oki .lt. 0) return
  end if
!
!     return ok
  ok = 0
!
  return
!
200 close(iu)
  errmsg = fomsgf(99, nmm,13,ofn,1)
  return
!
5 format(a)
15 format()
!
end subroutine gmexp
!
!     ==================================================================
!>    @brief geometry nodal and elements coordinates.
!
!>    @param[in] std standard input output
!>    @param[in] plt HPGL plot flag
!>    @param[in] cm center of mass
!>    @param[out] errmsg return an error message
!>    @param[out] ok return flag. unsuccessful if <0.
subroutine expgeom(std,plt,cm,errmsg,ok)
  use rd_textfun, only: fomsgf
  use rd_kinds, only: lrk
  implicit none
!
  logical :: std, plt
  real(lrk) :: cm
  integer :: ok
  character(len=99) :: errmsg

  integer :: iu, m
  character(len=5) :: cst
  character(len=7) :: nmm
  character(len=8) :: c8
  character(len=10) :: sct
  character(len=255) :: ofn
  parameter(cst = 'stdio',nmm = 'expgeom',c8 = 'nodcoord')
  data sct/' '/
!
!     nodal coordinates
  if (.not. std) then
    iu = 12
    call mntfnm(6,m,ofn)
!       open output file
    open(iu,file=ofn(1:m),err = 100)
  else
    iu = 6
    ofn = cst
    sct = c8
    call marksec(sct,0)
  end if
!
!     divisions
  call gmexp(iu,ofn,sct,std,plt,.true.,.false.,errmsg,ok)
  if (ok .lt. 0) return
!
  if (.not. std) then
!       close output
    close(iu)
  end if
!
!     plot output
  if (plt) then
!       turn plot grids off
    call gridsoff
    call egeom(cm,std)
!       turn plot grids on
    call gridson
  end if
!
  return
!
100 errmsg = fomsgf(99, nmm,1,ofn,1)
  return
!
end subroutine expgeom
!
