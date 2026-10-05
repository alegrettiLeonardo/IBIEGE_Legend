module shaft_timoshenko_cowper
  ! self-contained kind: this file must compile alone (-std=f2008),
  ! see tests/test_timoshenko_cowper_element.py
  ! M8.1: lrk promoted to real64, mirroring rd_kinds.f90's own M8.1 change.
  ! Still self-contained (no dependency on rd_kinds), so the isolated-compile
  ! contract keeps holding.
  use, intrinsic :: iso_fortran_env, only: real64
  implicit none
  integer, parameter :: lrk = real64
  private
  public :: cowper_properties, timo_cowper_stiffness, timo_cowper_mass, &
            timo_cowper_gyro, ump_element_matrix
contains

! ============================================================================
! RotorDin - complete cylindrical Timoshenko shaft element with Cowper shear
! correction.  The routines use the historical RotorDin lateral DOF ordering:
!   [x1, z1, theta_x1, theta_z1, x2, z2, theta_x2, theta_z2]
! and preserve the RotorDin gyroscopic sign convention.
!
! The section properties A and I are the effective element properties already
! prepared by PREDAD (mid-point properties for native conical sections).
! ============================================================================

subroutine cowper_properties(e, gs, area, iner, kappa, phi, ok)
  implicit none
  real(lrk), intent(in) :: e, gs, area, iner
  real(lrk), intent(out) :: kappa, phi
  integer, intent(out) :: ok
  real(lrk) :: nu, s2, dd, r2, den

  ok = -1
  kappa = 0.0_lrk
  phi = 0.0_lrk

  if (e <= 0.0_lrk .or. gs <= 0.0_lrk .or. area <= 0.0_lrk .or. iner <= 0.0_lrk) return

  ! PREDAD calculates gs = E/[2(1+nu)], hence this recovers the input nu.
  nu = e/(2.0_lrk*gs) - 1.0_lrk
  if (nu <= -0.999_lrk .or. nu >= 0.5_lrk) return

  ! Reconstruct (Di/Do)^2 from A and I for an annular circular section:
  !   A = pi/4 (Do^2-Di^2), I = pi/64 (Do^4-Di^4)
  ! so Do^2+Di^2 = 16 I/A and Do^2-Di^2 = 4 A/pi.
  s2 = 16.0_lrk*iner/area
  dd = 4.0_lrk*area/acos(-1.0_lrk)
  if (s2 <= 0.0_lrk .or. dd <= 0.0_lrk) return
  r2 = (s2-dd)/(s2+dd)
  ! Round-off at a nominally solid section can make r2 slightly negative.
  r2 = max(0.0_lrk, min(r2, 0.99999994_lrk))

  den = (1.0_lrk+r2)**2*(7.0_lrk+6.0_lrk*nu) + r2*(20.0_lrk+12.0_lrk*nu)
  if (den <= 0.0_lrk) return
  kappa = 6.0_lrk*(1.0_lrk+nu)*(1.0_lrk+r2)**2/den
  if (kappa <= 0.0_lrk) return

  ok = 1
end subroutine cowper_properties

subroutine timo_cowper_stiffness(l, e, gs, area, iner, phi, kappa, ke, ok)
  implicit none
  real(lrk), intent(in) :: l, e, gs, area, iner
  real(lrk), intent(out) :: phi, kappa
  real(lrk), intent(out) :: ke(8,8)
  integer, intent(out) :: ok
  real(lrk) :: l2, fac
  integer :: i, j

  ke = 0.0_lrk
  phi = 0.0_lrk
  kappa = 0.0_lrk
  if (l <= 0.0_lrk) then
    ok = -1
    return
  end if

  call cowper_properties(e,gs,area,iner,kappa,phi,ok)
  if (ok < 0) return
  phi = 12.0_lrk*e*iner/(kappa*gs*area*l*l)
  l2 = l*l
  fac = e*iner/((1.0_lrk+phi)*l*l*l)

  ke(1,1)=12.0_lrk;       ke(1,4)=-6.0_lrk*l;       ke(1,5)=-12.0_lrk;      ke(1,8)=-6.0_lrk*l
  ke(2,2)=12.0_lrk;       ke(2,3)= 6.0_lrk*l;       ke(2,6)=-12.0_lrk;      ke(2,7)= 6.0_lrk*l
  ke(3,2)= 6.0_lrk*l;     ke(3,3)=(4.0_lrk+phi)*l2; ke(3,6)=-6.0_lrk*l;     ke(3,7)=(2.0_lrk-phi)*l2
  ke(4,1)=-6.0_lrk*l;     ke(4,4)=(4.0_lrk+phi)*l2; ke(4,5)= 6.0_lrk*l;     ke(4,8)=(2.0_lrk-phi)*l2
  ke(5,1)=-12.0_lrk;      ke(5,4)= 6.0_lrk*l;       ke(5,5)=12.0_lrk;       ke(5,8)= 6.0_lrk*l
  ke(6,2)=-12.0_lrk;      ke(6,3)=-6.0_lrk*l;       ke(6,6)=12.0_lrk;       ke(6,7)=-6.0_lrk*l
  ke(7,2)= 6.0_lrk*l;     ke(7,3)=(2.0_lrk-phi)*l2; ke(7,6)=-6.0_lrk*l;     ke(7,7)=(4.0_lrk+phi)*l2
  ke(8,1)=-6.0_lrk*l;     ke(8,4)=(2.0_lrk-phi)*l2; ke(8,5)= 6.0_lrk*l;     ke(8,8)=(4.0_lrk+phi)*l2

  do i=1,8
    do j=1,8
      ke(i,j)=fac*ke(i,j)
    end do
  end do
  ok = 1
end subroutine timo_cowper_stiffness

subroutine timo_cowper_mass(l, e, gs, rho, area, iner, me, phi, kappa, ok)
  implicit none
  real(lrk), intent(in) :: l, e, gs, rho, area, iner
  real(lrk), intent(out) :: me(8,8), phi, kappa
  integer, intent(out) :: ok
  real(lrk) :: mt(8,8), mr(8,8)
  real(lrk) :: p2, l2, ft, fr
  real(lrk) :: m1, m2, m3, m4, m5, m7, m11, m12, m13, m14, m15, m16
  integer :: i, j

  me=0.0_lrk; mt=0.0_lrk; mr=0.0_lrk; phi=0.0_lrk; kappa=0.0_lrk
  if (l <= 0.0_lrk .or. rho <= 0.0_lrk) then
    ok=-1
    return
  end if
  call cowper_properties(e,gs,area,iner,kappa,phi,ok)
  if (ok < 0) return
  phi = 12.0_lrk*e*iner/(kappa*gs*area*l*l)
  p2=phi*phi; l2=l*l

  ! Translational consistent Timoshenko mass, uniform cylindrical element.
  m1 = 468.0_lrk + 882.0_lrk*phi + 420.0_lrk*p2
  m2 = (66.0_lrk + 115.5_lrk*phi + 52.5_lrk*p2)*l
  m3 = 162.0_lrk + 378.0_lrk*phi + 210.0_lrk*p2
  m4 = (39.0_lrk + 94.5_lrk*phi + 52.5_lrk*p2)*l
  m5 = (12.0_lrk + 21.0_lrk*phi + 10.5_lrk*p2)*l2
  m7 = (9.0_lrk + 21.0_lrk*phi + 10.5_lrk*p2)*l2

  mt(1,1)=m1;  mt(1,4)=-m2; mt(1,5)=m3;  mt(1,8)= m4
  mt(2,2)=m1;  mt(2,3)= m2; mt(2,6)=m3;  mt(2,7)=-m4
  mt(3,2)=m2;  mt(3,3)= m5; mt(3,6)=m4;  mt(3,7)=-m7
  mt(4,1)=-m2; mt(4,4)= m5; mt(4,5)=-m4; mt(4,8)=-m7
  mt(5,1)=m3;  mt(5,4)=-m4; mt(5,5)=m1;  mt(5,8)= m2
  mt(6,2)=m3;  mt(6,3)= m4; mt(6,6)=m1;  mt(6,7)=-m2
  mt(7,2)=-m4; mt(7,3)=-m7; mt(7,6)=-m2; mt(7,7)= m5
  mt(8,1)=m4;  mt(8,4)=-m7; mt(8,5)=m2;  mt(8,8)= m5
  ft = rho*area*l/(1260.0_lrk*(1.0_lrk+phi)**2)

  ! Rotary-inertia contribution, also dependent on Phi.
  m11 = 252.0_lrk
  m12 = (21.0_lrk-105.0_lrk*phi)*l
  m13 = (21.0_lrk-105.0_lrk*phi)*l
  m14 = (28.0_lrk+35.0_lrk*phi+70.0_lrk*p2)*l2
  m15 = (7.0_lrk+35.0_lrk*phi-35.0_lrk*p2)*l2
  m16 = m14

  mr(1,1)=m11;  mr(1,4)=-m12; mr(1,5)=-m11; mr(1,8)=-m13
  mr(2,2)=m11;  mr(2,3)= m12; mr(2,6)=-m11; mr(2,7)= m13
  mr(3,2)=m12;  mr(3,3)= m14; mr(3,6)=-m12; mr(3,7)=-m15
  mr(4,1)=-m12; mr(4,4)= m14; mr(4,5)= m12; mr(4,8)=-m15
  mr(5,1)=-m11; mr(5,4)= m12; mr(5,5)= m11; mr(5,8)= m13
  mr(6,2)=-m11; mr(6,3)=-m12; mr(6,6)= m11; mr(6,7)=-m13
  mr(7,2)=m13;  mr(7,3)=-m15; mr(7,6)=-m13; mr(7,7)= m16
  mr(8,1)=-m13; mr(8,4)=-m15; mr(8,5)= m13; mr(8,8)= m16
  fr = rho*iner/(210.0_lrk*l*(1.0_lrk+phi)**2)

  do i=1,8
    do j=1,8
      me(i,j)=ft*mt(i,j)+fr*mr(i,j)
    end do
  end do
  ok=1
end subroutine timo_cowper_mass

subroutine timo_cowper_gyro(l, e, gs, rho, area, iner, gf, ge, phi, kappa, ok)
  implicit none
  real(lrk), intent(in) :: l, e, gs, rho, area, iner, gf
  real(lrk), intent(out) :: ge(8,8), phi, kappa
  integer, intent(out) :: ok
  real(lrk) :: p2, l2, fac, g1, g2, g3, g4, g5, g6
  integer :: i, j

  ge=0.0_lrk; phi=0.0_lrk; kappa=0.0_lrk
  if (l <= 0.0_lrk .or. rho <= 0.0_lrk) then
    ok=-1
    return
  end if
  call cowper_properties(e,gs,area,iner,kappa,phi,ok)
  if (ok < 0) return
  phi = 12.0_lrk*e*iner/(kappa*gs*area*l*l)
  p2=phi*phi; l2=l*l

  g1=252.0_lrk
  g2=(21.0_lrk-105.0_lrk*phi)*l
  g3=(21.0_lrk-105.0_lrk*phi)*l
  g4=(28.0_lrk+35.0_lrk*phi+70.0_lrk*p2)*l2
  g5=(7.0_lrk+35.0_lrk*phi-35.0_lrk*p2)*l2
  g6=g4

  ! RotorDin sign convention. At Phi=0 and gf=1 this is exactly legacy COEGIR.
  ge(1,2)=-g1; ge(1,3)=-g2; ge(1,6)= g1; ge(1,7)=-g3
  ge(2,1)= g1; ge(2,4)=-g2; ge(2,5)=-g1; ge(2,8)=-g3
  ge(3,1)= g2; ge(3,4)=-g4; ge(3,5)=-g2; ge(3,8)= g5
  ge(4,2)= g2; ge(4,3)= g4; ge(4,6)=-g2; ge(4,7)=-g5
  ge(5,2)= g1; ge(5,3)= g2; ge(5,6)=-g1; ge(5,7)= g3
  ge(6,1)=-g1; ge(6,4)= g2; ge(6,5)= g1; ge(6,8)= g3
  ge(7,1)= g3; ge(7,4)= g5; ge(7,5)=-g3; ge(7,8)=-g6
  ge(8,2)= g3; ge(8,3)=-g5; ge(8,6)=-g3; ge(8,7)= g6

  fac = gf*rho*iner/(105.0_lrk*l*(1.0_lrk+phi)**2)
  do i=1,8
    do j=1,8
      ge(i,j)=fac*ge(i,j)
    end do
  end do
  ok=1
end subroutine timo_cowper_gyro

subroutine ump_element_matrix(l, ump, ku)
  implicit none
  real(lrk), intent(in) :: l, ump
  real(lrk), intent(out) :: ku(8,8)
  real(lrk) :: l2, fac
  integer :: i, j

  ku=0.0_lrk
  l2=l*l
  ! Preserve the corrected distributed translational UMP interpolation
  ! independently of the physical shaft mass formulation.
  ! ump is k' [N/m^2]; fac=ump*l therefore has [N/m], matching the
  ! translational stiffness terms after Hermite interpolation.
  ku(1,1)=156.0_lrk; ku(1,4)=-22.0_lrk*l; ku(1,5)=54.0_lrk;  ku(1,8)=13.0_lrk*l
  ku(2,2)=156.0_lrk; ku(2,3)= 22.0_lrk*l; ku(2,6)=54.0_lrk;  ku(2,7)=-13.0_lrk*l
  ku(3,2)=22.0_lrk*l;ku(3,3)=4.0_lrk*l2;  ku(3,6)=13.0_lrk*l;ku(3,7)=-3.0_lrk*l2
  ku(4,1)=-22.0_lrk*l;ku(4,4)=4.0_lrk*l2;ku(4,5)=-13.0_lrk*l;ku(4,8)=-3.0_lrk*l2
  ku(5,1)=54.0_lrk; ku(5,4)=-13.0_lrk*l;ku(5,5)=156.0_lrk; ku(5,8)=22.0_lrk*l
  ku(6,2)=54.0_lrk; ku(6,3)=13.0_lrk*l; ku(6,6)=156.0_lrk; ku(6,7)=-22.0_lrk*l
  ku(7,2)=-13.0_lrk*l;ku(7,3)=-3.0_lrk*l2;ku(7,6)=-22.0_lrk*l;ku(7,7)=4.0_lrk*l2
  ku(8,1)=13.0_lrk*l;ku(8,4)=-3.0_lrk*l2;ku(8,5)=22.0_lrk*l;ku(8,8)=4.0_lrk*l2
  fac=ump*l/420.0_lrk
  do i=1,8
    do j=1,8
      ku(i,j)=fac*ku(i,j)
    end do
  end do
end subroutine ump_element_matrix

end module shaft_timoshenko_cowper
