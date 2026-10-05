!     $Id$
!     ==================================================================
!
!>    @file calccm.f
!>    @brief calulates the center of mass, last changes<br>
!>    new center of mass calculation - francisco - 11/10/2010<br>
!>    Added disk offset - francisco - feb-19.
!
!     ==================================================================
!>    @brief calulates the center of mass.
!
!>    @param[out] cm return center of mass
!
subroutine calccm(cm)
  use com_conc, only: nmic, psic, vlmc, ixic, iyic, izic
  use com_dis, only: pd, d_d, h_d, rho_d, r2, nd
  use com_disa, only: d_i, i_x, i_y, m_d
  use com_doff, only: off_d
  use com_sea, only: rs, es, gs, dely, s, ie, ump
  use com_sec, only: n, y, nt, nn
  use rd_kinds, only: lrk
!
  implicit none
!
!     arguments
  real(lrk) :: cm
!
!     locals
  integer :: ii
  real(lrk) :: mt, md, vl1, vl2
!
!     input parameters
  integer :: mxd, mts
  parameter (mxd = 99,mts = 999)
!
!     disks
!     added - franciso - feb-19
!
!     masses and concentrated inertias
  integer :: mxic
  parameter (mxic = 15)
!
!     all sections (div)
!
!     additional section data
!
!     init mass and total product
  mt = 0._lrk
  md = 0._lrk
!
!     sections
  do ii = 1,nt-1
!       division length
    vl2 = y(ii+1)-y(ii)
!       mass = length*area*density
    vl1 = vl2*s(ii)*rs(ii)
!       mass sum
    mt=mt+vl1
!       section positions
    vl2 =  y(ii)+y(ii+1)
!       mean divion position
    vl2 = vl2/2._lrk
!       sum of product position*mass
    md = md+(vl1*vl2)
  end do
!
!     disks
  do ii = 1,nd
!       mass sum
    mt = mt+m_d(ii)
!       disk mass position with offset
    vl1 = pd(ii)+off_d(ii)
!       sum of product position*mass
    md = md+(vl1*m_d(ii))
  end do
!
!     concentrated
  do ii = 1,nmic
!       mass sum
    mt = mt+vlmc(ii)
!       sum of product position*mass
    md = md+(psic(ii)*vlmc(ii))
  end do
!
!     center of mass -> (sum of product position*mass)/(mass sum)
  cm = md/mt
!
  return
!
end subroutine calccm
!
