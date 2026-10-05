!     $Id$
!     ==================================================================
!
!>    @file thhmat.f
!>    @brief calculates receptance matrix hh
!>    last changes:<br>
!>    new file feb-20 - f.
!
!>    @brief calculates complex receptance matrix hh
!
!>    @param[in] rad system frequency (rad/s)
!>    @param[in] reye eye matrix [1]
!>    @param[in] tmd modal damping matrix
!>    @param[in] cfi eingenvectors matrix
!>    @param[in] clam engenvalues matrix
!>    @param[out] m1 return complex receptance matrix
!>    @param[in] nmd number of modes to consider
!>    @param[in] mdoff mode offset
!>    @param[in] ddm size of system and output matrices
!>    @param[in] mtg reye,tmd,cfi,clam square matrices dimension
!
subroutine thhmat(rad,reye,tmd,cfi,clam,m1,nmd,mdoff,ddm,mtg)
  use rd_kinds, only: lrk, wp
  implicit none
!
  integer :: nmd, mdoff, ddm, mtg
  real(lrk) :: rad, reye, tmd
!
  complex(wp) :: clam, cfi, m1
  dimension reye(mtg,mtg),tmd(mtg,mtg),cfi(mtg,mtg),clam(mtg,mtg),&
  &m1(mtg,mtg)
!
  real(lrk) :: rd2n
  integer :: nmdt
  complex(wp) :: j0, j1, r1, pmn, m2
  dimension m2(mtg,mtg)
  character(len=1) :: c1
  dimension c1(2)
  parameter (j0 = 0,j1 = (0,1),r1 = (1,0),&
  &c1 = (/'N','T'/))
!
!     -w^2
  rd2n = -1*rad**2
!     -w^2*eye -> m1
  call escmat_rrc(reye,m1,rd2n,ddm,ddm,mtg,mtg)
!
!     j*w*tcp -> m2
  pmn = j1*rad
  call escmat_rcc(tmd,m2,pmn,ddm,ddm,mtg,mtg)
!
!     m1+m2 -> m1
  call sommat_c(m1,m2,m1,ddm,ddm,mtg,mtg)
!
!     m1+clam -> m2
  call sommat_c(m1,clam,m2,ddm,ddm,mtg,mtg)
!
!     invert diagonal m2 -> m1, mode offset
!     starts on line and column 1+mdoff
  call invmatd_ct(m2,m1,ddm,mtg,mdoff)
!
!     subtract mode offset
  nmdt = nmd-mdoff
!
!     following operations are limited
!     by the number of modes -> nmdt
!
!     multiply cfi * m1 -> m2
  call ZGEMM (c1(1),c1(1),ddm,nmdt,nmdt,r1,&
  &cfi,mtg,m1,mtg,j0,m2,mtg)
!
!     multiply m2 * cfi' -> m1
  call ZGEMM (c1(1),c1(2),ddm,ddm,nmdt,r1,&
  &m2,mtg,cfi,mtg,j0,m1,mtg)
!
  return
!
end subroutine thhmat
!
