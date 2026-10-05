!     $Id$
!     ==================================================================
!
!>    @file tespmod.f
!>    @brief torsional modal space for eigenvalues
!>    last changes:<br>
!>    new file de-19 - f.
!
!     ==================================================================
!>    @brief torsion modal space matrices eigenvalues calculation.
!>    Using complex lapack routine to avoid to have all double complex
!>    routine members. Only real part is used, meaning does not presents
!>    "right side" eigenvectors and eigenvalues are real and not
!>    complex pairs, besides can be negative, leading to a imaginary
!>    frequency, but this can happens only in free systems with
!>    rigid body mode shape.
!
!>    @param[out] errmsg return a error message
!>    @param[out] ok return flag. unsuccessful if <0.
!
subroutine tespmod(errmsg,ok)
  use rd_textfun, only: fomsgf
  use com_tepm, only: rfi, rlam, ddm
  use com_tmat, only: tj, tk, tc, dm
  use rd_kinds, only: lrk, wp
  implicit none
!
!     arguments
  character(len=99) :: errmsg
  integer :: ok
!
!     maximal number of global matrices elements
  integer :: mtg, mte, nmax, n
  parameter (mtg = 500,mte = mtg,nmax = mte)
!
!     global torsion matrices
!     inertia tj, stiffness tk, damping tc
!
!     locals
  character(len=3) :: cbd
  character(len=8) :: nmm
  parameter (cbd = 'mte',nmm = 'tespmod')
  complex(wp) :: j0, r1
!
  integer :: i, j
!
!     complex matrices
  complex(wp) :: avl, aa, bb, fi, psi
  dimension avl(nmax),aa(nmax,nmax),bb(nmax,nmax),&
  &fi(nmax,nmax),psi(nmax,nmax)
!
!     torsion modal space
!
  intrinsic :: abs, aimag, real, sqrt, cmplx
!
!     initialize return
  ok = -1
  call progress_begin('TORSION_MODAL_SPACE',3,&
  &'assemble and solve torsional eigenproblem')
!
  j0 = cmplx(0._wp,0._wp, kind=wp)
  r1 = cmplx(1._wp,0._wp, kind=wp)
!
!     just stiffness and inertia, damping not
!
!     size of matrices
  n = dm
  if(n .gt. mte) then
    errmsg = fomsgf(99, nmm,4,cbd,0)
    return
  end if
!
!     complete matrices dimension
  ddm = n
!
  call progress_update('TORSION_MODAL_SPACE',1,3,real(dm, lrk),'DOF')
  call progress_stage('TORSION_MODAL_SPACE',&
  &'assemble inertia/stiffness eigenproblem')
!     matrices
  do i = 1,dm
    do j = 1,dm
!         inertia
      aa(i,j) = cmplx(tj(i,j),0, kind=wp)
!         stiffness
      bb(i,j) = cmplx(tk(i,j),0, kind=wp)
    end do
  end do
!
  call progress_update('TORSION_MODAL_SPACE',2,3,real(n, lrk),'EIGEN_DOF')
  call progress_stage('TORSION_MODAL_SPACE','solve eigenproblem')
!     solve adjoint eigenvalues problem ->fi,psi,avl
!     will set return ok
  call adjeig(aa,bb,fi,psi,avl,n,nmax,nmax,.false.,errmsg,ok)
!
  if(ok.ne.0) return
!     Publish RFI only after the complete torsional basis is valid.
!     This is an n-DOF eigenproblem, not a doubled state-space problem.
  call modal_require_range_counts(n,n,0,avl,nmax,errmsg,ok)
  if(ok.ne.0) return
  do j = 1,n
!       eigenvalue (avl already ordered)
    rlam(j) = real(abs(avl(j)), lrk)
    do i = 1,n
!         real eigenvector
      rfi(i,j) = real(real(fi(i,j), wp), lrk)
    end do
  end do
  call progress_update('TORSION_MODAL_SPACE',3,3,real(n, lrk),'MODES')
  call progress_end('TORSION_MODAL_SPACE','OK')
!
  return
!
end subroutine tespmod
!
