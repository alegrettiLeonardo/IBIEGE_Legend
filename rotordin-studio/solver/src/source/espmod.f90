!     $Id$
!     ==================================================================
!
!>    @file espmod.f
!>    @brief complex modal space, last changes:<br>
!>    espmod variable parameter matrices - francisco - dec-2008<br>
!>    espmod bearing + support complex parameters - francisco - mar-2010
!>    changed frequencies calculation in espmod - francisco - mar-2010<b
!>    removed final ordering in espmod - francisco - mar-2010<br>
!>    changed eespmod, added parameter pp - francisco - nov-2010<br>
!>    moved matrix HH assembly to paramnv - francisco - dec-2010<br>
!>    added complex constants for zero(j) and one(j) - francisco - apr-1
!>    changed name from naoadj to adjeig, added eigenvector dimension
!>    and eigenvector calculation flag - francisco - jun-20<br>
!>    added eigenvalues mode check functio chklamfn - francisco - jun-20
!>    updated espmod get nat. freq with chklamf on espmod - francisco -
!
!     ==================================================================
!>    @brief get eigenvalues column index for a given mode index.
!>     eigenvectors should be already ordered.
!
!>    \verbatim
!>    It is possible with complex matrices, that one eigenvalue does
!>    not represent a natural frequency. In this case the solution does
!>    not present pairs of complex and conjugate complex as expected,
!>    instead, presents large real part values and very small (if not
!>    zero) imaginary part. In this cases, the eigenvalues must be
!>    discarded as it correspondent eigenvector, that does not can be
!>    used to calculate a vibration mode.
!>    Please check available documentation. 17-SEP-2020 - FJDF.
!>    \endverbatim
!
!>    @param[in] lam eigenvalues vector ordered
!>    @param[in] imd mode index
!>    @param[in] tt size of eigenvalues vector
!>    @param[in] nmax eigenvalues vector dimension
!>    @return index of desired mode or -1 if error
!
integer function chklamf(lam,imd,tt,nmax)
  use rd_kinds, only: wp
  implicit none
!
  integer :: imd, tt, nmax
  complex(wp) :: lam
  dimension lam(nmax)
!
  real(wp) :: wn, qs, dprec, tinywn
  integer :: ii, jj, nscan
!     reject numerically real/zero roots; count only the positive-imagin
!     representative of each physical conjugate pair. This removes the
!     historical assumption that conjugates are adjacent after sorting.
  parameter (dprec = 5e-4_wp,tinywn = 1e-12_wp)
  intrinsic :: abs, aimag, min, real
!
  chklamf = -1
  if (imd .le. 0 .or. tt .le. 0 .or. nmax .le. 0) return
  nscan = min(tt,nmax)
  jj = 0
  do ii = 1,nscan
    wn = abs(lam(ii))
    if (wn .gt. tinywn) then
      qs = real(lam(ii), wp)/wn
      if (aimag(lam(ii)) .gt. 0._wp .and.1._wp-qs**2 .gt. dprec) then
        jj = jj+1
        if (jj .eq. imd) then
          chklamf = ii
          return
        end if
      end if
    end if
  end do
!
  return
!
end function chklamf
!
!     ==================================================================
!>    @brief solve complex adjoint (non-symmetric) eigenvalues problem.
!
!>    @param[in] aa A matrix
!>    @param[in] bb B matrix
!>    @param[out] fi return ordered right eigenvectores (ortho-normalize
!>    @param[out] psi return ordered left eigenvectores (ortho-normalize
!>    @param[out] avl return ordered eigenvalues
!>    @param[in] n eigenvalues problem size
!>    @param[in] fpdm dimension of fi and psi matrices
!>     1 when NOT to calculate eigenvectors (novec = true) and nmax othe
!>    @param[in] nmax dimension of eigenvalues aa, bb, avl matrices
!>    @param[in] novec if true fi and psi eigenvectors will NOT be calcu
!>    @param[out] errmsg return an error message
!>    @param[out] ok return flag. unsuccessful if <0.
!
subroutine adjeig(aa,bb,fi,psi,avl,n,fpdm,nmax,novec,errmsg,ok)
  use rd_textfun, only: fomsgf
  use com_epmq, only: modal_condition, modal_valid
  use rd_kinds, only: wp
  use rd_modal_metrics, only: generalized_quotient,record_generalized_roots, &
    eigen_root_status,root_finite
  implicit none
!     Shared modal-conditioning diagnostics.
!     Keep this include fixed-form compatible.
  integer :: mtq
  parameter (mtq = 1000)
!
!     Scale-invariant biorthogonal conditioning floor.
  real(wp) :: modal_condition_min
  parameter (modal_condition_min = 1e-10_wp)
!
!     arguments
  character(len=99) :: errmsg
  integer :: n, ok, nmax, fpdm
  complex(wp) :: aa, bb, fi, psi, avl
  dimension aa(nmax,nmax),bb(nmax,nmax),&
  &fi(fpdm,fpdm),psi(fpdm,fpdm),avl(nmax)
  logical :: novec
!
!     locals
  integer :: i, j, k, l, root_status
  character(len=1) :: c1
  character(len=3) :: erc
  character(len=6) :: nmm
  complex(wp) :: j0, r1, cs
  complex(wp) :: aphi
  real(wp) :: pnorm, anorm, eta, dznrm2
  logical :: pairok, real_pencil
  dimension c1(4),aphi(nmax)
  parameter (c1 = (/'B','V','N','C'/),nmm = 'adjeig',&
  &j0 = (0,0),r1 = (1,0))
!     error messages
!
!     eigenvalues
  integer :: IHI, ILO, INFO
  real(wp) :: ABNRM, BBNRM
!     array Arguments ..
  logical :: BWORK( 1 )
  integer :: IWORK( NMAX+2 )
  real(wp) :: LSCALE( NMAX ), RCONDE( 1 ), RCONDV( 1 ), RSCALE( NMAX), RWORK( 6*NMAX )
!     work dim
  complex(wp) :: ALPHA( NMAX ), BETA( NMAX ), WORK( 4 * NMAX )
!
!     dim locals
  integer :: idx
  complex(wp) :: aat, bbt, mji
  dimension idx(nmax),aat(nmax,nmax),bbt(nmax,nmax),mji(fpdm)
!
!     dcmplx -> double complex intrinsic
  intrinsic :: abs, aimag, huge, sqrt, tiny, cmplx, real
!
!     init return
  ok = -1
  do i = 1,min(n,mtq)
    modal_condition(i) = 1._wp
    modal_valid(i) = .true.
  end do
!
!     copy matrix aa -> aat
  call copmat_c(aa,aat,n,nmax,nmax)
!     copy matrix bb -> bbt
  call copmat_c(bb,bbt,n,nmax,nmax)
!
!  -- LAPACK driver routine (version 3.0) --
!     Univ. of Tennessee, Univ. of California Berkeley, NAG Ltd.,
!     Courant Institute, Argonne National Lab, and Rice University
!     June 30, 1999
!
!     HF02: retain the structure of an EXACTLY REAL pencil. Complex QZ may
!     leave a roundoff imaginary part on a real pole; frequency seeding must
!     not turn that artifact into a new low-frequency oscillatory branch.
!     No tolerance discards a physical imaginary matrix coefficient here.
  real_pencil=all(aimag(aa(1:n,1:n))==0._wp).and.all(aimag(bb(1:n,1:n))==0._wp)
  if (real_pencil) then
    call solve_real_pencil(info)
  else if (.not. novec) then
!       calculate eigenvectors
!       check fi and psi dimension
    if (fpdm .ne. nmax) then
!         error, invalid data
      call elmsge(1,8,nmm)
    end if
!        CALL ZGGEVX('Balance','Vectors (left)','Vectors (right)',
    CALL ZGGEVX(c1(1),c1(2),c1(2),&
!     & 'No reciprocal condition numbers',N,BB,NMAX,AA,
    &c1(3),N,BB,NMAX,AA,&
    &NMAX,ALPHA,BETA,PSI,fpdm,FI,fpdm,ILO,IHI,&
    &LSCALE, RSCALE, ABNRM,BBNRM,RCONDE,RCONDV,&
    &WORK,4*NMAX,RWORK,IWORK,BWORK,INFO)
!       attention! psi return * (conjugate)
!
  else
!       not calculate eigenvectors
!        CALL ZGGEVX('Balance','No vectors (left)','No Vectors (right)',
    CALL ZGGEVX(c1(1),c1(3),c1(3),&
!     & 'No reciprocal condition numbers',N,BB,NMAX,AA,
    &c1(3),N,BB,NMAX,AA,&
    &NMAX,ALPHA,BETA,PSI,fpdm,FI,fpdm,ILO,IHI,&
    &LSCALE,RSCALE,ABNRM,BBNRM,RCONDE,RCONDV,&
    &WORK,max(1,4*NMAX),RWORK,IWORK,BWORK,INFO)
!
  end if
!
!     check lapack info
  if(info .ne. 0) then
!       lapack fail error number -> erc
    write(erc,5) info
    errmsg = fomsgf(99, nmm,12,erc,0)
    return
  end if
!
!     copy back matrix aat -> aa
  call copmat_c(aat,aa,n,nmax,nmax)
  call copmat_c(bbt,bb,n,nmax,nmax)
!
!     lambda complex vector (eigenvalues).  Infinite/undefined
!     generalized roots are represented by a large real sentinel so
!     chklamf cannot misclassify them as oscillatory modes.
  do i = 1,n
    call generalized_quotient(alpha(i),beta(i),avl(i),root_status)
  end do
!
!     order abs eigenvlaues avl -> avl
  call ordena_c(avl,avl,idx,n,nmax,0)
  call record_generalized_roots(alpha(1:n),beta(1:n),idx(1:n),n)
!
!     check for eigenvectors
  if (.not. novec) then
!
!       calculated eigenvectors
!
!       multiply psi' * aat -> bbt  (C -> unconjugate psi)
!        call ZGEMM ('C','N', N, N, N, r1, psi, nmax, aat, nmax,
    call ZGEMM (c1(4),c1(3), N, N, N, r1, psi, nmax, aat, nmax,&
    &j0, bbt, nmax )
!
!       multiply bbt * fi -> aat (mj)
!        call ZGEMM ('N','N', N, N, N, r1, bbt, nmax, fi, nmax,
    call ZGEMM (c1(3),c1(3), N, N, N, r1, bbt, nmax, fi, nmax,&
    &j0, aat, nmax )
!
!       get the diagonal (psi'*aa*fi) aat -> mji
    call diag_c(aat,mji,n,nmax)
!
!       F01: snapshot ALL original LAPACK columns before applying IDX.
!       In-place destination writes can otherwise destroy a source column
!       needed by a later permutation cycle, including its eta diagnostic.
!       AAT/BBT are no longer needed for the pseudo-mass products above.
    call copmat_c(fi,aat,n,nmax,nmax)
    call copmat_c(psi,bbt,n,nmax,nmax)
!
!       LAPACK returns VL/VR.  Stored PSI is unconjugated below, so
!       downstream PSI**T is the mathematical VL**H.  Conditioning is
!       therefore measured with the same conjugate-transpose product:
!       eta=|VL**H*A*VR|/(||VL||_2*||A*VR||_2).
!       Invalid pairs are never divided by sqrt(pseudo-mass).
    do j = 1,n
      k = idx(j)
      do i = 1,n
        aphi(i) = j0
        do l = 1,n
          aphi(i) = aphi(i)+aa(i,l)*aat(l,k)
        end do
      end do
      pnorm = dznrm2(n,bbt(1,k),1)
      anorm = dznrm2(n,aphi,1)
      eta = 0._wp
      pairok = pnorm .gt. 0._wp .and. anorm .gt. 0._wp
      pairok = pairok .and. eigen_root_status(j)==root_finite
      pairok = pairok .and. pnorm .eq. pnorm
      pairok = pairok .and. anorm .eq. anorm
      pairok = pairok .and. pnorm .le. huge(1._wp)
      pairok = pairok .and. anorm .le. huge(1._wp)
      pairok = pairok .and. abs(mji(k)) .eq. abs(mji(k))
      pairok = pairok .and. abs(mji(k)) .le. huge(1._wp)
      if (pairok) then
        eta = (abs(mji(k))/pnorm)/anorm
        pairok = eta .eq. eta
        pairok = pairok .and. eta .le. huge(1._wp)
        pairok = pairok .and. eta .ge. modal_condition_min
      end if
      if (j .le. mtq) then
        modal_condition(j) = eta
        modal_valid(j) = pairok
      end if
!
      if (pairok) then
        cs = sqrt(mji(k))
        pairok = abs(cs) .gt. tiny(1._wp)
        pairok = pairok .and. abs(cs) .eq. abs(cs)
        pairok = pairok .and. abs(cs) .le. huge(1._wp)
      end if
      if (j .le. mtq) modal_valid(j) = pairok
!
      if (pairok) then
        do i = 1,n
          fi(i,j) = aat(i,k)/cs
!           unconjugate original LAPACK VL so PSI**T == VL**H
          psi(i,j) = cmplx(real(bbt(i,k), wp),-aimag(bbt(i,k)), kind=wp)/cs
        end do
      else
        do i = 1,n
          fi(i,j) = j0
          psi(i,j) = j0
        end do
      end if
    end do
!
  end if
!
!     return ok
  ok = 0
!
  return
!
5 format(i3)
!
contains
  subroutine solve_real_pencil(ierr)
    ! DGGEVX solves the same ordered pencil (BB,AA), with BALANC='B',
    ! SENSE='N'. Decode LAPACK's real storage BEFORE the common IDX/snapshot
    ! path. ALPHAI=0 identifies a real root; no damping cutoff is introduced.
    use, intrinsic :: ieee_arithmetic, only: ieee_is_finite
    integer, intent(out) :: ierr
    real(wp), allocatable :: ar(:,:),br(:,:),alphar(:),alphai(:),betar(:)
    real(wp), allocatable :: vl(:,:),vr(:,:),workr(:),leftscale(:),rightscale(:)
    real(wp) :: query(1),norma,normb,conde(1),condv(1)
    integer, allocatable :: iw(:)
    logical, allocatable :: bw(:)
    integer :: ldv,ncv,ilo_r,ihi_r,lwork_r,jj
    character(len=1) :: jobv
    ierr=0; ldv=1; ncv=1; jobv='N'
    if (.not.novec) then
      if (fpdm/=nmax) then
        ierr=-91; return
      end if
      ldv=n; ncv=n; jobv='V'
    end if
    allocate(ar(n,n),br(n,n),alphar(n),alphai(n),betar(n))
    allocate(vl(ldv,ncv),vr(ldv,ncv),leftscale(n),rightscale(n),iw(n+6),bw(n))
    ar=real(bb(1:n,1:n),wp); br=real(aa(1:n,1:n),wp)
    call dggevx('B',jobv,jobv,'N',n,ar,n,br,n,alphar,alphai,betar, &
      vl,ldv,vr,ldv,ilo_r,ihi_r,leftscale,rightscale,norma,normb,conde,condv, &
      query,-1,iw,bw,ierr)
    if (ierr/=0) return
    if (.not.ieee_is_finite(query(1))) then
      ierr=-92; return
    end if
    if (query(1)<1._wp.or.query(1)>=real(huge(lwork_r),wp)) then
      ierr=-92; return
    end if
    lwork_r=max(8*n,ceiling(query(1)))
    allocate(workr(lwork_r))
    ar=real(bb(1:n,1:n),wp); br=real(aa(1:n,1:n),wp)
    call dggevx('B',jobv,jobv,'N',n,ar,n,br,n,alphar,alphai,betar, &
      vl,ldv,vr,ldv,ilo_r,ihi_r,leftscale,rightscale,norma,normb,conde,condv, &
      workr,lwork_r,iw,bw,ierr)
    if (ierr/=0) return
    alpha(1:n)=cmplx(alphar,alphai,wp); beta(1:n)=cmplx(betar,0._wp,wp)
    if (.not.novec) then
      jj=1
      do while (jj<=n)
        if (alphai(jj)==0._wp) then
          fi(1:n,jj)=cmplx(vr(:,jj),0._wp,wp)
          psi(1:n,jj)=cmplx(vl(:,jj),0._wp,wp)
          jj=jj+1
        else
          ! Test the bounds before accessing jj+1 (no short-circuit assumed).
          if (jj>=n) then
            ierr=-93; return
          end if
          if (alphai(jj)<=0._wp.or.alphai(jj+1)>=0._wp) then
            ierr=-93; return
          end if
          fi(1:n,jj)=cmplx(vr(:,jj),vr(:,jj+1),wp)
          psi(1:n,jj)=cmplx(vl(:,jj),vl(:,jj+1),wp)
          fi(1:n,jj+1)=conjg(fi(1:n,jj))
          psi(1:n,jj+1)=conjg(psi(1:n,jj))
          jj=jj+2
        end if
      end do
    end if
    write(*,'(a,1x,i0)') 'RD_AUDIT_V1 EIGENSOLVER DGGEVX_REAL',n
  end subroutine solve_real_pencil
!
end subroutine adjeig
!
!     ==================================================================
!>    @brief adjoint eigenvalues problem without eigenvectors.
!
!>    @param[in] aa A matrix
!>    @param[in] bb B matrix
!>    @param[out] avl return eigenvalues
!>    @param[in] n eigenvalues problem size
!>    @param[in] nmax dimension of eigenvalues aa, bb, avl matrices
!>    @param[out] errmsg return an error message
!>    @param[out] ok return flag. unsuccessful if <0.
!
subroutine adjeig2(aa,bb,avl,n,nmax,errmsg,ok)
  use rd_kinds, only: wp
  implicit none
!
!     arguments
  character(len=99) :: errmsg
  integer :: n, ok, nmax
  complex(wp) :: aa, bb, avl
  dimension aa(nmax,nmax),bb(nmax,nmax),avl(nmax)
!
  integer :: fpdm
  parameter (fpdm = 1)
  complex(wp) :: fi, psi
  dimension fi(fpdm,fpdm),psi(fpdm,fpdm)
!
  call adjeig(aa,bb,fi,psi,avl,n,fpdm,nmax,.true.,errmsg,ok)
!
  return
!
end subroutine adjeig2
!
!     ==================================================================
!>    @brief fail closed when a requested ordered eigenpair is invalid.
!
subroutine modal_require_pair(idx,nreq,avl,nmax,errmsg,ok)
  use rd_kinds, only: wp
  implicit none
  integer :: idx, nreq, nmax, ok
  character(len=99) :: errmsg
  complex(wp) :: avl(nmax)
!     Compatibility entry: nreq counts state eigenpairs, not modes.
  call modal_require_pair_counts(idx,0,nreq,avl,nmax,errmsg,ok)
end subroutine modal_require_pair
!
!     Counts <= 0 mean unspecified/not applicable, never inferred.
subroutine modal_require_pair_counts(idx,nphysical,nstate,avl,&
&nmax,errmsg,ok)
  use com_epmq, only: modal_condition, modal_valid
  use rd_kinds, only: wp
  implicit none
!     Shared modal-conditioning diagnostics.
!     Keep this include fixed-form compatible.
  integer :: mtq
  parameter (mtq = 1000)
!
!     Scale-invariant biorthogonal conditioning floor.
  real(wp) :: modal_condition_min
  parameter (modal_condition_min = 1e-10_wp)
  integer :: idx, nphysical, nstate, nmax, ok
  character(len=99) :: errmsg
  complex(wp) :: avl(nmax)
  intrinsic :: abs
!
  ok = -1
  if (idx .lt. 1 .or. idx .gt. nmax .or. idx .gt. mtq) then
    errmsg = 'ERROR modal eigenpair index outside diagnostic range'
    return
  end if
  if (.not. modal_valid(idx)) then
!       Keep errmsg within its historical 99-character contract.
!       Full numeric context goes to stdout, also captured by the runner
    write(errmsg,10) idx
    write(*,'(a)') '[MODAL_SPACE] ill-conditioned modal eigenpair'
    if (nphysical .gt. 0) then
      write(*,'(a,i0)') 'Requested physical modes .... ',nphysical
    else
      write(*,'(a)')&
      &'Requested physical modes .... not specified/applicable'
    end if
    if (nstate .gt. 0) then
      write(*,'(a,i0)') 'Requested state eigenpairs .. ',nstate
    else
      write(*,'(a)')&
      &'Requested state eigenpairs .. not specified/applicable'
    end if
    write(*,'(a,i0)') 'First invalid eigenpair ..... ',idx
    write(*,'(a,es24.16e3)')&
    &'Condition indicator ......... ',modal_condition(idx)
    write(*,'(a,es24.16e3)')&
    &'Minimum accepted ............ ',modal_condition_min
    write(*,'(a,es24.16e3)')&
    &'Eigenvalue |lambda| ......... ',abs(avl(idx))
    return
  end if
  ok = 0
  return
!
10 format('ERROR ill-conditioned modal eigenpair ',i0,&
  &'; see MODAL_SPACE diagnostic')
end subroutine modal_require_pair_counts
!
!     ==================================================================
!>    @brief require every state eigenpair used by a modal truncation.
!
subroutine modal_require_range(nreq,avl,nmax,errmsg,ok)
  use rd_kinds, only: wp
  implicit none
  integer :: nreq, nmax, ok
  character(len=99) :: errmsg
  complex(wp) :: avl(nmax)
  call modal_require_range_counts(nreq,0,nreq,avl,nmax,errmsg,ok)
end subroutine modal_require_range
!
!     nreq is the number of columns checked; reported counts are separat
subroutine modal_require_range_counts(nreq,nphysical,nstate,avl,&
&nmax,errmsg,ok)
  use rd_kinds, only: wp
  implicit none
  integer :: nreq, nphysical, nstate, nmax, ok, j, oki
  character(len=99) :: errmsg
  complex(wp) :: avl(nmax)
!
  ok = -1
  if (nreq .lt. 1 .or. nreq .gt. nmax) then
    errmsg = 'ERROR requested modal order outside state-space range'
    return
  end if
  do j = 1,nreq
    call modal_require_pair_counts(j,nphysical,nstate,avl,nmax,&
    &errmsg,oki)
    if (oki .lt. 0) return
  end do
  ok = 0
  return
end subroutine modal_require_range_counts
!
!     ==================================================================
!>    @brief complex Modal Assurance Criterion for two eigenvectors.
!
real(wp) function macvec(u,v,n,nmax)
  use rd_kinds, only: wp
  implicit none
!
  integer :: n, nmax, i
  complex(wp) :: u, v, uv, uu, vv
  real(wp) :: den
  dimension u(nmax),v(nmax)
!
  intrinsic :: abs, conjg, real
!
  uv = (0._wp,0._wp)
  uu = (0._wp,0._wp)
  vv = (0._wp,0._wp)
  do i = 1,n
    uv = uv+conjg(u(i))*v(i)
    uu = uu+conjg(u(i))*u(i)
    vv = vv+conjg(v(i))*v(i)
  end do
!
  den = real(uu, wp)*real(vv, wp)
  if (den .gt. 1e-30_wp) then
    macvec = abs(uv)**2/den
  else
    macvec = 0._wp
  end if
!
  return
end function macvec
!
!     ==================================================================
!>    @brief preserve modal identity by complex-MAC continuation.
!
!>    The current eigensolution is matched one-to-one against pvec.
!>    first=true seeds the reference using the positive-imaginary member
!>    of each conjugate pair. qmac returns the accepted MAC per mode.
!
subroutine mactrack(fi,avl,pvec,sel,qmac,n,nmax,nmode,ncand,mxc,mxa,first,errmsg,ok)
  use rd_kinds, only: wp
  use com_mat, only: mm
  use com_epmq, only: modal_valid
  use rd_modal_tracking, only: mass_factor,track_modes
  implicit none
  integer :: n,nmax,nmode,ncand,mxc,mxa,ok,i,info,np
  complex(wp) :: fi(nmax,nmax),avl(nmax),pvec(nmax,mxc)
  integer :: sel(mxc),clusters(mxc)
  real(wp) :: qmac(mxc)
  real(wp), allocatable :: l(:,:)
  character(len=99) :: errmsg
  logical :: first
  ok=-1
  if(nmode<1.or.nmode>mxc.or.n>nmax.or.mod(n,2)/=0) then
    errmsg='mactrack: invalid requested state/physical dimensions'; return
  end if
  np=n/2
  if(np<1.or.np>size(mm,1)) then
    errmsg='mactrack: physical mass dimension mismatch'; return
  end if
  call mass_factor(mm(1:np,1:np),l,errmsg,info); if(info/=0) return
  ! Expand to ALL available candidates before rejecting weak continuation.
  ! NCAND/MXA remain in the public ABI but do not truncate the eigenspectrum.
  call track_modes(l,fi(1:n,1:n),avl(1:n),modal_valid(1:n), &
    pvec(1:np,1:nmode),first,sel(1:nmode),qmac(1:nmode),clusters(1:nmode),errmsg,info)
  if(info/=0) then
    ok=info; return
  end if
  do i=1,nmode
    pvec(1:n,i)=fi(1:n,sel(i))
  end do
  ok=0
end subroutine mactrack
!
!     ==================================================================
!>    @brief matrix for time response.
!
!>    @param[in] pp number of effective bearings with support
!>    @param[in] rpm speed, <0 then fixed bearing parameters
!>    @param[out] aa return matrix aa
!>    @param[in] mte matriz dimension
!>    @param[out] errmsg return an error message
!>    @param[out] ok return flag. unsuccessful if <0.
!
subroutine esptem(pp,rpm,aa,mte,errmsg,ok)
  use com_mat, only: mm, mg, dm, smn
  use rd_kinds, only: lrk, wp
  implicit none
!
!     arguments
  real(lrk) :: rpm
  integer :: pp, mte, ok
  character(len=99) :: errmsg
!     complex space matrix
  complex(wp) :: aa
  dimension aa(mte,mte)
!
!     locals
  real(lrk) :: rad, rpm2radf
  integer :: i, j, ki, kj, oki
  complex(wp) :: j0
  parameter (j0 = (0,0))
!
!     global matrices max number of elementos
  integer :: mtg
  parameter (mtg = 500)
  complex(wp) :: mk2, mc2
  dimension mk2(mtg,mtg),mc2(mtg,mtg)
!
!     global matrices mass mm, stiffness mk,
!     gyroscopic mg and damping mc
!
!     dcmplx -> double complex intrinsic
  intrinsic :: cmplx
!
!     init return
  ok = -1
!
  if (rpm .lt. 0) then
!       fixed parameters
    rad = 0
    call prpkc(rad,dm,pp,mk2,mc2,1,errmsg,oki)
    if (oki .lt. 0) return
  else
!       variable parameters (rpm)
!       rpm -> rad/s
    rad = rpm2radf(rpm)
    call prpkc(rad,dm,pp,mk2,mc2,0,errmsg,oki)
    if (oki .lt. 0) return
  end if
!
!     aa matrix
  do i = 1,dm
    ki = i+dm
!
    do j = 1,dm
      kj = j+dm
      aa(i,j) = cmplx(((rad*mg(i,j))),0, kind=wp)+mc2(i,j)
      aa(i,kj) = cmplx(mm(i,j),0, kind=wp)
      aa(ki,j) = cmplx(mm(i,j),0, kind=wp)
      aa(ki,kj) = j0
    end do
!
  end do
!
!     retorno ok
  ok = 0
!
  return
!
!     sub end
!
end subroutine esptem
!
!     ==================================================================
!>    @brief matrix assembly hh = i*wr*AA+BB, unbalance response.

!>    @param[in] rad speed rad/s
!>    @param[out] dmm hh return dimension
!>    @param[in] pp number of effective bearings with support
!>    @param[out] hh return matrix
!>    @param[out] errmsg return an error message
!>    @param[out] ok return flag. unsuccessful if <0.
!
subroutine mntmth(rad,dmm,pp,fx,hh,errmsg,ok)
  use com_mat, only: mm, mg, dm, smn
  use com_mtk, only: mk1
  use rd_kinds, only: lrk, wp
  implicit none
!
!     arguments
  real(lrk) :: rad
  integer :: dmm, pp, fx, ok
  character(len=99) :: errmsg
!
!     locals
  integer :: ii, jj, ki, kj, oki
  complex(wp) :: j1, j0, jr, tm1, tm2
  parameter (j1 = (0,1),j0 = (0,0))
!
!     global matrices
  integer :: mtg, mte
  parameter (mtg = 500,mte = 2*mtg)
!
!     local matrices
  complex(wp) :: wk, wc, hh
  dimension wk(mtg,mtg),wc(mtg,mtg),hh(mte,mte)
!
!     global matrices mass mm, stiffness mk,
!     gyroscopic mg and damping mc
!
!     base stiffness matrix
!
!     dcmplx -> double complex intrinsic
  intrinsic :: cmplx
!
!     init return
  ok = -1
!
!     prepare matrices with variable parameters
  call prpkc(rad,dm,pp,wk,wc,fx,errmsg,oki)
  if (oki .lt. 0) return
!
!     size hh
  dmm = 2*dm
!
!     complex speed j*w -> jr
  jr = j1*rad
!
!     prepares hh matrix as
!     G  = RAD*G1+C
!     AA = [G M;M Z]
!     BB = [K Z;Z -M]
!     hh = i*wr*AA+BB
!
  do ii = 1,dm
    ki = ii+dm
!
    do jj = 1,dm
      kj = jj+dm
!
!         RAD*G1+cc
      tm1 = cmplx(rad*mg(ii,jj),0, kind=wp)+wc(ii,jj)
!
!         K1+kk
      tm2 = cmplx(mk1(ii,jj),0, kind=wp)+wk(ii,jj)
      hh(ii,jj) = (jr*tm1)+tm2
!
!         M
      tm1 = cmplx(mm(ii,jj),0, kind=wp)
!
!         Z
      hh(ii,kj) = jr*tm1
!
!         Z
      hh(ki,jj) = jr*tm1
!
!         -M
      tm2 = cmplx(-mm(ii,jj),0, kind=wp)
      hh(ki,kj) = tm2
!
    end do
!
  end do
!
!     Frequency-domain foundation impedance table, when selected.
!     The table already represents Zf(w), so it is added directly to
!     the physical displacement block. PHYSICAL_MCK remains in prpkc.
  call foundation_add_dynamic_hh(rad,dm,hh,mte,errmsg,oki)
  if(oki.lt.0) return
!
!     return ok
  ok = 0
!
  return
!
end subroutine mntmth
!
!     ==================================================================
!>    @brief modal space - matrices AA e BB assembly.
!
!>    @param[in] pp number of effective bearings with support
!>    @param[in] rpm speed, if <0 then fixed bearing parameters
!>    @param[out] errmsg return an error message
!>    @param[out] ok return flag. unsuccessful if <0.
!
subroutine espmod(pp,rpm,errmsg,ok)
  use rd_kinds, only: lrk
  implicit none
  real(lrk) :: rpm
  integer :: pp, ok
  character(len=99) :: errmsg
!
!     Compatibility entry point: historical synchronous modal contract.
  call espmod_sync(pp,rpm,errmsg,ok)
  return
end subroutine espmod
!
!     ==================================================================
!>    @brief historical synchronous modal formulation.
!
subroutine espmod_sync(pp,rpm,errmsg,ok)
  use rd_kinds, only: lrk
  implicit none
  real(lrk) :: rpm
  integer :: pp, ok, fx
  character(len=99) :: errmsg
!
!     rpm<0 is the historical fixed-bearing selector.  For variable
!     bearings rpm selects K/C, while the synchronous gyroscopic term
!     remains the historical (M-iG) formulation.
  fx = 0
  if (rpm .lt. 0._lrk) fx = 1
  call espmod_core(pp,rpm,fx,0,errmsg,ok)
  return
end subroutine espmod_sync
!
!     ==================================================================
!>    @brief natural modes at a prescribed physical spin speed.
!
subroutine espmod_speed(pp,rpm,fx,errmsg,ok)
  use rd_kinds, only: lrk
  implicit none
  real(lrk) :: rpm
  integer :: pp, fx, ok
  character(len=99) :: errmsg
!
  if (rpm .lt. 0._lrk) then
    errmsg = 'espmod_speed: prescribed spin rpm must be nonnegative'
    ok = -1
    return
  end if
  call espmod_core(pp,rpm,fx,1,errmsg,ok)
  return
end subroutine espmod_speed
!
!     ==================================================================
!>    @brief shared modal assembly/eigensolution core.
!>    kind=0: synchronous historical (M-iG) state formulation.
!>    kind=1: prescribed spin Mqdd+(C+Omega G)qd+Kq=0.
!
subroutine espmod_core(pp,rpm,fx,kind,errmsg,ok)
  use rd_textfun, only: fomsgf
  use com_epm, only: fi, fn, avl, ddm
  use com_epm1, only: psi
  use com_mat, only: mm, mg, dm, smn
  use com_mtk, only: mk1
  use rd_kinds, only: lrk, wp
  use rd_modal_metrics, only: spectrum_report
  implicit none
!
  real(lrk) :: rpm
  integer :: pp, fx, kind, ok
  character(len=99) :: errmsg
!
  character(len=3) :: cld
  character(len=7) :: nmm
  parameter (cld = 'mte',nmm = 'espmod')
  integer :: i, j, ki, kj, n, oki
  real(lrk) :: mnr, rad, spin, rpm2radf
  complex(wp) :: mt, j0
  parameter(mnr = 1e-9_lrk,j0 = (0._wp,0._wp))
!
  integer :: mtg, mte
  parameter (mtg = 500,mte = 2*mtg)
!
!
!
  complex(wp) :: aa, bb, mk2, mc2
  dimension aa(mte,mte),bb(mte,mte),&
  &mk2(mtg,mtg),mc2(mtg,mtg)
  intrinsic :: abs, real, cmplx
!
  ok = -1
  call progress_begin('MODAL_SPACE',4,&
  &'assemble and solve lateral eigenproblem')
  n = 2*dm
  ddm = n
  if(n .gt. mte) then
    errmsg = fomsgf(99, nmm,4,cld,0)
    return
  end if
  if (kind .ne. 0 .and. kind .ne. 1) then
    errmsg = 'espmod: invalid modal formulation selector'
    return
  end if
!
  spin = 0._lrk
  if (rpm .ge. 0._lrk) spin = rpm2radf(rpm)
  call progress_update('MODAL_SPACE',1,4,real(dm, lrk),'DOF')
  call progress_stage('MODAL_SPACE',&
  &'assemble bearing/support/foundation K and C')
!
  if (fx .eq. 1) then
    call prpkc(mnr,dm,pp,mk2,mc2,1,errmsg,oki)
  else
    if (rpm .lt. 0._lrk) then
      errmsg = 'espmod: variable bearing modal rpm is invalid'
      return
    end if
    rad = rpm2radf(rpm)
    call prpkc(rad,dm,pp,mk2,mc2,0,errmsg,oki)
  end if
  if (oki .lt. 0) return
  call sommat_rdc(mk1,mk2,mk2,dm,dm,mtg,mtg)
!
  call progress_update('MODAL_SPACE',2,4,rpm,'RPM')
  if (kind .eq. 0) then
    call progress_stage('MODAL_SPACE',&
    &'assemble synchronous historical state matrices')
  else
    call progress_stage('MODAL_SPACE',&
    &'assemble prescribed-spin state matrices')
  end if
!
  do i = 1,dm
    ki = i+dm
    do j = 1,dm
      kj = j+dm
      if (kind .eq. 0) then
!           Historical synchronous problem:
!           (M-iG)*lambda**2+C*lambda+K=0.
        mt = cmplx(mm(i,j),-mg(i,j), kind=wp)
        aa(i,j) = mc2(i,j)
        aa(i,kj) = mt
        aa(ki,j) = mt
        aa(ki,kj) = j0
        bb(i,j) = mk2(i,j)
        bb(i,kj) = j0
        bb(ki,j) = j0
        bb(ki,kj) = -mt
      else
!           Prescribed spin:
!           M*qdd+(C+Omega*G)*qd+K*q=0.
        mt = cmplx(mm(i,j),0._wp, kind=wp)
        aa(i,j) = mc2(i,j)+cmplx(spin*mg(i,j),0._wp, kind=wp)
        aa(i,kj) = mt
        aa(ki,j) = mt
        aa(ki,kj) = j0
        bb(i,j) = mk2(i,j)
        bb(i,kj) = j0
        bb(ki,j) = j0
        bb(ki,kj) = -mt
      end if
    end do
  end do
!
  call progress_update('MODAL_SPACE',3,4,real(n, lrk),'STATE_DOF')
  call progress_stage('MODAL_SPACE','solve adjoint eigenproblem')
  call adjeig(aa,bb,fi,psi,avl,n,mte,mte,.false.,errmsg,oki)
  if (oki .ne. 0) return
  do i = 1,dm
    if(kind==1) then
      fn(i) = real(abs(aimag(avl(i))), lrk)
    else
      fn(i) = real(abs(avl(i)), lrk)
    end if
  end do
!
  if(kind==1) call spectrum_report('MODES',real(rpm,wp),avl(1:n))
  call progress_update('MODAL_SPACE',4,4,real(dm, lrk),'MODES')
  ok = 0
  call progress_end('MODAL_SPACE','OK')
  return
end subroutine espmod_core
!

