!     $Id$
!     ==================================================================
!
!>    @file resp_t.f
!>    @brief time response, fidex bearing parameters,
!>    last changes:<br>
!>    added pv, speed dependent bearing params - francisco - 11/02/2009<
!>    added pp, bearing support - francisco - 22/10/2010<br>
!>    removed absolute value (abs) from p(j,m) - francisco - 22/10/2010<
!>    added parameter shp, speed shape - francisco 15/06/2011<br>
!>    added ori block /unb/ - francisco - 29/06/2011<br>
!>    added variable jj, speed screen change - francisco 29/092014<br>
!>    removed k= k+1 for one speed - francisco 29/09/2014<br>
!>    added time response angle range on mdd block - francisco 01/10/201
!>    changed pmn to complex - francisco - feb-19<br>
!>    added central messages, function - francisco - apr-19<br>
!>    changed mxb = 99 francisco - apr-20<br>
!>    added angle unit handling - francisco - sep-20<br>
!>    updated screen feedback to central routine in saidas.f - francisco
!>    added support/check output - francisco aug-21.
!
!     ==================================================================
!>    @brief time response.
!
!>    @param[in] pdm matrix dimension
!>    @param[in] pp number of effective bearings with support
!>    @param[in] pv speed dependent bearing variables
!>    @param[in] shp speed shape flag
!>    @param[in] std standard input output
!>    @param[in] plt HPGL plot output
!>    @param[out] errmsg return an error message
!>    @param[out] ok return flag. unsuccessful if <0.
!
subroutine resp_t(pdm,pp,pv,shp,std,plt,errmsg,ok)
  use rd_textfun, only: cadjf, ciff, femsgf, fomsgf
  use com_epm, only: fi, fn, avl, ddm
  use com_epm1, only: psi
  use com_epmq, only: modal_condition, modal_valid
  use com_mdd, only: qtd_modos, porb, rorb, rang
  use com_mfa, only: ma, rm, au
  use com_sec, only: n, y, nt, nn
  use com_unb, only: nini_r, nfin_r, dw_r
  use com_unb0, only: pr, desp, ori, np, nm
  use com_unb1, only: ndd, mu, ed, tpf, nb
  use rd_kinds, only: lrk, wp
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
  integer :: pdm, pp, pv, ok
  character(len=99) :: errmsg
  logical :: shp, std, plt
!
!     locals
  logical :: isr
  character(len=1) :: crd, cln
  character(len=3) :: cbd
  character(len=2) :: dr(2)
  character(len=6) :: nmm
  character(len=9) :: cfm
  dimension cln(5)
  integer :: i, j, k, l, m, ns, nr, nmd, o, h, jj, oki, ifidxof
  real(lrk) :: pi2, rpif
  complex(wp) :: cs, j0, j1, r1
  real(wp) :: eta, dznrm2
!
  integer :: mxr
  parameter (mxr = 100)
  parameter (cln = (/'N','T','R','r',' '/),nmm = 'resp_t',&
  &dr = (/'FW','BW'/),cfm = '(f6.0,6a)',cbd = 'mtr')
!
!     changed pmn to complex - francisco - feb-19
  real(lrk) :: ndr, rad, rpm, fps
  real(lrk) :: rpm2radf, toradf, riff
  complex(wp) :: p1, p2, pmn
!
!     input parameters
  integer :: mxd
  parameter (mxd = 99)
!
!     total number of sections
  integer :: mts
  parameter (mts = 999)
!
!     all sections (divisions)
!
!     unbalance / orbits
  integer :: mxb, mxp
  parameter (mxb = 99,mxp = 9)
!
!     unbalance
!     kind of force 0=unbalance 1=concentrated
!
!     modes
!
!     amplification factor and modal rpm block
!     angle unit r -> radian, default degree
!
!     maximum number of elements global matrivcs, size
  integer :: mtg
  parameter (mtg = 500)
!
!     globals modal space
  integer :: mte
  parameter (mte = 2*mtg)
!
!
!     locals
!
!     complex space matrix
  complex(wp) :: aa, psil, fil, p, aphi
  real(wp) :: pnorm, anorm
!
!     pdd-position section / disks
!     mtr-amplitude and speed arrays dimension
!     no-number of orbit outputs, should be defined on entrada.f
!     prs-orbit nearest section index
  integer :: dm, pdd, mtr, npo, no, prs, foundation_ndof_f
  parameter (npo = 50,mtr = 2*mtg)
!
!     orbit time points
!     section / orbit position (mm)
  real(lrk) :: t, u, w, rpg, py
!     preccession forward FW / backward BW
  character(len=2) :: di
!
  dimension t(npo),u(mts,mtr,npo),w(mts,mtr,npo),rpg(mtr),&
  &py(mts),&
  &aa(mte,mte),psil(mte,mte),fil(mte,mte),p(mte,npo),&
  &aphi(mte),pnorm(mte),anorm(mte),&
  &pdd(mxd),prs(mts),di(mts,mtr)
!
!     dcmplx -> double complex intrinsic
  intrinsic :: abs, cos, exp, huge, min, nint, sin, sqrt, tiny, cmplx
!
!     return
  ok = -1
!
  j0 = cmplx(0._wp,0._wp, kind=wp)
  j1 = cmplx(0._wp,1._wp, kind=wp)
  r1 = cmplx(1._wp,0._wp, kind=wp)
!
!     check for excitation angle in radian, default degree
  crd = cadjf(1, au,1)
  isr = crd .eq. cln(3) .or. crd .eq. cln(4)
!
!     parameter check
!     check speed increment
  if (dw_r .le. 0) then
!       speed increment <= 0
    errmsg = femsgf(99, nmm,6,4,0)
    return
  end if
!     check speed range
  if (nfin_r-nini_r .le. 0) then
!       speed range <= 0
    errmsg = femsgf(99, nmm,6,5,0)
    return
  end if
!
!     set matrix size
!     if modal space was calculedn ddm is available on common
  ddm = pdm
!
!     check number of modes
  if (qtd_modos .lt. 1) then
!       todo
!       assumed
    nmd = ddm/4
  else
!       space modes =2*modes
    nmd = qtd_modos*2
  end if
  if (nmd .gt. ddm) then
    errmsg = 'resp_t: requested modal order exceeds state dimension'
    return
  end if
!
!     speed (rpm)
  if (rorb .gt. 0._lrk) then
!       just one speed, initial speed
    rpm = rorb
    ndr = 1
    nr = 1
  else
!       UNBFD is the response speed increment [rpm], consistently with
!       resp_f/resp_fv and the frontend solver contract.
    ndr = dw_r
!       initial speed
    rpm = nini_r
!       avoid the singular zero-speed harmonic evaluation
    if (rpm .le. 0._lrk) rpm = ndr
!       number of generated speed points, including the initial point
    nr = int((nfin_r-rpm)/ndr)+1
    if (nr .lt. 1) nr = 1
  end if
!
!     fail before indexing rpg/u/w beyond the historical speed capacity.
  if (nr .gt. mtr) then
    errmsg = fomsgf(99, nmm,4,cbd,0)
    return
  end if
!
!     search unbalance positions
  call inddbl(y,ndd,pdd,nb,nt,mxb,mxd,mts,errmsg,oki)
  if (oki .lt. 0) return
!
!     check for some unbalance excitation
  no = 0
  do i = 1,nb
!       force kind check
    if (tpf(i) .eq. 0) then
      no = 1
      exit
    end if
  end do
!     no unbalance excitation
  if (no .eq. 0) then
    errmsg = fomsgf(99, nmm,18,cln(5),0)
    return
  end if
!
!     check for speed shape
  if (.not. shp) then
!       just one output position
!       orbit output, should review entrada.f
    no = 1
    py(no) = porb
!       search for section position
!       nearest of orbit position -> prs,py
    call indorb(y,py,prs,py,no,nt,mts,mts,errmsg,oki)
    if (oki .lt. 0) return
!       check for support
    if (pp .gt. 0) then
!         have support
      if (porb .lt. 0) then
!           suport number 1,2
        i = nint(abs(porb))
        if (i .gt. 0 .and. i .le. pp) then
!             back to support index < 0
          py(no) = porb
        else
!            'invalid option' !6
!            'no/invalid support' !8
          errmsg = femsgf(99, nmm,6,8,0)
          return
        end if
      end if
    else
!         no support present
      if (porb .lt. 0) then
!           cannot be < 0, no support present
        errmsg = femsgf(99, nmm,6,8,0)
        return
      end if
    end if
  else
!       speed shape / all sections
!       TIMSPD > 0 keeps the historical one-speed behavior. TIMSPD <= 0
!       now reuses the already-resolved UNBIF/UNBFF/UNBFD sweep above, so
!       the existing u(section,speed,point) storage and s_resp_t output can
!       expose every native section at every solved speed without changing
!       the harmonic-response formulation.
!       number of orbit positions = number of sections
    no = nt
!       copy y -> py
    do k = 1,no
!         positions
      py(k) = y(k)
!         index
      prs(k) = k
    end do
  end if
!
!     2*pi
  pi2 = 2*rpif()
!     orbit array
!     changed from original, that divides t by rad inside speed loop
!     but on co/sinus calculation restores due product t by rad
  do i = 1,npo
    t(i) = pi2*(i-1)/(npo-1)
  end do
!
!     matrix dimension without supoort
  dm = ddm/2-2*pp-foundation_ndof_f()
!
  call progress_begin('TIME_HARMONIC_RESPONSE',nr,&
  &'historical lateral time-harmonic response')
!     controls speed screen showing
  jj = 0
!
!     speed loop
  k = 0
  do while(k .lt. nr)
    k = k+1
!       check array limit
    if (k .gt. mtr) then
      errmsg = fomsgf(99, nmm,4,cbd,0)
      return
    end if
!
    call progress_update('TIME_HARMONIC_RESPONSE',k,nr,rpm,&
    &'RPM')
!
!       bearing variable parameters (pv <> 0)
    if (pv .ne. 0) then
!         Historical resp_t intentionally retains the synchronous
!         modal basis; changing this basis changes its legacy transfer
!         formula.  Variable-bearing cases recompute that basis here.
      call espmod_sync(pp,rpm,errmsg,oki)
      if (oki .ne. 0) return
    end if
!
!       Fail before any secondary modal projection if a requested pair
!       was already rejected by the primary biorthogonal normalization.
    call modal_require_range_counts(nmd,max(0,qtd_modos),nmd,&
    &avl,mte,errmsg,oki)
    if (oki .lt. 0) return
!
!       rpm -> rad/s
    rad = rpm2radf(rpm)
!
!       matrix for time response -> aa
    call esptem(pp,rpm,aa,mte,errmsg,oki)
    if (oki .ne. 0) return
!
!       Preserve scale-independent norms for the second normalization.
!       Stored PSI is unconjugated LAPACK VL, so PSI**T is VL**H.
    do j = 1,nmd
      pnorm(j) = dznrm2(ddm,psi(1,j),1)
      do i = 1,ddm
        aphi(i) = j0
        do l = 1,ddm
          aphi(i) = aphi(i)+aa(i,l)*fi(l,j)
        end do
      end do
      anorm(j) = dznrm2(ddm,aphi,1)
    end do
!
!       matrix product (psi' * aa) -> (fil)
    call ZGEMM (cln(2),cln(1), ddm, ddm, ddm, r1, psi, mte,&
    &aa, mte, j0, fil, mte )
!
!       matrix product (fil * fi) -> (aa)
    call ZGEMM (cln(1),cln(1), ddm, ddm, ddm, r1, fil, mte,&
    &fi, mte, j0, aa, mte )
!
!       Second numerical boundary: only requested modal columns are
!       normalized, and each one must pass an independent eta check.
    do j = 1,nmd
      eta = 0._wp
      if (pnorm(j) .gt. 0._wp .and. anorm(j) .gt. 0._wp) then
        if (pnorm(j) .le. huge(1._wp) .and.anorm(j) .le. huge(1._wp)) then
          eta = (abs(aa(j,j))/pnorm(j))/anorm(j)
        end if
      end if
      modal_condition(j) = min(modal_condition(j),eta)
      if (eta .ne. eta .or. eta .gt. huge(1._wp) .or.eta .lt. modal_condition_min) modal_valid(j) = .false.
      call modal_require_pair_counts(j,max(0,qtd_modos),nmd,&
      &avl,mte,errmsg,oki)
      if (oki .lt. 0) return
!
      cs = sqrt(aa(j,j))
      if (abs(cs) .ne. abs(cs) .or.abs(cs) .gt. huge(1._wp) .or.abs(cs) .le. tiny(1._wp)) then
        modal_condition(j) = 0._wp
        modal_valid(j) = .false.
        call modal_require_pair_counts(j,max(0,qtd_modos),nmd,&
        &avl,mte,errmsg,oki)
        return
      end if
      do i = 1,ddm
        fil(i,j) = fi(i,j)/cs
!           transpose matrix (psi) -> (psil)
        psil(j,i) = psi(i,j)/cs
      end do
    end do
!
!       zero load matrix (p)
    call zermat_c(p,ddm,npo,mte,npo)
!
!       orbit calculation -> (p)
!       elements of tmp matrix
    do j = 1,nmd
!         unbalance
      do i = 1,nb
!           force kind check for unbalance (0)
        if (tpf(i) .eq. 0) then
!             if angle is not given in radian convert deg -> rad
          fps = riff(.not. isr,toradf(ed(i)),ed(i))
!             complex force
          pmn = mu(i)*rad**2*exp(j1*fps)
!             excitation position index
          l = ifidxof(pdd(i),1)
          h = l+1
!             orbit loop
          do m = 1,npo
!               removed product t by rad, see above note on t
            p1 = (rad*psil(j,l)-psil(j,h)*avl(j))*cos(t(m))
            p2 = (rad*psil(j,h)+psil(j,l)*avl(j))*sin(t(m))
            p(j,m) = p(j,m)+(pmn*(p1-p2)/(rad**2+avl(j)**2))
          end do
!             force if
        end if
!
!           unbalance
      end do
!
    end do
!
!       matrix product (fil) * (p) -> (aa)
!       displacements, fil must have same number of columns as modes
!        call ZGEMM (cln(1),cln(1), ddm, npo, ddm, r1, fil, mte,
    call ZGEMM (cln(1),cln(1), ddm, npo, nmd, r1, fil, mte,&
    &p, mte, j0, aa, mte )
!
!       orbit positions array
    do o = 1,no
!         normal position >= 0
      if (.not. py(o) .lt. 0._lrk) then
!           section matriz position
        h = prs(o)
!           excitation position x index
        l = ifidxof(h,1)
      else
!           support < 0
        if (pp .eq. 0) then
          errmsg = femsgf(99, nmm,6,8,0)
          return
        end if
!           x index on support
        l = dm+(nint(abs(py(o)))-1)*2+1
      end if
!         z index
      m = l+1
!         graphical points
      do j = 1,npo
        u(o,k,j) = real(real(aa(l,j), wp), lrk)
        w(o,k,j) = real(real(aa(m,j), wp), lrk)
      end do
!
!         check precession direction
      rad = (u(o,k,2)*w(o,k,1))-(u(o,k,1)*w(o,k,2))
!         if rad>0 FW else BW
      di(o,k) = ciff(2, rad .gt. 0,dr(1),dr(2))
!
!         orbit position
    end do
!
!       graphic speed
    rpg(k) = rpm
!
!       new speed rpm
    rpm = rpm+ndr
!
!       end speed loop
  end do
!
!     output
!     number of speeds
  ns = k
!     will set ok, number of modes = space modes / 2
  call progress_stage('TIME_HARMONIC_RESPONSE',&
  &'write orbit/time-harmonic output')
  call s_resp_t(shp,no,py,rpg,u,w,di,ns,npo,nmd/2,&
  &mts,mtr,rang,std,plt,errmsg,ok)
  if(ok.eq.0) call progress_end('TIME_HARMONIC_RESPONSE','OK')
!
  return
!
!     end sub
!
end subroutine resp_t
!
