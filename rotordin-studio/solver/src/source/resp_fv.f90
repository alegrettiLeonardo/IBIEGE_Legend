!     $Id$
!     ==================================================================
!
!>    @file resp_fv.f
!>    @brief unbalance response, speed variable bearing parameters.
!>    last changes:<br>
!>    new unbalance response with speed variable bearing parameters - fr
!>    added st, status of automatic analysis - francisco - 16/06/2011<br
!>    added ori, common block /unb/, vector transform - francisco - 29/0
!>    added zgetrf that was removed from zgetri - francisco - 16/04/2015
!>    updated coord rot angle to radian, *BUG* corr - francisco - jul-15
!>    moved vetorial transformation to matfun - francisco - jul-15<br>
!>    removed ma argument on maxamp call - francisco - oct-15<br>
!>    added ok, errmsg arguments on maxamp call - francisco - oct-15<br>
!>    added central messages, functions - francisco - apr-19<br>
!>    removed ori angle guess, always in radian - francisco - apr-19
!>    removed nb check, already done in inddbl - francisco - feb-20<br>
!>    changed mxb = 99 francisco - apr-20<br>
!>    updated matrix inversion by LU fact - francisco - may-20<br>
!>    added angle unit handling - francisco - sep-20<br>
!>    updated screen feedback to central routine in saidas.f - francisco
!>    added support output check - francisco - aug-21.
!
!     ==================================================================
!>    @brief frequency respose calculation, speed bearing parameters.
!
!>    @param[in] ddm matrix dimension
!>    @param[in] pp number of effective bearings with support
!>    @param[in] std standard input output
!>    @param[in] frl true if plot vertical scale is logarithmic
!>    @param[in] plt HPGL output
!>    @param[out] errmsg return an error message
!>    @param[out] ok return flag. unsuccessful if <0.
!
subroutine resp_fv(ddm,pp,fx,std,frl,plt,errmsg,ok)
  use rd_textfun, only: cadjf, femsgf, fomsgf
  use com_mdd1, only: ru
  use com_mfa, only: ma, rm, au
  use com_sec, only: n, y, nt, nn
  use com_unb, only: nini_r, nfin_r, dw_r
  use com_unb0, only: pr, desp, ori, np, nm
  use com_unb1, only: ndd, mu, ed, tpf, nb
  use rd_kinds, only: lrk, wp
  use rd_response_quality, only: response_quality_t,response_capture, &
    response_condition,response_report
  implicit none
  type(response_quality_t) :: audit_state
  complex(wp), allocatable :: audit_force(:)
!
!     arguments
  integer :: ddm, pp, fx, ok
  character(len=99) :: errmsg
  logical :: std, frl, plt
!
!     locals
  logical :: isr, isd
  character(len=1) :: crd, cbs
  character(len=3) :: cinfo, clb
  character(len=7) :: nmm
  character(len=9) :: cfm
  integer :: dm, i, j, k, l, m, o, nr, oki, ptotal, ifidxof, foundation_ndof_f
  real(lrk) :: ndr, rad, rpm, fps, prec, rpm2radf, toradf, riff
  complex(wp) :: pmn, xr, zr, c1p, c1n
  dimension cbs(5)
  parameter(nmm = 'resp_fv',cfm = '(f6.0,6a)',cbs = (/'r','R','d','D','N'/),clb = 'mtr',prec = 1e-6_lrk)
!
!     input parameters
  integer :: mxr, mxd
  parameter (mxr = 100,mxd = 99)
!
!     total number of sections
  integer :: mts
  parameter (mts = 999)
!
!     unbalance
  integer :: mxb, mxp
  parameter (mxb = 99,mxp = 9)
!
!     maximum number of global matrices elements
  integer :: mtg
  parameter (mtg = 500)
!
!     modal space globals
  integer :: mte
  parameter (mte = 2*mtg)
!
!     speed and amplitude vectors
  integer :: mtr
  parameter (mtr = 2*mtg)
!
!     all sections (divisions)
!
!     unbalance block, ori -> radian
!     kind of force, 0 -> unbalance, 1 -> concentrated
!
!     response angle unit 'd' or 'D' for degree, default radian
!
!     modal space block
!     angle unit r -> radian, default degree
!
!     parameters for lapack
  integer :: info, lda, ldb
  PARAMETER (lda = mte,ldb = mte)
  integer :: ipiv( mte )
!
!     locals with dimension
  complex(wp) :: hh, rsp, amp
  integer :: pdd, prs
  real(lrk) :: rpg, py
!     response analysis
  integer :: cn
  real(lrk) :: vf, vr, vm
  character(len=1) :: st
!
  dimension hh(mte,mte),rsp(mte,1),&
  &pdd(mxd),prs(mxp),&
  &amp(mxp,mtr),rpg(mtr),py(mxp),&
  &cn(mxp),vf(mxp,2*mxp),vr(mxp,2*mxp),&
  &vm(mxp,2*mxp),st(mxp,2*mxp)
!
!     dcmplx -> double complex intrinsic
  intrinsic :: abs, char, exp, nint, cmplx
!
!     init return
  ok = -1
!
  c1p = cmplx(0._wp,1._wp, kind=wp)
  c1n = cmplx(0._wp,-1._wp, kind=wp)
!
!     check for excitation angle in radian
  crd = cadjf(1, au,1)
  isr = crd .eq. cbs(1) .or. crd .eq. cbs(2)
!     check for response angle in radian, default radian
  crd = cadjf(1, ru(1),1)
  isd = crd .eq. cbs(3) .or. crd .eq. cbs(4)
!
!     parameters check
!     speed delta
  if (dw_r .le. 0) then
!       invalid option,speed increment<=0
    errmsg = femsgf(99, nmm,6,4,0)
    return
  end if
!     speed range
  if (nfin_r-nini_r .le. 0._lrk) then
!       invalid option,speed limits
    errmsg = femsgf(99, nmm,6,5,0)
    return
  end if
!     response positions
  if (np .le. 0._lrk) then
!       invalid option,on resp. position
    errmsg = femsgf(99, nmm,6,7,0)
    return
  end if
!
!     speed step (rpm)
  ndr = dw_r
!     initial speed
  rpm = nini_r
!
!     search nearest unbalance section position -> pdd
  call inddbl(y,ndd,pdd,nb,nt,mxb,mxd,mts,errmsg,oki)
  if (oki .lt. 0) return
!
!     search nearest response section position -> prs,py
  call indrsp(y,pr,prs,py,np,nt,mxp,mts,errmsg,oki)
  if (oki .lt. 0) return
!
!     matrix dimension without support
  dm = ddm/2-2*pp-foundation_ndof_f()
!
!     speed (rpm) visualization control
  l = 1
!
!     initialize optional FOUNDATION interface response output
  call foundation_response_begin()
!
!     Direct-response telemetry observes the existing rpm loop only.
  ptotal=int((nfin_r-nini_r)/ndr)+1
  if (ptotal .lt. 1) ptotal=1
  call progress_begin('DIRECT_RESPONSE',ptotal,&
  &'solve dynamic stiffness at each speed')
  call progress_stage('DIRECT_RESPONSE',&
  &'factor dynamic stiffness and solve response')
!
!     speed loop
  k = 0
  do while(rpm .le. nfin_r+prec)
!       iteration count
    k = k+1
!       check output dimension
    if (k .gt. mtr) then
!         'out of bounds' !4
      errmsg = fomsgf(99, nmm,4,clb,0)
      call progress_end('DIRECT_RESPONSE','FAILED')
      return
    end if
!
    if (rpm .ge. mxr*l .and. .not. std) then
!         screen speed + backspace
      call bprint(rpm,0,cfm,6)
      l = l+1
    end if
!
!       rpm -> rad/s
    rad = rpm2radf(rpm)
!
!       H matrix for this speed rad/s
    call mntmth(rad,ddm,pp,fx,hh,errmsg,oki)
    if(oki .lt. 0) then
      call progress_end('DIRECT_RESPONSE','FAILED')
      return
    end if
!       The factorization of A to get IPIV
!       the unit diagonal elements of L are not stored
    call response_capture(hh,ddm,audit_state,errmsg,oki)
    if(oki/=0) return
    call ZGETRF( ddm, ddm, hh, LDA, IPIV, INFO )
    if (info .ne. 0) then
      write(cinfo,15)info
      errmsg = fomsgf(99, nmm,23,cinfo,0)
      call progress_end('DIRECT_RESPONSE','FAILED')
      return
    end if
!
!       zero complex response matrix -> rsp
    call response_condition(hh,ddm,audit_state,errmsg,oki)
    if(oki/=0) return
    if(allocated(audit_force)) deallocate(audit_force)
    allocate(audit_force(ddm))
    call zervec_c(rsp,ddm,mte)
!
!       unbalance positions
!       mu -> unbalance kg*m, ed -> phase degree
    do i = 1,nb
!         check for kind of force, 0 -> unbalance
      if (tpf(i) .eq. 0) then
!           if angle is not given in radian convert deg -> rad
        fps = riff(.not. isr,toradf(ed(i)),ed(i))
!           unbalance -> complex force
        xr = c1p*fps
        pmn = (mu(i)*rad**2)*exp(xr)
!           force vector position index
        m = pdd(i)
        j = ifidxof(m,1)
        o = j+1
!           1 -> x, 2 -> z, 3 -> x axis rot, 4 -> z axis rot.
        zr = c1n*pmn
        rsp(j,1) = rsp(j,1)+zr
        rsp(o,1) = rsp(o,1)+pmn
      end if
!
    end do
!
!       solve system [hh]*{rsp} = {rsp} -> (rsp)
    audit_force=rsp(1:ddm,1)
    call ZGETRS(cbs(5), ddm,1, hh, LDA, IPIV, rsp, ldb, INFO )
!       check lapack error code
    if (info .ne. 0) then
!         got error, prepare error message
      write(cinfo,15) info
      errmsg = fomsgf(99, nmm,23,cinfo,0)
      call progress_end('DIRECT_RESPONSE','FAILED')
      return
    end if
!
!       FOUNDATION displacement and dynamic reaction at interfaces.
    call response_report(real(rpm,wp),real(rad,wp),rsp(1:ddm,1), &
      audit_force,audit_state,errmsg,oki)
    if(oki/=0) return
    call foundation_response_point(rpm,rad,rsp,mte)
!
!       response positions
    do i = 1,np
!         normal position > 0
      if (.not. prs(i) .lt. 0._lrk) then
!           normal
        m = prs(i)
        j = ifidxof(m,0)
      else
!           support < 0
        if (pp .eq. 0) then
          errmsg = femsgf(99, nmm,6,8,0)
          call progress_end('DIRECT_RESPONSE','FAILED')
          return
        end if
!           check support index
        m = abs(prs(i))
        if (m .le. 0 .or. m .gt. pp) then
!             invalid support index
          errmsg = femsgf(99, nmm,6,8,0)
          call progress_end('DIRECT_RESPONSE','FAILED')
          return
        end if
        j = dm+(m-1)*2
      end if
!         x index
      m = j+1
!         z index
      o = m+1
!
!         check in radian (default) deg -> rad
      rad = riff(.not. isd,ori(i),toradf(ori(i)))
!         coordinate rotation -> xr,zr
      call vrotate(rsp(m,1),rsp(o,1),rad,xr,zr)
!         complex amplitudes
!         1 -> x, 2 -> z, 3 -> x axis rot, 4 -> z axis rot.
      o = nint(desp(i))
      if(o .eq. 1) then
        amp(i,k) = xr
      else if(o .eq. 2) then
        amp(i,k) = zr
      else
!           coordinates offset
!           1 -> x, 2 -> z, 3 -> x axis rot, 4 -> z axis rot.
        j = j+o
        amp(i,k) = rsp(j,1)
      end if
!
    end do
!
!       rpm speed to graphics
    rpg(k) = rpm
    call progress_update('DIRECT_RESPONSE',k,ptotal,rpm,'RPM')
!
!       new rpm
    rpm = rpm+ndr
!
!       end speed loop
  end do
!
  nr = k
!
!     output
!
!     automatic response analysis, rdfra.f
  call progress_stage('DIRECT_RESPONSE','analyze response peaks')
  call maxamp(np,nr,cn,rpg,amp,vf,vr,vm,st,mxp,mte,errmsg,oki)
  if(oki .lt. 0) then
    call progress_end('DIRECT_RESPONSE','FAILED')
    return
  end if
!     vf amplification factor vector
!     vr maximum speeds vector
!     vm maximums values
!     st status (n)ormal or assumes the same speed on
!      the side (i)nitial (f)inal (F)ail if nothing could be got
!
!     output (full number of modes = dim)
!     will set ok
  call progress_stage('DIRECT_RESPONSE',&
  &'write frequency-response output')
  call s_resp_f(ddm/2,np,nr,cn,py,desp,ori,rpg,amp,vf,vr,vm,&
  &st,mxp,mte,std,frl,plt,errmsg,ok)
  if (ok .lt. 0) then
    call progress_end('DIRECT_RESPONSE','FAILED')
    return
  end if
  call progress_end('DIRECT_RESPONSE','OK')
!
  return
!
15 format(i3)
!
end subroutine resp_fv
!
