!     $Id$
!     ==================================================================
!
!>    @file resp_f.f
!>    @brief unbalance response, fidex bearing parameters, last changes:
!>    new unbalance responde analysis - francisco - 03/12/2008<br>
!>    added desp em s_resp_f - francisco - 11/10/2010<br>
!>    added pp, number of supports - francisco - 22/10/2010<br>
!>    added st, status of automatic analysis - francisco - 16/06/2011<br
!>    added dr, reference offset angle  - francisco - 27/06/2011<br>
!>    added ori, common block /unb/, vector transform - francisco - 29/0
!>    added std i/o -francisco - 16/02/2012<br>
!>    updated coord. rot. angle to radian,*BUG* corr. - francisco - jul-
!>    moved vetorial transformation to matfun.f - francisco - jul-15<br>
!>    removed ma argument on maxamp call - francisco - oct-15<br>
!>    added ok, errmsg arguments on maxamp call - francisco - oct-15<br>
!>    added central messages, functions - francisco - apr-19<br>
!>    removed ori angle guess, always in radian - francisco - apr-19<br>
!>    removed nb check, already done in inddbl - francisco - feb-20<br>
!>    changed mxb = 99 francisco - francisco - apr-20<br>
!>    added angle unit handling - francisco - sep-20<br>
!>    updated screen feedback to central routine in saidas.f - francisco
!>    added support output check - francisco - aug-21.
!
!     ==================================================================
!>    @brief frequency respose calculation, fixed bearing parameters.
!
!>    @param[in] pp number of effective bearings with support
!>    @param[in] std standard input output
!>    @param[in] frl true if plot vertical scale is logarithmic
!>    @param[in] plt HPGL output
!>    @param[out] errmsg return an error message
!>    @param[out] ok return flag. unsuccessful if <0.
!
subroutine resp_f(pp,std,frl,plt,errmsg,ok)
  use rd_textfun, only: cadjf, femsgf, fomsgf
  use com_epm, only: fi, fn, avl, ddm
  use com_epm1, only: psi
  use com_mdd1, only: ru
  use com_mfa, only: ma, rm, au
  use com_sec, only: n, y, nt, nn
  use com_unb, only: nini_r, nfin_r, dw_r
  use com_unb0, only: pr, desp, ori, np, nm
  use com_unb1, only: ndd, mu, ed, tpf, nb
  use rd_kinds, only: lrk, wp
  implicit none
!
!     arguments
  integer :: pp, ok
  character(len=99) :: errmsg
  logical :: std, frl, plt
!
!     locals
  logical :: isr, isd
  character(len=1) :: crd, cbs
  character(len=3) :: cbl
  character(len=6) :: nmm
  character(len=9) :: cfm
  dimension cbs(7)
  integer :: dm, i, j, k, l, m, mxr, o, nr, oki, ifidxof, foundation_ndof_f
  real(lrk) :: ndr, rad, rpm, fps, prec, rpm2radf, toradf, riff
  complex(wp) :: j0, c1p, c1n, r1, pmn, xr, zr
  parameter (prec=1e-6_lrk,mxr = 100,nmm = 'resp_f', cfm = '(f6.0,6a)',cbl = 'mtr',cbs = (/'r','R','d','D','N','T', &
    & ' '/))
!
!     input parameters
  integer :: mxd
  parameter (mxd = 99)
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
!     mte = 2 * mtg
  parameter (mte = 2*mtg)
!
!     speed and amplitude vectors
  integer :: mtr
  parameter (mtr = 2*mtg)
!
!     all sections (divisions)
!
!     unbalance block, ori -> rad
!     kind of force, 0 -> unbalance, 1 -> concentrated
!
!     response angle unit 'd' or 'D' for degree, default radian
!
!     modal space block
!
!     amplification factor and modal rpm block
!     angle unit r -> radian, default degree
!
!     locals with dimension
!     rsp and modal are vectors.  The historical implementation formed
!     the full receptance matrix Phi*D*Psi^T at every speed, although
!     only its product by the force vector was required.  Keeping the
!     operation in modal-vector form is algebraically identical and
!     avoids three dense matrix products per speed point.
  complex(wp) :: rsp, q, modal, amp
  real(lrk) :: rpg, py, mnr
  integer :: pdd, prs
!     response analysis
  integer :: cn
  real(lrk) :: vf, vr, vm
  character(len=1) :: st
!
  dimension rsp(mte,1),q(mte,1),modal(mte,1),amp(mxp,mtr),&
  &rpg(mtr),py(mxp),&
  &pdd(mxd),prs(mxp),&
  &cn(mxp),&
  &vf(mxp,2*mxp),vr(mxp,2*mxp),vm(mxp,2*mxp),&
  &st(mxp,2*mxp)
!     minimum speed (rpm)
  parameter (mnr = 1e-9_lrk)
!
!     dcmplx -> double complex intrinsic
  intrinsic :: abs, char, exp, nint, cmplx
!
!     return
  ok = -1
!
  j0 = cmplx(0._wp,0._wp, kind=wp)
  r1 = cmplx(1._wp,0._wp, kind=wp)
  c1p = cmplx(0._wp,1._wp, kind=wp)
  c1n = cmplx(0._wp,-1._wp, kind=wp)
!
!     check for excitation angle in radian, default degree
  crd = cadjf(1, au,1)
  isr = crd .eq. cbs(1) .or. crd .eq. cbs(2)
!     check for response angle in degree, default radian
  crd = cadjf(1, ru(1),1)
  isd = crd .eq. cbs(3) .or. crd .eq. cbs(4)
!
!     parameters check
!     speed delta
  if (dw_r .le. 0) then
    errmsg = femsgf(99, nmm,6,4,0)
    return
  end if
!     speed range
  if (nfin_r-nini_r .le. 0) then
    errmsg = femsgf(99, nmm,6,5,0)
    return
  end if
!     response positions
  if (np .le. 0) then
    errmsg = femsgf(99, nmm,6,7,0)
    return
  end if
!     unbalance excitation
  l = 0
  do i = 1,nb
    if (tpf(i) .eq. 0) then
      l = 1
      exit
    end if
  end do
!     check found unbalance excitation
  if (l .eq. 0) then
!       no excitation found
    errmsg = fomsgf(99, nmm,18,cbs(7),0)
    return
  end if
!
!     speed step (rpm)
  ndr = dw_r
!     initial speed
  rpm = nini_r
!     parameter for min RPM
  if (rpm .le. 0._lrk) rpm = mnr
!
!     search nearest unbalance section position -> pdd
  call inddbl(y,ndd,pdd,nb,nt,mxb,mxd,mts,errmsg,oki)
  if (oki .lt. 0) return
!
!     search nearest response section position -> prs
  call indrsp(y,pr,prs,py,np,nt,mxp,mts,errmsg,oki)
  if (oki .lt. 0) return
!
!     matrix dimension without support
  dm = ddm/2-2*pp-foundation_ndof_f()
!
!     TODO check this criteria
!     response number of modes
  if (nm .le. 0 .or. nm .gt. ddm) then
    nm = ddm/4
  end if
!
!     Legacy callable modal path: nm is the consumed column count.
  call modal_require_range(nm,avl,mte,errmsg,oki)
  if (oki .lt. 0) return
!
!     speed (rpm) visualization control
  l = 1
!
!     initialize optional FOUNDATION interface response output
  call foundation_response_begin()
!
!     speed loop
  k = 0
  do while(rpm .le. nfin_r+prec)
!       iteration count
    k = k+1
!
!       check output dimension
    if (k .gt. mtr) then
      errmsg = fomsgf(99, nmm,4,cbl,0)
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
!       zero complex force vector -> q
    call zervec_c(q,ddm,mte)
!
!       unbalance positions
!       mu -> unbalance kg*m, ed -> phase degree
    do i = 1,nb
!         check for kind of force, 0 -> unbalance
      if (tpf(i) .eq. 0) then
!           if angle is not given in radians convert deg -> rad
        fps = riff(.not. isr,toradf(ed(i)),ed(i))
!           unbalance -> complex force
        xr = c1p*fps
        pmn = (mu(i)*rad**2)*exp(xr)
!           force vector position index
        j = ifidxof(pdd(i),1)
        o = j+1
!           1 -> x, 2 -> z, 3 -> x axis rot, 4 -> z axis rot.
        zr = c1n*pmn
        q(j,1) = q(j,1)+zr
        q(o,1) = q(o,1)+pmn
      end if
!
    end do
!
!       Modal response without forming the full receptance matrix:
!       rsp = fi(:,1:nm) * diag(1/(avl+j*w)) * psi(:,1:nm)^T * q.
!       This is the same equation used historically, evaluated as two
!       matrix-vector products plus a diagonal scaling.
    call zervec_c(modal,nm,mte)
    call ZGEMV(cbs(6),ddm,nm,r1,psi,mte,q,1,j0,modal,1)
    do j=1,nm
      modal(j,1)=modal(j,1)/(avl(j)+c1p*rad)
    enddo
    call zervec_c(rsp,ddm,mte)
    call ZGEMV(cbs(5),ddm,nm,r1,fi,mte,modal,1,j0,rsp,1)
!
!       FOUNDATION displacement and dynamic reaction at interfaces.
    call foundation_response_point(rpm,rad,rsp,mte)
!
!       response positions
    do i = 1,np
!         check for normal or support position
      if(.not. (prs(i) .lt. 0._lrk)) then
!           normal > 0
        j = ifidxof(prs(i),0)
      else
!           support < 0
        if (pp .le. 0) then
!             number of support <= 0
          errmsg = femsgf(99, nmm,6,8,0)
          return
        end if
!           check support index
        m = abs(prs(i))
        if (m .le. 0 .or. m .gt. pp) then
!             invalid support index
          errmsg = femsgf(99, nmm,6,8,0)
          return
        end if
!           x1,z1 -> x2,z2
        o = (m-1)*2
        j = dm+o
      end if
!         x index
      m = j+1
!         z index
      o = m+1
!
!         check in radian (default) deg -> rad
      rad = riff(.not. isd,ori(i),toradf(ori(i)))
!         rotate coordinates x,z on rad -> xr,zr
      call vrotate(rsp(m,1),rsp(o,1),rad,xr,zr)
!         response coordinate 1=horizontal, 2=vertical
      o = nint(desp(i))
!         result complex amplitudes
      if(o .eq. 1) then
        amp(i,k) = xr
      else if(o .eq. 2) then
        amp(i,k) = zr
      else
!           no orientation
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
!
!       new rpm
    rpm = rpm+ndr
!
!       end speed loop
  end do
!
  nr = k
!     output
!
!     automatic response analysis
  call maxamp(np,nr,cn,rpg,amp,vf,vr,vm,st,mxp,mte,errmsg,oki)
  if(oki .lt. 0) return
!
!     output (full number of modes = dim)
!     added false, no variable bearing params
!     will set ok
  call s_resp_f(nm,np,nr,cn,py,desp,ori,rpg,amp,vf,vr,vm,&
  &st,mxp,mte,std,frl,plt,errmsg,ok)
!
  return
!
!     sub end
!
end subroutine resp_f
!
