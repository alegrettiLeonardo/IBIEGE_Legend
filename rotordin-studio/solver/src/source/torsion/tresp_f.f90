!     $Id$
!     ==================================================================
!
!>    @file tresp_f.f
!>    @brief torsion frequency response
!>    last changes:<br>
!>    new file - francisco - feb-20<br>
!>    changed mxb = 99 - francisco - apr-20<br>
!>    updated screen feedback to central routine in saidas.f - francisco
!
!     ==================================================================
!>    @brief torsion frequency respose calculation.
!
!>    @param[in] im torsion model index
!>    @param[in] anrd plot output angular displacement in radian
!>    @param[in] lgp plot output logarithmic angular displacement
!>    @param[in] std standard input output
!>    @param[in] plt HPGL output
!>    @param[out] errmsg return an error message
!>    @param[out] ok return flag. unsuccessful if <0.
!
subroutine tresp_f(im,anrd,lgp,std,plt,errmsg,ok)
  use rd_textfun, only: femsgf, fomsgf
  use com_mdampso, only: mdoff
  use com_sec, only: n, y, nt, nn
  use com_sprat, only: crat
  use com_tepm, only: rfi, rlam, ddm
  use com_tmdm, only: tmd
  use com_unb, only: nini_r, nfin_r, dw_r
  use com_unb0, only: pr, desp, ori, np, nm
  use com_unb1, only: ndd, mu, ed, tpf, nb
  use rd_kinds, only: lrk, wp
  implicit none
!
!     arguments
  integer :: im, ok
  character(len=99) :: errmsg
  logical :: anrd, lgp, std, plt
!
!     locals
  integer :: oki, ptotal
  character(len=1) :: blank, c1
  character(len=3) :: cbd
  character(len=7) :: nmm
  character(len=9) :: cf
  parameter(blank = ' ',c1 = 'N',cbd = 'mtr',&
  &nmm = 'tresp_f',cf = '(f6.0,6a)')
  logical :: eok
!
!     all sections (divisions)
  integer :: mts
  parameter (mts = 999)
!
!     speed ratio
  integer :: mtt
  parameter (mtt = mts)
!
!     unbalance
  integer :: mxb, mxp
  parameter (mxb = 99,mxp = 9)
!     tpf type of excitation force
!     -1:transient torque,0:unbalance (default),1:concentrated,
!      2:harmonic torque,3:static torque,4:freq. resposnse
!
!     torsion modal space
  integer :: mtg
  parameter (mtg = 500)
!
!     mode offset
!     mode offset
!
!     modal damping
!
  integer :: mxd
  parameter (mxd = 99)
!
!     speed step
  real(lrk) :: ndr, rpm, rad, rpm2radf
  integer :: i, j, k, l, ll, pdd, prs, mxm, mxr, nrp, nmd, nmdt
  dimension pdd(mxd),prs(mxp)
  parameter (mxm = 10,mxr = 100)
  complex(wp) :: j0, j1, r1, pmn
  parameter (j0 = (0,0),j1 = (0,1),r1 = (1,0))
!
!     speed and amplitude vectors
  integer :: mtr
  parameter (mtr = mtg)
!
  real(lrk) :: rpg, py, reye, rdr, tqr, str
  real(wp) :: rd
  complex(wp) :: clam, cfi, qq, m1, rsp, amp
  dimension rpg(mtr),py(mxp),&
  &reye(mtg,mtg),clam(mtg,mtg),cfi(mtg,mtg),&
  &qq(mtg,1),m1(mtg,mtg),rsp(mtg,mtg),amp(mxp,mtr),&
  &rd(mtr),rdr(mxp,mtr),tqr(mxp,mtr),str(mxp,mtr)
!
  intrinsic :: exp, min
!
!     return
  ok = -1
!
!     parameters check
!     speed delta
  if (dw_r .le. 0) then
!       speed <= 0
    errmsg = femsgf(99, nmm,6,4,0)
    return
  end if
!     speed range
  if ((nfin_r-nini_r) .le. 0._lrk) then
!       speed range <= 0
    errmsg = femsgf(99, nmm,6,5,0)
    return
  end if
!     response positions
  if (np .le. 0._lrk) then
!       no response positions
    errmsg = femsgf(99, nmm,6,7,0)
    return
  end if
!
!     number of modes
!     on entrada.f, number of modes (complex -> 2x)
  nmd = nm/2
!
!     response number of modes
  if (nmd .lt. 1 .or. nmd .gt. ddm) then
    nmd = min(mxm,ddm)
!       'invalid number of modes' !26
    call elmsgw(0,26,nmm)
  end if
!
!     subtract mode offset
  nmdt = nmd-mdoff
!
!     number of modes to consider
  if (nmdt .lt. 1) then
!       number of modes to consider < 1
    errmsg = femsgf(99, nmm,6,25,0)
    return
  end if
!
!     speed step (rpm).  UNBFD shares /unb/ with the lateral
!     response and therefore has the same contract: increment in rpm.
  ndr = dw_r
!
!     initial speed
  rpm = nini_r
!
!     search nearest unbalance section position -> pdd
  call inddbl(y,ndd,pdd,nb,nt,mxb,mxd,mts,errmsg,oki)
  if (oki .lt. 0) return
!
!     search nearest response section position -> prs
  call indrsp(y,pr,prs,py,np,nt,mxp,mts,errmsg,oki)
  if (oki .lt. 0) return
!
!     real eye matrix
  call matidn_r(reye,ddm,mtg)
!
!     diagonal double complex lambda matrix, rlam -> clam
  call matdia_rc(rlam,clam,ddm,mtg,mtg)
!
!     complex FI, rfi -> cfi, mode offset, starts on column 1+mdoff
  call copmat2_rct(rfi,cfi,ddm,ddm,mtg,mtg,mtg,mtg,0,mdoff)
!
!     zero response vector
  call zervec_c(qq,ddm,mtg)
!
!     torsion frequency response excitation ok flag
  eok = .false.
!
  do i = 1,nb
!       check for kind of force, 4 -> frequency response
!       see expmod.f, blockd.f
    if (tpf(i) .eq. 4) then
!         there is at least one excitation
      eok = .true.
!         speed ratio
      if (pdd(i) .gt. 1) then
        rad = crat(pdd(i)-1)
      else
        rad = 1
      end if
!         mu -> excitation torque N.m, ed -> phase radian
      pmn = rad*mu(i)*exp(j1*ed(i))
!         model index
      if (im .eq. 1) then
        ll = pdd(i)
      else
        ll = (pdd(i)-1)*2+1
      end if
      qq(ll,1) = qq(ll,1)+pmn
    end if
  end do
!
!     check for at least one freq. response
  if (.not. eok) then
!       no excitation found -> zero response
    errmsg = fomsgf(99, nmm,18,blank,0)
    return
  end if
!
!     speed (rpm) visualization control
  l = 1
!
  ptotal=int((nfin_r-nini_r)/ndr)+1
  if (ptotal .lt. 1) ptotal=1
  call progress_begin('TORSION_FREQUENCY_RESPONSE',ptotal,&
  &'solve torsional response at each speed')
  call progress_stage('TORSION_FREQUENCY_RESPONSE',&
  &'evaluate modal receptance and response')
!
!     speed loop
  k = 0
!
  do while (rpm .le. nfin_r)
!       iteration count
    k = k+1
!
!       check output dimension
    if (k .gt. mtr) then
      errmsg = fomsgf(99, nmm,4,cbd,0)
      call progress_end('TORSION_FREQUENCY_RESPONSE','FAILED')
      return
    end if
!
    if (rpm .ge. (mxr*l) .and. .not. std) then
!         screen speed + backspace
      call bprint(rpm,0,cf,6)
      l = l+1
    end if
!
!       rpm -> rad/s
    rad = rpm2radf(rpm)
!
!       receptance matrix hh -> m1, thhmat.f
    call thhmat(rad,reye,tmd,cfi,clam,m1,nmd,mdoff,ddm,mtg)
!
!       multiply m1 * qq -> rsp
    call ZGEMM (c1,c1,ddm,1,ddm,r1,m1,mtg,qq,mtg,j0,rsp,mtg)
!
!        convert complex angular displacent amplitude -> rd
    do j = 1,ddm
      rd(j) = abs(rsp(j,1))
    end do
!
!       response positions
    do i = 1,np
!         model index, response index -> ll
      if (im .eq. 1) then
        ll = prs(i)
      else
        ll = 2*prs(i)-1
      end if
!
!         speed ratio
      if (prs(i) .gt. 1) then
        rad = crat(prs(i)-1)
      else
        rad = 1
      end if
!         complex amplitude with speed ratio -> amp
      amp(i,k) = rad*rsp(ll,1)
!         response division index
      j = prs(i)-1
!         torque and displacement on element
!         between two divisions -> rdr,tqr,str (ttrqdiv.f)
      call tdvtrst(im,j,mtr,rd,rdr(i,k),tqr(i,k),str(i,k))
    end do
!
!       rpm speed to graphics
    rpg(k) = rpm
    call progress_update('TORSION_FREQUENCY_RESPONSE',k,ptotal,&
    &rpm,'RPM')
!
!       new rpm
    rpm = rpm+ndr
!
!       end speed loop
  end do
!
!     number of data points
  nrp = k
!
!     text output will set ok
  call progress_stage('TORSION_FREQUENCY_RESPONSE',&
  &'write torsional frequency-response output')
  call ts_resp_f(nmdt,np,nrp,py,rpg,amp,rdr,tqr,str,&
  &mxp,mtr,anrd,lgp,std,plt,errmsg,ok)
  if (ok .lt. 0) then
    call progress_end('TORSION_FREQUENCY_RESPONSE','FAILED')
    return
  end if
  call progress_end('TORSION_FREQUENCY_RESPONSE','OK')
!
  return
!
end subroutine tresp_f
!
