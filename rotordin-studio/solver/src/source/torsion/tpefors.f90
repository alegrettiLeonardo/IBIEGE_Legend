!     $Id$
!     ==================================================================
!
!>    @file tpefors.f
!>    @author francisco
!>    @date 11-feb-20
!>    @brief torsional periodic forced response, last changes:<br>
!>     new file - feb-20<br>
!>     changed mxb = 99 francisco - apr-20.
!
!     ==================================================================
!>    @brief torsional periodic forced response.
!>    always cosine type.
!
!>    @param[in] im torsion model index
!>    @param[in] anrd plot output angular displacement in radian
!>    @param[in] std standard input output
!>    @param[in] plt HPGL output
!>    @param[out] errmsg return an error message
!>    @param[out] ok return flag. unsuccessful if <0.
!
subroutine tpefors(im,anrd,std,plt,errmsg,ok)
  use rd_textfun, only: femsgf, fomsgf
  use com_mdampso, only: mdoff
  use com_sec, only: n, y, nt, nn
  use com_sprat, only: crat
  use com_tepm, only: rfi, rlam, ddm
  use com_thmfr, only: thfr
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
  logical :: anrd, std, plt
!
!     locals
  integer :: oki
  character(len=1) :: c1
  character(len=3) :: cbd
  character(len=7) :: nmm
  parameter(c1 = 'N',cbd = 'mxb',nmm = 'tpefors')
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
  integer :: mxr, mxb
  parameter (mxr = 9, mxb = 99)
!
!      tpf type of excitation force
!     -1:transient torque,0:unbalance (default),1:concentrated,
!     2:harmonic torque,3:static torque,4:freq. response,5:dist. force
!     torsion harmonic excitation frequency (rad/s)
!     or force offset for kind = 1 (m)
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
  real(lrk) :: rad, reye, pse, wra
  integer :: i, j, k, l, ll, l1, idhr, nhr, nmd, nmdt, pdd
  dimension pse(mxb),wra(mxb),pdd(mxb),reye(mtg,mtg),idhr(mxb)
!
!     harmonic frequencies unique values
  integer :: wui, wni, wnui
  dimension wui(mxb,mxb),wni(mxb)
!
  complex(wp) :: clam, cfi, qq, rsp, hrs, m1, j0, j1, r1, pmn
  dimension clam(mtg,mtg),cfi(mtg,mtg),&
  &qq(mtg,1),m1(mtg,mtg),rsp(mtg,1),hrs(mtg,mxb)
!
  intrinsic :: exp, cmplx
!
!     return
  ok = -1
!
  j0 = cmplx(0._wp,0._wp, kind=wp)
  j1 = cmplx(0._wp,1._wp, kind=wp)
  r1 = cmplx(1._wp,0._wp, kind=wp)
!
!     parameters check
!     number of modes
  nmd = nm
!
!     check number of modes
  if (nmd .lt. 1 .or. nmd .gt. ddm) then
    nmd = ddm
!       warning
    call elmsgw(0,26,nmm)
  end if
!
!     subtract mode offset
  nmdt = nmd-mdoff
!
!     check number of modes to consider
  if (nmdt .lt. 1) then
    errmsg = femsgf(99, nmm,6,25,0)
    return
  end if
!
!     get harmonic excitations
  nhr = 0
  do i = 1,nb
!       check bounds
    if (i .gt. mxb) then
!         out of bounds
      errmsg = fomsgf(99, nmm,4,cbd,0)
      return
    end if
!       check for kind of force, 2 -> harmonic excitation
!       see expmod.f, blockd.f
    l1 = tpf(i)
    if (l1 .eq. 2) then
      nhr = nhr+1
!
!         check bounds
      if (nhr .gt. mxb) then
!           out of bounds
        errmsg = fomsgf(99, nmm,4,cbd,0)
        return
      end if
!
!         keep excitation index
      idhr(nhr) = i
!         excitation position
      pse(nhr) = ndd(i)
!         excitation frequency (rad/s)
      wra(nhr) = thfr(i)
    end if
  end do
!
!     check for at least one harmonic excitation
  if (nhr .eq. 0) then
!       none harmonic
    errmsg = femsgf(99, nmm,6,28,0)
    return
  end if
!
!     search nearest harmonic excitation section positions pse -> pdd
  call inddbl(y,pse,pdd,nhr,nt,mxb,mxb,mts,errmsg,oki)
  if (oki .lt. 0) return
!
!     get unique harmonic excitation frequencies (rad/s)
  call unique(wra,wui,wni,wnui,nhr,mxb,1e-3_lrk)
!
  call progress_begin('TORSION_HARMONIC_RESPONSE',wnui,&
  &'solve unique torsional harmonic frequencies')
!     real eye matrix -> reye
  call matidn_r(reye,ddm,mtg)
!
!     diagonal double complex lambda matrix, rlam -> clam
  call matdia_rc(rlam,clam,ddm,mtg,mtg)
!
!     complex FI, rfi -> cfi, mode offset, starts on column 1+mdoff
  call copmat2_rct(rfi,cfi,ddm,ddm,mtg,mtg,mtg,mtg,0,mdoff)
!
!     unique frequency loop. maximum bound is nhr (mxb)
  do k = 1,wnui
!
!       zero response vector -> qq
    call zervec_c(qq,ddm,mtg)
!
!       indices same frequency
    l1 = wni(k)
    do i = 1,l1
!         harmonic excitation vector position index
      j = wui(k,i)
!         global excitation index
      l = idhr(j)
!         speed ratio, position pdd is already for harmonic excitation
      if (pdd(j) .gt. 1) then
        rad = crat(pdd(j)-1)
      else
        rad = 1
      end if
!         mu -> excitation torque N.m, ed -> phase radian
      pmn = rad*mu(l)*exp(j1*ed(l))
      if (im .eq. 1) then
        ll = pdd(j)
      else
        ll = (pdd(j)-1)*2+1
      end if
!         excitation vector for unique frequency
      qq(ll,1) = qq(ll,1)+pmn
    end do
!
!       unique frequency index
    j = wui(k,1)
!       system current frequency (rad/s)
    rad = wra(j)
!
!       receptance matrix hh -> m1, thhmat.f
    call thhmat(rad,reye,tmd,cfi,clam,m1,nmd,mdoff,ddm,mtg)
!
!       multiply m1 * qq -> rsp
!       system relative angular displacemnt for current frequency
    call ZGEMM (c1,c1,ddm,1,ddm,r1,m1,mtg,qq,mtg,j0,rsp,mtg)
!
!       save complex result for each unique frequency rsp -> hrs
    do j = 1,ddm
      hrs(j,k) = rsp(j,1)
    end do
    call progress_update('TORSION_HARMONIC_RESPONSE',k,wnui,rad,&
    &'RAD_S')
!       unique frequency loop
  end do
!
!     analysis of complex displacement angles, each frequency
!     will set ok
  call progress_stage('TORSION_HARMONIC_RESPONSE',&
  &'write harmonic response output')
  call tpefora(im,ddm,idhr,pse,wra,wui,wnui,hrs,&
  &nhr,mxb,mtg,anrd,std,plt,errmsg,ok)
  if(ok.eq.0) call progress_end('TORSION_HARMONIC_RESPONSE','OK')
!
  return
!
end subroutine tpefors
!
