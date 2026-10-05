!     $Id$
!     ==================================================================
!
!>    @file tpefora.f
!>    @author francisco
!>    @date 11-feb-20
!>    @brief torsional periodic forced response analysis, last changes:<
!>     new file - feb-20<br>
!>     changed mxb = 99 francisco - apr-20.
!
!     ==================================================================
!>    @brief torsion harmonic response analysis
!
!>    @param[in] im torsion model index
!>    @param[in] ddm complete torsion matrices dimension
!>    @param[in] idhr position index main input
!>    @param[in] pse harmonic input positions
!>    @param[in] wra excitation frequency (rad/s)
!>    @param[in] wui number o unique frequencies
!>    @param[in] wnui unique frequencies indices
!>    @param[in] hrs complex harmonic response per unique frequency
!>    @param[in] nhr number of harmonic input positions
!>    @param[in] mxbe harmonic input positions dimension, same as mxb
!>    @param[in] mtg complex harmonic response dimension
!>    @param[in] anrd plot output angular displacement in radian
!>    @param[in] std standard input output
!>    @param[in] plt HPGL output
!>    @param[out] errmsg return an error message
!>    @param[out] ok return flag. unsuccessful if <0.
!
subroutine tpefora(im,ddm,idhr,pse,wra,wui,wnui,hrs,&
&nhr,mxbe,mtg,anrd,std,plt,errmsg,ok)
  use rd_textfun, only: fomsgf
  use com_sec, only: n, y, nt, nn
  use com_sprat, only: crat
  use com_thmfr, only: thfr
  use com_tpfrt, only: fprt, fprts
  use com_unb0, only: pr, desp, ori, np, nm
  use com_unb1, only: ndd, mu, ed, tpf, nb
  use rd_kinds, only: lrk, wp
  implicit none
!
!     arguments
  integer :: im, ddm, idhr, nhr, mxbe, mtg, ok
  real(lrk) :: pse, wra
  dimension idhr(mxbe),pse(mxbe),wra(mxbe)
!     harmonic frequencies unique values
  integer :: wui, wnui
  dimension wui(mxbe,mxbe)
  complex(wp) :: hrs
  dimension hrs(mtg,mxbe)
  character(len=99) :: errmsg
  logical :: anrd, std, plt
!
!     locals
  character(len=3) :: cbd
  character(len=7) :: nmm
  character(len=10) :: c10
  parameter(cbd = 'mth',nmm = 'tpefora',c10 = 'fprt/fprts')
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
!     periodic force response simulation time and time step
!
  integer :: j, k, l, l1, m, o, prs, di, iiff, oki
  real(lrk) :: py, tt, aa, aa1, va, va1, fi, fi1, ww, am, am1, sr, rd, tq, st, ad, tqdiv1f, rargf
  complex(wp) :: ca, ca1
  dimension py(mxr),prs(mxr)
!
!     input output time harmonic response
  integer :: mth, ntp, pui, pni, pnui
  parameter (mth = 9999)
!
  real(lrk) :: htv, hmv, hit, hav, htq, hst, hmt, hta, hma
  dimension htv(mth),hmv(mxr),hit(mth,mxb),hav(mth,mxr),&
  &htq(mth,mxr),hst(mth,mxr),hta(mth,mxr),hmt(mxr),hma(mxr)&
!     excitation position unique values
  &,pui(mxb,mxb),pni(mxb)
!
  intrinsic :: abs, aimag, cos, real
!
!     return
  ok = -1
!
!     check  fprt->final time / fprts -> time step
  if (fprt .le. 0 .or. fprts .le. 0 .or. fprts .gt. fprt) then
!       'invalid data ' !8
    errmsg = fomsgf(99, nmm,8,c10,0)
    return
  end if
!
!     search nearest response section position
  call indrsp(y,pr,prs,py,np,nt,mxr,mts,errmsg,oki)
  if (oki .lt. 0) return
!
!     get unique harmonic input positions
!     1 mm precision, same as in predad subroutine
  call unique(pse,pui,pni,pnui,nhr,mxb,1e-3_lrk)
!     pui unique values indices
!     pni number of occurrences for each unique number
!     pnui number of unique values
!
!     time harmonic input torque -> hit
!
!     time loop
  k = 0
!     initial time (s)
  tt = 0
!
  do while (tt .le. fprt)
!
!       iteration count
    k = k+1
    if (k .gt. mth) then
!         'index out of bounds' !5
      errmsg = fomsgf(99, nmm,5,cbd,0)
      return
    end if
!
!       current time -> htv
    htv(k) = tt
!
!       unique positions
    do j = 1,pnui
!         Fourier sum per unique excitation position
      aa = 0
!         elements of j unique position
      l1 = pni(j)
      do l = 1,l1
        !         local position index
        m = pui(j,l)
!           global position index
        o = idhr(m)
!           torque value (N.m)
        va = mu(o)
!           phase (rad)
        fi = ed(o)
!           excitation frequency (rad/s)
        ww = thfr(o)
        am =  va*cos(ww*tt+fi)
        aa = aa+am
!           Fourier loop
      end do
!         harmonic time input -> hit
      hit (k,j) = aa
!         position loop
    end do
!
!       new time
    tt = tt + fprts
!
!       check output dimension
    if (k .gt. mth) then
      errmsg = fomsgf(99, nmm,4,cbd,0)
      return
    end if
!
!       end time loop
  end do
!
!     number of time data points -> ntp
  ntp = k
!
!     zero max division response
  call zervec_r(hmv,np,mxr)
!     zero max torque
  call zervec_r(hmt,np,mxr)
!     zero max response
  call zervec_r(hma,np,mxr)
!
!     time interval
  do k = 1,ntp
!
!       time step
    tt = htv(k)
!
!       response positions
    do j = 1, np
!         position index
      m = prs(j)
!         model index
      if (im .eq. 1) then
        l = m
!           next division to get angle
        l1 = iiff(l.lt.ddm,l+1,l-1)
      else
        l = (m-1)*2+1
        l1 = iiff(l.lt.ddm,l+2,l-2)
      end if
!         division id
      di = iiff(l.lt.ddm,m,m-1)
!         speed ratio
      if (prs(j) .gt. 1) then
        sr = crat(prs(j)-1)
      else
        sr = 1
      end if
!
!         Fourier sum
      aa = 0
      aa1 = 0
!         unique frequencies loop
      do m = 1,wnui
!           complex displacement angle value on response position divisi
        ca = hrs(l,m)
!           complex displacement angle value next division to response p
        ca1 = hrs(l1,m)
!           displacement angle amplitude -> va
        va = real(abs(ca), lrk)
        va1 = real(abs(ca1), lrk)
!           displacement angle -> fi
        fi = rargf(ca)
        fi1 = rargf(ca1)
!           unique frequency index
        o = wui(m,1)
!           cos frequency (rad/s)
        ww = wra(o)
!           harmonic time amplitude
        am = sr*va*cos(ww*tt+fi)
        am1 = sr*va1*cos(ww*tt+fi1)
!           sum of time harmonic amplitudes
        aa = aa+am
        aa1 = aa1+am1
!           frequency loop
      end do
!         time angular displacement amplitude output->hav
      hav(k,j) = aa
!         twist angle on element
      ad = aa-aa1
!         torque,stress,safety on division -> tq,st
      rd = tqdiv1f(di,ad,tq,st)
!         time angular relative displacement amplitude on division -> ht
      hta(k,j) = rd
!         time torque on division -> htq
      htq(k,j) = tq
!         time stress on division -> hst
      hst(k,j) = st
!         max angular division displacement value -> hmv
      if (abs(ad) .gt. hmv(j)) hmv(j) = abs(ad)
!         max torque value -> hmt
      if (abs(tq) .gt. hmt(j)) hmt(j) = tq
!         max total angular displacement value -> hma
      if (abs(aa) .gt. hma(j)) hma(j) = abs(aa)
!         response position loop
    end do
!       time step loop
  end do
!
!     output will set ok
  call ts_pefor(im,np,ntp,pse,pui,pnui,wnui,wui,wra,&
  &pr,idhr,crat,&
  &hrs,htv,hmv,hit,hav,htq,hst,hta,hmt,hma,&
  &mth,mtt,mtg,mxb,mxr,anrd,std,plt,errmsg,ok)
!
  return
!
end subroutine tpefora
!

