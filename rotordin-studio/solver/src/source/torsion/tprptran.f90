!     $Id$
!     ==================================================================
!
!>    @file tprptran.f
!>    @author francisco
!>    @date 03-mar-2020
!>    @brief torsion numerical time integration data preparation, last c
!>     new file - mar-20<br>
!>     changed mxb = 99 francisco - apr-20.
!
!     ==================================================================
!>    @brief check for some minimum damping at main diagonal of
!>     torsion damping matrix.
!
!>    @return true if there is some damping above minimum
!>     on the main diagonal of torsion damping matrix
!
logical function chkdmpf(dm,mtg,tc1)
  use rd_kinds, only: lrk, wp
  implicit none
!
!     dimnesion,maximal number of global matrices elements
  integer :: dm, mtg
  real(wp) :: tc1
  dimension tc1(mtg,mtg)
!
!     minimum torsion damping, minimum transient torque amplification
  real(lrk) :: dmp, mndmp
  parameter (mndmp = 1e-3_lrk)
  integer :: i
  logical :: chkdmp
!
  intrinsic :: real
!
  chkdmp = .false.
  do i = 1,dm
!       standard damping plus modal
    dmp = real(tc1(i,i), lrk)
!       check for minimum damping
    chkdmp = dmp .gt. mndmp
!       exit if ok
    if (chkdmp) exit
  end do
!
  chkdmpf = chkdmp
!
  return
!
end function chkdmpf
!
!     ==================================================================
!>    @brief prepare data for transient analysis.
!
!>    @param[in] im torsion model index
!>    @param[in] anrd plot output angular displacement in radians
!>    @param[in] stdio standard input output
!>    @param[in] plt HPGL output
!>    @param[out] errmsg return an error message
!>    @param[out] ok return flag. unsuccessful if <0.
!
subroutine tprptran(im,anrd,stdio,plt,errmsg,ok)
  use rd_textfun, only: fomsgf
  use com_mdd, only: qtd_modos, porb, rorb, rang
  use com_sec, only: n, y, nt, nn
  use com_tmat, only: tj, tk, tc, dm
  use com_tmdm, only: tmd
  use com_ttraneq, only: tequt
  use com_ttransc, only: ttris, ttrrs, ttrit, ttret
  use com_ttransd, only: ttrmd, ttrdtt, ttrdta, nttra, ttrdsz, iseqt
  use com_unb1, only: ndd, mu, ed, tpf, nb
  use rd_kinds, only: lrk, wp
  implicit none
!
  character(len=99) :: errmsg
  integer :: im, ok
  logical :: anrd, stdio, plt
!
!     all sections (divisions)
  integer :: mts
  parameter (mts = 999)
!
!     frequency response and unbalance
  integer :: mxr, mxb
  parameter (mxr = 9, mxb = 99)
!
!     ori -> radian
!     kind of exc, see expgeo.f
!
!     modes
!     time orbit position
!
!     maximal number of global matrices elements
  integer :: mtg
  parameter (mtg = 500)
!
!     global torsion matrices:inertia tj, stiffness tk, damping tc
!
!     modal damping
!
!     transient
  integer :: mxttra, mxttrd
  parameter (mxttra = 5,mxttrd =10000)
!     number of transients, number of data points
!     integration step, result step, integration init,integration end
!     damping,time,torque amplitude
!     torque equation
!
  integer :: ii, jj, oki, no, imt, m1, m2, di, tid, nte, ntr, nrstps, iiff
!>    imt numerical integration method
!>     0 = Newmark 1 = Wilson-theta
  data nrstps /0/
  real(lrk) :: prec, minamp, std, mean, dm1, stddvf
!     integration step,view step,initial integration,final integration,p
  real(lrk) :: its, vts, iit, fit, tpd, mndmp
!     minimum torsion damping, minimum transient torque amplification
  character(len=1) :: blank
  character(len=6) :: cnm
  character(len=8) :: nmm
  dimension cnm(3)
!
  parameter (imt = 1, prec = 1e-12_lrk,minamp = 1e-3_lrk,mndmp = 1e-3_lrk,blank = ' ' ,nmm = 'tprptran',cnm = &
    & (/'mxr   ','mxttra','t.tran'/))
!
  integer :: mxd
  parameter (mxd = 99)
!
!     tlm = transient load matrix, 1st column time, next load values
!     one more than maximum torsion transient excitations
  real(lrk) :: py, po, tlm, dti, restm, resum, iptld, tq, st, restq, resst, resad, tqm, tqdiv1f
  real(wp) :: tc1
  integer :: pdd, prs, ite, itr, epi, mxttrs, mxrd
  parameter (mxttrs = 5*mxttrd,mxrd = mxttrs*mxr)
  dimension py(mxr),po(mxr),tlm(mxttrd,mxttra+1),&
  &dti(mxttrd),pdd(mxd),&
  &prs(mxr),ite(mxb),epi(mxttra),itr(mxr),tc1(mtg,mtg),&
  &tqm(mxttra),&
!      result time, result displacent, input load
  &restm(mxttrs),resum(mxttrs,mxr),iptld(mxttrs,mxr),&
  &restq(mxttrs,mxr),resst(mxttrs,mxr),resad(mxttrs,mxr)
!     init resum,iptld
  data resum/mxrd*0/,iptld/mxrd*0/
!
!     check for minimum damping
  logical :: chkdmpf
  data dm1/0/
!
  intrinsic :: abs
!
!     return init
  ok = -1
!
!     check for transient time data
  if (ttrdsz .le. 0) then
    errmsg = fomsgf(99, nmm,30,blank,0)
    return
  end if
!
!     search nearest unbalance section position -> pdd
  call inddbl(y,ndd,pdd,nb,nt,mxb,mxd,mts,errmsg,oki)
  if (oki .lt. 0) return
!
!     search for section position
!     nearest of orbit position
  no = 1
  po(1) = porb
  call indorb(y,po,prs,py,no,nt,mxr,mts,errmsg,oki)
  if (oki .lt. 0) return
!
!     copy damping matrix tc -> tc1
  call copmat1_d(tc,tc1,dm,dm,mtg,mtg,mtg,mtg)
!
!     sum modal damping tc1+tmd -> tc1
  call sommat_drd(tc1,tmd,tc1,dm,dm,mtg,mtg)
!
!     torque time equation time step
  if (iseqt) then
    dm1 = (ttret-ttrit)/(ttrdsz-1)
  end if
!
!     copy time vector to time load matrix ttrdtt(:) -> tlm(:,1)
  do ii = 1,ttrdsz
!       torque time equation
    if (iseqt) then
!         equation time step
      ttrdtt(ii) = ttrit+(ii-1)*dm1
    end if
!       time data
    tlm(ii,1) = ttrdtt(ii)
!       delta t
    if (ii .gt. 1) then
      dti(ii-1) = tlm(ii,1)-tlm(ii-1,1)
    end if
  end do
!
!     number of time deltas
  jj = ttrdsz-1
!     standard deviation and mean of input time delta
  std = stddvf(dti,jj,mxttrd,mean,dm1)
!
!     reduce step if std dev is high
  dm1 = 5*std/mean
!     one fourth of mean time step
  if (dm1 .le. 1) dm1 = 4
!     integration step
  its = mean/dm1
!     view step
  vts = mean/dm1/4
!     initial integration time
  iit = ttrdtt(1)
!     final integration time
  fit = ttrdtt(ttrdsz)
!     time period
  tpd = fit-iit
!
!     override default integration step
  if(ttris .gt. 0) its = ttris
!     override default result step
  if(ttrrs .gt. 0) then
!       result step should be greater than integration step
    if(ttrrs .gt. its) then
!         override default result step
      vts = ttrrs
    else
!         same as integration step
      vts = its
    end if
  end if
!     override default initial integration time
  if(ttrit .gt. 0) iit = ttrit
!     override default final integration time
  if(ttret .gt. 0) fit = ttret
!
!     number of time excitations
  nte = 0
!
!     loop on unbalance loads
  do ii = 1,nb
!       unbalance kind
    jj = tpf(ii)
!
!       check for transient kind
    if (jj .lt. 0) then
!         transient kind
!         torque load index
      tid = abs(jj)
!         transient load count
      nte = nte+1
!         check excitation bound
      if (nte .gt. mxttra) then
        errmsg = fomsgf(99, nmm,4,cnm(2),0)
        return
      end if
!         check load index
      if (tid .gt. nttra .or. nte .gt. nttra) then
        errmsg = fomsgf(99, nmm,8,cnm(3),0)
        return
      end if
!         excitation section index
      m1 = pdd(ii)
!         torsional model 1 = 2 nodes, 2 = three nodes
      if (im .eq. 1) then
!           model index
        jj = m1
      else if (im .eq. 2) then
        jj = (m1-1)*2+1
      end if
!         excitation position index
      epi(nte) = ii
!         excitation coordinate position vector
      ite(nte) = jj
!         used defined damping
      std = ttrmd(tid)
!         add defined damping on damping matrix
      tc1(jj,jj) = tc1(jj,jj)+std
!         get torque multiplier
      std = mu(ii)
!         check for near zero, minamp
!         'no or low excitation' !28
      if (std .lt. minamp) call elmsgw(0,28,nmm)
      tqm(nte) = std
!         torque time equation time step
      if (iseqt) then
!           recursive time function evaluator (evalf.f)
!           torque data -> ttrdta
        call tgntp(tid,std,ttrdsz,mxttrd,mxttra,&
        &ttrdtt,ttrdta,tequt(tid),errmsg,oki)
        if (oki .lt. 0) return
      end if
!         prepare torque load data
!         torque values for given index
!         copy transient load vector to time load matrix
!         ttrdta(:,tid+1) -> tlm(:,nte+1)
      do jj = 1,ttrdsz
!           apply multiplier std
        tlm(jj,nte+1) = ttrdta(tid,jj)*std
      end do
!         excitation kind if
    end if
!       excitation loop
  end do
!
!     check for minimum value of damping
  if (.not. chkdmpf(dm,mtg,tc1)) then
!       warning message
    call elmsgw(0,30,nmm)
!       first excitation section index
    m1 = ite(1)
!       torsional model 1 = 2 nodes, 2 = three nodes
    if (im .eq. 1) then
!         model index
      jj = m1
    else if (im .eq. 2) then
      jj = (m1-1)*2+1
    end if
!       add defined minimum damping on damping matrix
    tc1(jj,jj) = mndmp
  end if
!
!     check for transient excitations
  if (nte .gt. 0) then
!       have excitation
!
!       check if work with periodic function
    if (fit .gt. tpd) then
!         check for "strictly "whole period f(1) = f(T)
      do ii = 1,nte
        jj = ii+1
        dm1 = abs(tlm(1,jj)-tlm(ttrdsz,jj))
        if (dm1 .gt. prec) then
          errmsg = fomsgf(99, nmm,29,cnm(3),0)
          return
        end if
      end do
    end if
!
!       twist calculation needs two coordinates (element)
    ntr = 2*no
!
!       check for result capacity
    if (ntr .gt. mxr) then
      errmsg = fomsgf(99, nmm,4,cnm(1),0)
      return
    end if
!
!       loop over response positions
    do ii = 1,no
!         response index
      jj = prs(ii)
!         torsional model 1 = 2 nodes, 2 = three nodes
      if (im .eq. 1) then
!           model index
        m1 = jj
!           next division to get angle
        m2 = iiff(m1.lt.dm,m1+1,m1-1)
      else if (im .eq. 2) then
        m1 = (jj-1)*2+1
        m2 = iiff(m1.lt.dm,m1+2,m1-2)
      end if
!         index in pairs
      jj = (ii-1)*2+1
!         two consecutive responses to calculate twist on element
      itr(jj) = m1
      itr(jj+1) = m2
    end do
!        check for transient excitations if
  else
!       have no transient excitation
    errmsg = fomsgf(99, nmm,28,cnm(1),0)
    return
  end if
!
!     numerical integration
!     remember results are indexed, not at coordinate
  call tnuintegu(tj,tc1,tk,dm,ttrdsz,tpd,tlm,&
  &ite,nte,iit,fit,its,imt,itr,ntr,&
  &vts,nrstps,restm,resum,iptld,&
  &mtg,mxr,mxttrd,mxttra,mxttrs,errmsg,oki)
  if (oki .lt. 0) return
!
!     twist and torque calulation
!     loop over response positions
  do ii = 1,no
!       response index
    jj = prs(ii)
!       torsional model 1 = 2 nodes, 2 = three nodes
    if (im .eq. 1) then
      m1 = jj
    else if (im .eq. 2) then
      m1 = (jj-1)*2+1
    end if
!       division id
    di = iiff(m1.lt.dm,jj,jj-1)
!       pair of indices
    m1 = (ii-1)*2+1
!       time loop
    do jj = 1,nrstps
!         twist on di division -> std
      std = resum(jj,m1)-resum(jj,m1+1)
      resad(jj,ii) = std
!         calculate torque and stress on di divition -> tq,st
      std = tqdiv1f(di,std,tq,st)
!         hold time torque on element di
      restq(jj,ii) = tq
!         hold time shear stress on element di
      resst(jj,ii) = st
    end do
!       result points loop
  end do
!
!     output
  call ts_nuint(iit,fit,its,vts,epi,nte,ndd,tpf,no,imt,py,&
  &ttrdsz,ttrmd,ttrdtt,ttrdta,nrstps,restm,resum,iptld,&
  &restq,resst,resad,tqm,&
  &mxttra,mxb,mxttrd,mxttrs,mxr,&
  &iseqt,anrd,stdio,plt,errmsg,oki)
  if (oki .lt. 0) return
!
!     return ok
  ok = 0
!
  return
!
end subroutine tprptran
!
