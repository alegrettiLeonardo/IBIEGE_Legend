!     $Id$
!     ==================================================================
!
!>    @file tsaidas.f
!>    @brief torsional result output
!>    last changes:<br>
!>    new file dec-19 - f
!>    added equivalent stiffness for static torque - francisco - oct-20<
!>    moved sjoinrf to tmatfun - francisco - apr-21<br>
!>    moved sjoinif to tmatfun - francisco - may-21<br>
!>    added added flexural-torsion optional data flag on tparfile - fran
!>    added added flexural-torsion  output - francisco jul-21<br>
!>    added torsion and bend stress factors on output - francisco - jul-
!>    added plain format job on matrix export - francisco - nov-21.
!>    \verbatim
!>     Added a warning messaging system per section.
!>     Based on octal system with seven levels 0-6.
!>     It is not possible sum same level errors.
!>    \endverbatim
!>     @see flxwarn
!
!     ==================================================================
!>    @brief prepare torsion output file name
!
!>    @param[in] knd output file id, lt 0 for plot
!>    @param[out] lng returns the file name length without spaces
!>    @param[out] ofn returns the output file name,
!>    @see init.f
!>    @see saidas.f
!
subroutine tmntfnm(knd,lng,ofn)
  implicit none
!
!     arguments
  integer :: knd, lng
  character(len=255) :: ofn
!
  integer :: i
  character(len=511) :: buf
  character(len=1) :: dot, sla
  character(len=2) :: tor
  parameter (dot = '.',sla = '/',tor = '_t')
!
  intrinsic :: index, len_trim
!
!     standard call
  call mntfnm(knd,lng,ofn)
!
!     search for last dot ../name.txt
  i = index(ofn,dot,.true.)
  if (i .gt. 2) then
!       dot found
    write(buf,'(3a)') ofn(1:i-1),tor,ofn(i:)
  else
    write(buf,'(2a)') ofn(1:lng),tor
  end if
  lng = len_trim(buf)
  if (lng .gt. 255) lng = 255
  ofn = buf(1:lng)
!
  return
!
end subroutine tmntfnm
!
!     ==================================================================
!>    @brief torsion modes output
!
!>    @param[in] qtm number of desired mode
!>    @param[in] npv number of sections
!>    @param[in] idm desired mode index vector
!>    @param[in] fmd mode frequency vector (rad/s)
!>     already in desired mode index
!>    @param[in] py section position vector (m)
!>    @param[in] tm  mode shape matrix
!>     already in desired mode index
!>    @param[in] xmd modes vector dimension
!>    @param[in] mts section vector dimension
!>    @param[in] mtg modal matrix dimension
!>    @param[in] std standard input output
!>    @param[in] plt HPGL output
!>    @param[out] errmsg return error message
!>    @param[out] ok return flag. unsuccessful if <0.
!
subroutine ts_modos(qtm,npv,idm,fmd,py,&
&tm,xmd,mts,mtg,std,plt,errmsg,ok)
  use rd_textfun, only: fomsgf, txtmsgf
  use com_sta, only: stm
  use rd_kinds, only: lrk
  implicit none
!
!     locals
  integer :: i, j, iu
  integer :: k(2)
  character(len=1) :: c1
  character(len=2) :: c2
  character(len=5) :: c5
  character(len=6) :: cst
  character(len=7) :: c7
  character(len=8) :: nmm
  character(len=10) :: sec
  character(len=14) :: c14
  character(len=25) :: ft
  character(len=50) :: txt
  character(len=255) :: ofn
  dimension txt(2)
  parameter (c1 = '(',c2 = '  ',c5 = '(a20,',c7 = 'E18.10)',&
  &cst = 'stdio',nmm = 'ts_modos',sec = 'tmodeshp',&
  &c14 = '(a,a10,i2,4x))')
!
!     arguments
  integer :: qtm, npv, idm, xmd, mts, mtg, ok
  real(lrk) :: fmd, tm, py
  dimension idm(xmd),fmd(xmd),tm(xmd,mtg),py(mts)
  character(len=99) :: errmsg
  logical :: std, plt
!
!     stamp
!
  intrinsic :: len_trim
!
!     init return
  ok = -1
!
!     not std i/o
  if (.not. std) then
    iu = 12
    call tmntfnm(0,i,ofn)
    open(iu,file=ofn(1:i),err=100)
  else
!       std i/o
    iu = 6
    ofn = cst
    call marksec(sec,0)
  end if
!
  write(iu,5,err=200)stm
  call outdsc(iu)
!     torsion mode shapes
  txt(1) = txtmsgf(50, 129)
  i = len_trim(txt(1))
  write(iu,5,err=200)txt(1)(1:i)
  write(iu,220)
!
!     frequency, 36
  txt(1) = txtmsgf(50, 36)
  k(1) = len_trim(txt(1))
!     rad/s, 108
  txt(2) = txtmsgf(50, 108)
  k(2) = len_trim(txt(2))
  write(iu,25,err=200) (txt(i)(1:k(i)),i = 1,2)
!
!     mode =, 38
  txt(1) = txtmsgf(50, 38)
  k(1) = len_trim(txt(1))
!     f =, 96
  txt(2) = txtmsgf(50, 96)
  k(2) = len_trim(txt(2))
!
  do i = 1,qtm
!       mode index
    j = idm(i)
    write(iu,1000,err=200)&
    &txt(1)(1:k(1)),j,txt(2)(1:k(2)),fmd(i)
  end do
  write(iu,220)
!     relative Angular Displacement
  call fltout(iu,txtmsgf(50, 101))
!     graph Data
  call fltout(iu,txtmsgf(50, 95))
!     format prep
  write(ft,15) c5,qtm,c14
  write(iu,ft,err=200) txtmsgf(50, 42),(c2,txtmsgf(50, 38),idm(i),i=1,qtm)
!
  write(ft,15) c1,qtm+1,c7
!
  do j = 1,npv
!       mode index
    write(iu,ft,err=200) py(j),(tm(i,j),i = 1,qtm)
  end do
!
!     not std i/o
  if (.not. std) then
    close(iu)
  else
    call marksec(sec,1)
  end if
!
!     plot output
  if (plt) then
    if (.not. std) then
!         output file, 0=>-99
      call tmntfnm(-99,i,ofn)
    end if
    call tmshp(qtm,npv,idm,fmd,py,tm,xmd,&
    &mts,mtg,std,ofn)
  end if
!
!     return ok
  ok = 0
  return
!
100 errmsg = fomsgf(99, nmm,1,ofn,1)
  return
!
200 close(iu)
  errmsg = fomsgf(99, nmm,13,ofn,1)
  return
!
5 format(a)
15 format(a,i3,a)
25 format(t1,a,t16,a,t34,a)
220 format()
1000 format(a,1x,i2,1x,a,2E18.10)
!
end subroutine ts_modos
!
!     ==================================================================
!>    @brief torsion static torque angular displacement and shear
!>     stress output
!
!>    @param[in] ic response column index
!>    @param[in] tav division angular displacements
!>    with speed ratio
!>    @param[in] trq toque into divisions
!>    @param[in] tau shear stress over two consecutive sections
!>    @param[in] tsf equivalent stiffness matrix, stiffness,
!>     bearing and torque positions (m).
!>    @param[in] nsf number of calculated equivalent stiffness
!>    @param[in] mtg output vector dimension
!>    @param[in] mrc output vector dimension
!>    @param[in] mxts dimension of tsf vector
!>    @param[in] std standard input output
!>    @param[in] plt HPGL output
!>    @param[out] errmsg return error message
!>    @param[out] ok return flag. unsuccessful if <0.
!
subroutine ts_static(ic,tav,trq,tau,tsf,&
&nsf,mtg,mrc,mxts,std,plt,errmsg,ok)
  use rd_textfun, only: ciff, fomsgf, txtmsgf
  use com_eix2, only: ys
  use com_sec, only: n, y, nt, nn
  use com_sta, only: stm
  use com_tshyi, only: tshyipu
  use com_tstest, only: eqstid, ntest
  use rd_kinds, only: lrk
  implicit none
!
!     arguments
  integer :: ic, nsf, mtg, mrc, mxts, ok
  real(lrk) :: tav, trq, tau, tsf
  dimension tav(mtg,mrc),trq(mtg,mrc),tau(mtg,mrc),tsf(mxts,3)
  character(len=99) :: errmsg
  logical :: std, plt
!
!     locals
  integer :: i, j, k, m, imx, iu, indsecf
  character(len=255) :: ofn
  character(len=50) :: txt, tx1, tx2
  character(len=10) :: sct
  character(len=9) :: nmm
  character(len=8) :: cus
  character(len=6) :: afs
  character(len=5) :: cst
  character(len=3) :: cun
  character(len=1) :: sig, cds
  real(lrk) :: prec, ad, yd, sc, tq, fs, tm, riff, mxv, mnv
  dimension cst(2),cun(2),cds(5),mxv(2)
  logical :: idcoupf
  parameter(prec = 1e-12_lrk, sc = 1e6_lrk,cun = (/'(m)','mts'/),nmm = 'ts_static',sct = 'tstattrq',cds = (/'-','+', &
    & ' ','#','-'/),cst = (/'stdio',' >100'/),cus = '(Nm/rad)')
!
  integer :: mxs
  parameter (mxs = 99)
!
!     shaft data
!     added yield strength - francisco - dec-19
!     see blockd.f
!
!     total number of divisions
  integer :: mts
  parameter (mts = 999)
!
  integer :: mtt
  parameter (mtt = mts)
!
!     all sections (divisions)
!
!     torsional shear yield pu
!     yield strength multiplier
!
!     equivalent stiffness for static
  integer :: mxtstf
  parameter (mxtstf = 5)
!
!     stamp
!
!     ranges
!     rnvl range matrix index of vin in each range
!     rgvl range values matrix minimum and maximum each range
!     hval number of elements in each range
!     nrdm range dimension and number of ranges
  integer :: nrdm, ndrn, ndrg, hval, rnvl
  parameter (nrdm = 8,ndrn = mts*nrdm,ndrg = nrdm*2)
  real(lrk) :: rgvl, ltq
  logical :: tdr
  dimension hval(nrdm),rnvl(nrdm,mts),rgvl(nrdm,2),&
  &ltq(mts)
  data hval/nrdm*0/,rnvl/ndrn*0/,rgvl/ndrg*0/,ltq/mts*0/
!
!     stress log precision limit
  real(lrk) :: lprec
  parameter (lprec = 0.1_lrk)
!
  intrinsic :: abs, log, len_trim, real
!
!     init return
  ok = -1
!
!     not std i/o
  if (.not. std) then
    iu = 12
    call tmntfnm(8,m,ofn)
    open(iu,file=ofn(1:m),err=100)
  else
!       std i/o
    iu = 6
    ofn = cst(1)
    call marksec(sct,0)
  end if
!
  write(iu,5,err=200) stm
  call outdsc(iu)
  txt = txtmsgf(50, 102)
  i = len_trim(txt)
  write(iu,5,err=200) txt(1:i)
  write(iu,220,err=200)
!
  txt = txtmsgf(50, 103)
  tx1 = txtmsgf(50, 110)
  i = len_trim(txt)
  j = len_trim(tx1)
  write(iu,15,err=200) txt(1:i),tx1(1:j)
!
  txt = txtmsgf(50, 104)
  tx1 = txtmsgf(50, 111)
  i = len_trim(txt)
  j = len_trim(tx1)
  write(iu,15,err=200) txt(1:i),tx1(1:j)
!
!     max min
  imx = 0
!     log scale range
  mxv(1) = lprec
  mnv = 1e16_lrk
!     max stress
  mxv(2) = 0
!
!     divisions loop
  do i = 2,nt
!       section index
    k = indsecf(i)
    if (k .le. 0) then
!         section not found
      errmsg = fomsgf(99, nmm,14,cds(3),0)
      return
    end if
!       y starts at zero
    j = i-1
!       relative angular displacement, with speed ratio
    ad = tav(j,ic)
!       shear yeld strength MPa
    yd = ys(k)*tshyipu/sc
!       t/ts = factor of safety
    tm = abs(tau(j,ic))
!       sc = scale (mega = 1e6)
    if (yd .lt. lprec) then
      fs = 0
    else if (tm .gt. prec) then
      fs = yd/tm*sc
    else
!         see format below
      fs = 100
    end if
!       check format
    if (fs .lt. 10) then
      write(afs,25) fs
    else if (fs .ge. 10 .and. fs .lt. 100) then
      write(afs,35) fs
    else
!         go over
      write(afs,5) cst(2)
    end if
!       log range
    if (abs(tau(j,ic)) .gt. lprec) then
      if (i .gt. mts) then
        errmsg = fomsgf(99, nmm,4,cun(2),0)
        return
      end if
!         section ok
      ltq(i) = log(abs(tau(j,ic)))
!         maximum and minimum
      if (ltq(i) .gt. mxv(1)) mxv(1) = ltq(i)
      if (ltq(i) .lt. mnv) mnv = ltq(i)
    else
      ltq(i) = 0._lrk
    end if
!
!       torque
    tq = abs(trq(j,ic))
!       stress MPa
    tm = abs(tau(j,ic)/sc)
!       too small values
    tm = riff(tm .gt. prec,tm,0._lrk)
!
!       max stress
    if (tm .gt. mxv(2)) then
      mxv(2) = tm
!         max index
      imx = i
    end if
!
!       torque direction
    tdr = tm .gt. prec .and. tau(j,ic) .lt. 0
    sig = ciff(1, tdr,cds(1),cds(2))
    if (idcoupf(j)) then
!         coupling division
      write(iu,75,err = 200) y(j),cds(1),y(i),tq,ad,sig
    else
!         standard division
      write(iu,50,err = 200) y(j),cds(1),y(i),tq,ad,yd,tm,afs,sig
    end if
  end do
!
  write(iu,45,err = 200)
!     y = 97
  txt = txtmsgf(50, 97)
!     max. stress     (Mpa) 135
  tx1 = txtmsgf(50, 135)
  i = len_trim(txt)
  j = len_trim(tx1)
  write(iu,55,err=200) tx1(1:j),txt(1:i),cun(1)
  if (imx .gt. 0) write(iu,65,err=200) mxv(2),y(imx)
!
  if (nsf .gt. 0) then
    write(iu,45,err=200)
!       equivalent stiffness
    call fltout(iu,txtmsgf(50, 64))
!       bearing
    txt = txtmsgf(50, 57)
!       torque
    tx1 = txtmsgf(50, 65)
!       stiffness
    tx2 = txtmsgf(50, 142)
    i = len_trim(txt)
    j = len_trim(tx1)
    k = len_trim(tx2)
    write(iu,85,err=200) txt(1:i),tx1(1:j),tx2(1:k)
!       number label, position y(m), stiffness unit
    txt = txtmsgf(50, 42)
    i = len_trim(txt)
    write(iu,95,err=200)&
    &cds(4),cds(5),txt(1:i),cds(4),cds(5),txt(1:i),cus
!       values, bearing number-pos.,torque number-pos., stiffness
    do i = 1,nsf
      write(iu,105,err=200)&
      &eqstid(i,1),cds(5),tsf(i,2),&
      &eqstid(i,2),cds(5),tsf(i,3),tsf(i,1)
    end do
  end if
!
!     not std i/o
  if (.not. std) then
    close(iu)
  else
    call marksec(sct,1)
  end if
!
!     plot output
  if (plt) then
    if (.not. std) then
!         output file, 0=>-99
      call tmntfnm(-99,m,ofn)
    end if
!
!       calculate nrdm stress ranges
    call tstrange(ltq,nrdm,nt,mts,nrdm,.true.,.true.,&
    &mnv,mxv(1),rnvl,hval,rgvl)
!
!       prepare divisions geometry for plot
    call gmexp(0,cds(3),cds(3),std,.true.,.false.,.false.,errmsg,ok)
    if (ok .lt. 0) return
!       plot
    call tsgeom(std,rnvl,hval,rgvl,mts,nrdm)
  end if
!
  ok = 0
  return
!
100 errmsg = fomsgf(99, nmm,1,ofn,1)
  return
!
200 close(iu)
  errmsg =  fomsgf(99, nmm,13,ofn,1)
  return
!
!     formats
5 format(a)
15 format(2a)
25 format(f5.2)
35 format(f5.1)
45 format()
55 format(1x,a,1x,a,1x,a)
65 format(5x,en10.2,1x,f10.4)
!     coupling
75 format(f7.4,a,f7.4,1x,en10.2,1x,&
  &e10.2,1x,'     -    ',1x,'     -    ',&
  &1x,'   -  ',1x,a)
85 format(1x,a,t20,a,t35,2a)
95 format(2x,2(2(a,1x),a,6x),a)
105 format(2(i3,1x,a,f7.4,5x),EN18.10)
!     standard
50 format(f7.4,a,f7.4,1x,en10.2,1x,&
  &e10.2,1x,en10.2,1x,en10.2,1x,a,1x,a)
220 format()
!
end subroutine ts_static
!
!     ==================================================================
!>    @brief torsion frequency response response output.
!
!>    @param[in] nm number of considered modes
!>    @param[in] np number of output points
!>    @param[in] nrp number of speeds
!>    @param[in] py y output vector
!>    @param[in] rpg speeds vector (rpm)
!>    @param[in] amp complex angular amplitude vector (ead)
!>    @param[in] rdr angular displacement on output division with
!>     speed ratio (rad)
!>    @param[in] tqr torque on output division with speed ratio (N.m)
!>    @param[in] str shear stress on output division
!>    @param[in] mxp position vector dimension
!>    @param[in] mtr speed vector dimension
!>    @param[in] anrd plot output angular displacement in radians
!>    @param[in] lgp plot output logarithmic angular displacement
!>    @param[in] std standard input output
!>    @param[in] plt HPGL output
!>    @param[out] errmsg return error message
!>    @param[out] ok return flag. unsuccessful if <0.
!
subroutine ts_resp_f(nm,np,nrp,py,rpg,amp,rdr,tqr,str,&
&mxp,mtr,anrd,lgp,std,plt,errmsg,ok)
  use rd_textfun, only: fomsgf, txtmsgf
  use com_sta, only: stm
  use rd_kinds, only: lrk, wp
  implicit none
!
!     locals
  integer :: i, j, m, iu
  real(lrk) :: arg, riff, rpif, rpm2radf, rargf
  character(len=4) :: aun
  character(len=5) :: c5, cst
  character(len=6) :: c6
  character(len=8) :: c8
  character(len=9) :: c9, nmm
  character(len=10) :: sec
  character(len=12) :: c12
  character(len=19) :: c19
  character(len=35) :: ft
  character(len=50) :: txt
  character(len=255) :: ofn
  dimension c5(2)
  parameter(cst = 'stdio',aun = 'rad.',c5 = (/'(20x,','(a20,'/),&
  &c6 = 'E18.10',c8 = '(a19,1x,',sec = 'tfrqrsp',c9 = '(E18.10))',&
  &c12 = '(a14,i2,2x))',nmm = 'ts_resp_f',&
  &c19 = '('' y'',a10,i2,4x))')
!
!     arguments
  integer :: nm, np, nrp, mxp, mtr, ok
  real(lrk) :: py, rpg, rdr, tqr, str
  complex(wp) :: amp
  dimension py(mxp),rpg(mtr),amp(mxp,mtr),&
  &rdr(mxp,mtr),tqr(mxp,mtr),str(mxp,mtr)
  character(len=99) :: errmsg
  logical :: anrd, lgp, std, plt
!
!     dimension local
  real(lrk) :: ang(mxp)
!
!     stamp
!
  intrinsic :: abs, aimag, int, len_trim, real
!
  ok = -1
!
  if (.not. std) then
    iu = 12
    call tmntfnm(2,m,ofn)
!       open output file
    open(iu,file=ofn(1:m),err = 100)
  else
    iu = 6
    m = 5
    ofn = cst
    call marksec(sec,0)
  end if
!
  write(iu,5,err=200) stm
  call outdsc(iu)
  txt = txtmsgf(50, 105)
  i = len_trim(txt)
  write(iu,15,err=200) txt(1:i),nm
  write(iu,220,err=200)
!
!     output positions
!     prepare format
  write(ft,300) c5(1),np,c12
  txt = txtmsgf(50, 18)
  write(iu,ft,err=200) (txt(1:18),i,i = 1,np)
  write(ft,300) c8,np,c9
  txt = txtmsgf(50, 42)
  write(iu,ft,err=200) txt(1:20),(py(i),i = 1,np)
  write(iu,220)
!
!     amplitude
  call fltout(iu,txtmsgf(50, 106))
  call fltout(iu,txtmsgf(50, 95))
!     prep format
  write(ft,300) c5(2),np,c19
  write(iu,ft,err=200)txtmsgf(50, 108),(txtmsgf(50, 18),i,i = 1,np)
  write(ft,400) np+1,c6
  do i = 1,nrp
    write(iu,ft,err=200)rpm2radf(rpg(i)),(abs(amp(j,i)),j = 1,np)
  end do
  write(iu,220)
!
!     phase
  call fltout(iu,txtmsgf(50, 107))
  call fltout(iu,txtmsgf(50, 95))
  write(ft,300) c5(2),np,c19
  write(iu,ft,err=200) txtmsgf(50, 108),(txtmsgf(50, 18),i,i = 1,np)
  write(ft,400) np+1,c6
!
  do i = 1,nrp
    do j = 1,np
!         angle
      arg = rargf(amp(j,i))
!         if negative add + 2*pi
      ang(j) = riff(arg .lt. 0,arg+2*rpif(),arg)
    end do
    write(iu,ft,err=200) rpm2radf(rpg(i)),(ang(j),j = 1,np)
  end do
  write(iu,220)
!
!     angular displacement on division
  call fltout(iu,txtmsgf(50, 130))
  call fltout(iu,txtmsgf(50, 95))
  write(ft,300) c5(2),np,c19
  write(iu,ft,err=200) txtmsgf(50, 108),(txtmsgf(50, 18),i,i = 1,np)
  write(ft,400) np+1,c6
  do i = 1,nrp
    write(iu,ft,err=200) rpm2radf(rpg(i)),(rdr(j,i),j = 1,np)
  end do
  write(iu,220,err=200)
!
!     torque on division
  call fltout(iu,txtmsgf(50, 131))
  call fltout(iu,txtmsgf(50, 95))
  write(ft,300) c5(2),np,c19
  write(iu,ft,err=200) txtmsgf(50, 108),(txtmsgf(50, 18),i,i = 1,np)
  write(ft,400) np+1,c6
  do i = 1,nrp
    write(iu,ft,err=200)rpm2radf(rpg(i)),(tqr(j,i),j = 1,np)
  end do
  write(iu,220,err=200)
!
!     stress on division
  call fltout(iu,txtmsgf(50, 136))
  call fltout(iu,txtmsgf(50, 95))
  write(ft,300) c5(2),np,c19
  write(iu,ft,err=200) txtmsgf(50, 108),(txtmsgf(50, 18),i,i = 1,np)
  write(ft,400) np+1,c6
  do i = 1,nrp
    write(iu,ft,err=200) rpm2radf(rpg(i)),(str(j,i),j = 1,np)
  end do
!
!     not std i/o
  if (.not. std) then
!       close output file
    close(iu)
  else
    call marksec(sec,1)
  end if
!
!     plot output
  if (plt) then
    if (.not. std) then
!         output file
      call tmntfnm(-2,m,ofn)
    end if
!
    call tfrqr(rpg,amp,rdr,tqr,str,np,nrp,mxp,mtr,&
    &anrd,lgp,std,ofn)
  end if
!
!     return ok
  ok = 0
  return
!
100 errmsg = fomsgf(99, nmm,1,ofn,1)
  return
!
200 close(iu)
  errmsg = fomsgf(99, nmm,13,ofn,1)
  return
!
5 format(a)
15 format(a,i3)
220 format()
300 format(a,i2,a)
400 format('(',i2,a,')')
!
end subroutine ts_resp_f
!
!     ==================================================================
!>    @brief torsion Campbell output.
!
!>    @param[in] teln speed time vector, 1x, 2x, ...
!      negative value represents 2 x grid frequency (slip)
!>    @param[in] ig intersections vector natural frequency index
!>    @param[in] ic intersections vector index
!>    @param[in] px intersections vector 1x, 2x, 0.5x
!>    @param[in] py critical frequencies
!>    @param[in] git critical frequencies by speed matrix (rad/s)
!>    @param[in] nini initial speed (rpm)
!>    @param[in] nfin initial speed (rpm)
!>    @param[in] ndw number of spped from nini to nfin
!>    @param[in] tcpn Campbell nominal speed (rpm)
!>    @param[in] tsmg nominal speed separation margin (pu)
!>    @param[in] ncn number of intersctions vector
!>    @param[in] nteln number of speed time vector
!>     elements on teln vector
!>    @param[in] nnf number of critical speed
!>    @param[in] mtg git vector dimension
!>    @param[in] mxteln dimension of speed time vector
!>    @param[in] mxpt dimension of intersection vectors
!>    @param[in] std standard io flag
!>    @param[in] plt HPGL output
!>    @param[out] errmsg return an error message
!>    @param[out] ok return flag. unsuccessful if <0.
!
subroutine ts_cmpbl(teln,ig,ic,px,py,git,nini,nfin,ndw,&
&tcpn,tsmg,&
&ncn,nteln,nnf,mtg,mxteln,mxpt,std,plt,errmsg,ok)
  use rd_textfun, only: fomsgf, txtmsgf
  use com_sta, only: stm
  use rd_kinds, only: lrk
  implicit none
!
!     locals
  integer :: i, k, l, m, iu

  real(lrk) :: xv, dlr, ff
  real(lrk) :: rad2hzf
!
  character(len=1) :: clb
  character(len=5) :: cfn, cf3
  character(len=8) :: nmm
  character(len=9) :: cf1
  character(len=10) :: sec
  character(len=17) :: cf2
  character(len=40) :: ft
  character(len=50) :: buf
  character(len=255) :: ofn
  dimension cf1(2),cf2(2)
  parameter (clb = '(',cfn = 'stdio',&
  &cf1 = (/'(EN18.8))','(i2,16x))'/),&
  &cf2 = (/'(f6.1,''x'',12x))','(f6.1,''x'',12x),'/),&
  &cf3 = '(a18,',nmm = 'ts_cmpbl',&
  &sec = 'tcampbell')
!
!     arguments
  integer :: ig, ic, ncn, nteln, nnf, mtg, mxteln, mxpt, ok
  dimension ff(mxpt)
  real(lrk) :: teln, git, px, py, nini, nfin, ndw, tcpn, tsmg
  dimension teln(mxteln),&
  &ig(mxpt),ic(mxpt),px(mxpt),py(mxpt),git(mtg)
  character(len=99) :: errmsg
  logical :: std, plt
!
!     stamp
!
!     locals
!
  intrinsic :: abs
!
!     return init
  ok = -1
!     not std i/o
  if (.not. std) then
!       output file
    iu = 12
    call tmntfnm(1,m,ofn)
    open(iu,file=ofn(1:m),err = 100)
  else
    iu = 6
    ofn = cfn
    m = len_trim(ofn)
    call marksec(sec,0)
  end if
!
  write(iu,15,err=200) stm
  call outdsc(iu)
  call fltout(iu,txtmsgf(50, 109))
  write(iu,5,err=200)
!
!     critical speeds
  call fltout(iu,txtmsgf(50, 2))
!
!     prep format
  write(ft,300,err=200 )clb,nteln,cf2(1)
  write(iu,ft,err=200) (teln(k),k=1,nteln)
!
!     prep format
  write(ft,300,err=200) clb,nteln,cf1(1)
!     number of natural frequencies
  do i = 1,nnf
!       number of intersections
    do k = 1,nteln
!         number of crossings
      ff(k) = 0
      do l = 1,ncn
!           natural and cross indices
        if (ig(l) .eq. i .and. ic(l) .eq. k .and.&
        &px(l) .gt. 0) then
          ff(k) = px(l)
          exit
        end if
      end do
    end do
    write(iu,ft,err=200) (ff(l),l = 1,nteln)
  end do
  write(iu,5,err=200)
!
!     critical frequecies
  call fltout(iu,txtmsgf(50, 3))
!     prep format
  write(ft,300,err=200) clb,nteln,cf2(1)
  write(iu,ft,err=200) (teln(k),k=1,nteln)
!
!     prep format
  write(ft,300,err=200) clb,nteln,cf1(1)
!     prep format
  write(ft,300,err=200) clb,nteln,cf1(1)
!     number of natural frequencies
  do i = 1,nnf
!       number of intersections
    do k = 1,nteln
!         number of crossings
      ff(k) = 0
      do l = 1,ncn
!           natural and cross indices
        if (ig(l) .eq. i .and. ic(l) .eq. k .and.&
        &py(l) .gt. 0) then
          ff(k) = py(l)
          exit
        end if
      end do
    end do
    write(iu,ft,err=200) (ff(l),l = 1,nteln)
  end do
  write(iu,5,err=200)
!
!     diagram points
  call fltout(iu,txtmsgf(50, 4))
  call fltout(iu,txtmsgf(50, 95))
!     prep format
  write(ft,25,err=200) cf3,nteln,cf2(2),nnf,cf1(2)
!
  buf = txtmsgf(50, 23)
  write(iu,ft,err=200)&
  &buf(1:18),(teln(k),k=1,nteln),(k,k=1,nnf)
!
!     prep format
  k = nteln+nnf+1
  write(ft,300,err=200) clb,k,cf1(1)
!
!     graph data
  dlr = (nfin-nini)/ndw
!     RD-019: index-driven sweep. The point count is fixed by the deck and
!     therefore independent of REAL32/REAL64 accumulated-roundoff.
  do i = 0,nint(ndw)
    xv = nini+real(i,lrk)*dlr
    do k = 1,nteln
!         number of crossings
      if (teln(k) .gt. 0) then
        ff(k) = xv*teln(k)
      else if (tcpn .gt. 0) then
        ff(k) = xv*teln(k)/tcpn+abs(teln(k))
        if (ff(k) .lt. 0) ff(k) = 0
      end if
    end do
    write(iu,ft,err=200)&
    &xv,(ff(k),k=1,nteln),(rad2hzf(git(k)),k=1,nnf)
  end do
!
  if (.not. std) then
!       close output
    close(iu)
  else
    call marksec(sec,1)
  end if
!
!     plot
  if (plt) then
    if (.not. std) then
!         output file
      call tmntfnm(-1,m,ofn)
    end if
!
    call tcpblp(teln,px,py,git,nini,nfin,&
    &tcpn,tsmg,&
    &ncn,nteln,nnf,mtg,mxteln,mxpt,std,ofn)
  end if
!
!     return ok
  ok = 0
  return
!
100 errmsg = fomsgf(99, nmm,1,ofn,1)
  return
!
!     close even std i/o
200 close(iu)
  errmsg = fomsgf(99, nmm,13,ofn,1)
  return
!
5 format()
15 format(a)
25 format(a,i2,a,i3,a)
300 format('(',a,i3,a,')')
!
end subroutine ts_cmpbl
!
!     ==================================================================
!>    @brief torsion periodic forced response output.
!
!>    @param[in] im torsion model index
!>    @param[in] np number of response positions
!>    @param[in] ntp number of time steps
!>    @param[in] pse unique input positions vector (m)
!>    @param[in] pui unique input position indices matrix
!>    @param[in] pnui number of unique input positions
!>    @param[in] wnui number of unique input frequencies
!>    @param[in] wui unique input frequencies indices matrix
!>    @param[in] wra unique freuquencies vector (rad/s)
!>    @param[in] pr response vector positions (m)
!>    @param[in] idhr position index main input
!>    @param[in] crat coupling speed ration each division vector
!>    @param[in] hrs unique frequency complex output angle (rad)
!>    @param[in] htv time vector (s)
!>    @param[in] hmv output maximum time angle per response position (ra
!>    @param[in] hit input time torque per unique excitation position (N
!>    @param[in] hav output time angle each response position (rad)
!>    @param[in] htq output time torque each response position (rad)
!>    @param[in] hst output stress each response position (Pa)
!>    @param[in] hta output time angle each response position division (
!>    @param[in] hmt output maximum time torque per response position (r
!>    @param[in] hma output maximum total angle per response position (r
!>    @param[in] mth time output input row dimension
!>    @param[in] mtt division speed ratio vector dimension
!>    @param[in] mtg output time angle matrix row dimension
!>    @param[in] mxb harmonic dimension
!>    @param[in] mxr respose position vector dimension
!>    @param[in] anrd plot output angular displacement in radians
!>    @param[in] std standard io flag
!>    @param[in] plt HPGL output
!>    @param[out] errmsg return an error message
!>    @param[out] ok return flag. unsuccessful if <0.
!
subroutine ts_pefor(im,np,ntp,pse,pui,pnui,wnui,wui,wra,&
&pr,idhr,crat,&
&hrs,htv,hmv,hit,hav,htq,hst,hta,hmt,hma,&
&mth,mtt,mtg,mxb,mxr,anrd,std,plt,errmsg,ok)
  use rd_textfun, only: fomsgf, txtmsgf
  use com_sta, only: stm
  use rd_kinds, only: lrk, wp
  implicit none
!
!     arguments
  integer :: im, np, ntp, pui, pnui, wnui, wui, idhr, mth, mtt, mtg, mxb, mxr, ok
  real(lrk) :: pse, wra, pr, crat, htv, hmv, hit, hav, htq, hst, hta, hmt, hma
  complex(wp) :: hrs
  dimension pse(mxb),pui(mxb,mxb),wui(mxb,mxb),&
  &wra(mxb),pr(mxr),crat(mtt),&
  &idhr(mxb),&
  &hrs(mtg,mxb),htv(mth),hmv(mxr),hit(mth,mxb),&
  &hav(mth,mxr),htq(mth,mxr),hst(mth,mxr),hta(mth,mxr),&
  &hmt(mxr),hma(mxr)
  character(len=99) :: errmsg
  logical :: anrd, std, plt
!
  real(lrk) :: srt, am, an, ww, rargf
  complex(wp) :: ca
  integer :: i, iu, j, k, m, m1
  character(len=1) :: ddot, c1
  character(len=3) :: c3
  character(len=5) :: cst
  character(len=8) :: c8, nmm, cip, crp
  character(len=10) :: sec
  character(len=12) :: c12
  character(len=40) :: ft
  character(len=50) :: buf
  character(len=255) :: ofn
  dimension cip(mxb),crp(mxb),c12(2)
  parameter (ddot = ':',c1 = '<',c3 = '(a,',&
  &cst = 'stdio',c8 = '(EN14.6,',nmm = 'ts_pefor',&
  &sec = 'tperfors',c12 = (/'(1x,a12,4x))','(1x,EN16.6))'/))
!
!     stamp
!
  intrinsic :: abs, aimag, len_trim, real
!
!     return init
  ok = -1
!
!     not std i/o
  if (.not. std) then
!       output file
    iu = 12
    call tmntfnm(20,m,ofn)
    open(iu,file=ofn(1:m),err=100)
  else
    ofn = cst
    m = 5
    iu = 6
    call marksec(sec,0)
  end if
!
  write(iu,5,err=200) stm
  call outdsc(iu)
  call fltout(iu,txtmsgf(50, 112))
  write(iu,25,err=200)
!
  do i = 1,np
!       y position
    buf = txtmsgf(50, 42)
    j = len_trim(buf)
    write(iu,15,err=200) buf(1:j),pr(i)
!       max tot. ang. displ.
    buf = txtmsgf(50, 137)
    j = len_trim(buf)
    write(iu,35,err=200) buf(1:j),hma(i)
!       max div. ang. displ.
    buf = txtmsgf(50, 113)
    j = len_trim(buf)
    write(iu,35,err=200) buf(1:j),hmv(i)
!       max torque
    buf = txtmsgf(50, 127)
    j = len_trim(buf)
    write(iu,35,err=200) buf(1:j),hmt(i)
    buf = txtmsgf(50, 114)
    j = len_trim(buf)
!       harm contents title
    write(iu,5,err=200) buf(1:j)
    buf = txtmsgf(50, 115)
    j = len_trim(buf)
!       content columns titles
    write(iu,5,err=200) buf(1:j)
!       index
    j = idhr(i)
!       torsional model 1 = 2 nodes, 2 = three nodes
    if (im .eq. 1) then
!         model index
      m1 = j
    else if (im .eq. 2) then
      m1 = (j-1)*2+1
    end if
!        speed ratio
    if (j .gt. 1) then
      srt = crat(j-1)
    else
      srt = 1
    end if
    do j = 1,wnui
!         each unique frequency response complex angle
      ca = hrs(m1,j)
!         cos amplitude
      am = srt*real(abs(ca), lrk)
!         cos angle
      an = rargf(ca)
!         frequency index
      k = wui(j,1)
!         cos frequency
      ww = wra(k)
      write(iu,65,err=200) j,ddot,ww,ddot,am,c1,an
!         unique frequencies
    end do
!       position loop
  end do
!
!     time input per unique position time -> position value
  write(iu,25,err=200)
!     input torque
  call fltout(iu,txtmsgf(50, 116))
  call fltout(iu,txtmsgf(50, 95))
!     y input torque position (m)
  do i = 1,pnui
    j = pui(i,1)
    write(cip(i),55) pse(j)
  end do
!     time(s) pos(m)->
  buf = txtmsgf(50, 117)
  j = len_trim(buf)
  write(ft,45) c3,pnui,c12(1)
  write(iu,ft,err=200) buf(1:j),(cip(i),i=1,pnui)
!
  write(ft,45) c8,pnui,c12(2)
  do i = 1,ntp
    write(iu,ft,err=200) htv(i),(hit(i,j),j=1,pnui)
!       end time loop
  end do
!
!     time angle output time -> position value
  write(iu,25,err=200)
!     total angular displacement at division
  call fltout(iu,txtmsgf(50, 130))
  call fltout(iu,txtmsgf(50, 95))
!     y response position (m)
  do i = 1,np
    write(crp(i),55)pr(i)
  end do
!     time(s) pos(m)->
  buf = txtmsgf(50, 117)
  j = len_trim(buf)
  write(ft,45) c3,pnui,c12(1)
  write(iu,ft,err=200) buf(1:j),(crp(i),i=1,np)
!
  write(ft,45) c8,np,c12(2)
  do i = 1,ntp
    write(iu,ft,err=200) htv(i),(hav(i,j),j=1,np)
  end do
!
!     time division angle output time -> position value
  write(iu,25,err=200)
!     total angular displacement at division
  call fltout(iu,txtmsgf(50, 134))
  call fltout(iu,txtmsgf(50, 95))
!     y response position (m)
  do i = 1,np
    write(crp(i),55) pr(i)
  end do
!     time(s) pos(m)->
  buf = txtmsgf(50, 117)
  j = len_trim(buf)
  write(ft,45) c3,pnui,c12(1)
  write(iu,ft,err=200) buf(1:j),(crp(i),i=1,np)
!
  write(ft,45) c8,np,c12(2)
  do i = 1,ntp
    write(iu,ft,err=200) htv(i),(hta(i,j),j=1,np)
  end do
!
!     time division torque output time -> position value
  write(iu,25,err=200)
!     torque at division
  call fltout(iu,txtmsgf(50, 131))
  call fltout(iu,txtmsgf(50, 95))
!     time(s) pos(m)->
  buf = txtmsgf(50, 117)
  j = len_trim(buf)
  write(ft,45) c3,pnui,c12(1)
  write(iu,ft,err=200) buf(1:j),(crp(i),i=1,np)
!
  write(ft,45) c8,np,c12(2)
  do i = 1,ntp
    write(iu,ft,err=200) htv(i),(htq(i,j),j=1,np)
  end do
!
!     time division shear stress output time -> position value
  write(iu,25,err=200)
!     shear stres at division
  call fltout(iu,txtmsgf(50, 136))
  call fltout(iu,txtmsgf(50, 95))
!     time(s) pos(m)->
  buf = txtmsgf(50, 117)
  j = len_trim(buf)
  write(ft,45) c3,pnui,c12(1)
  write(iu,ft,err=200) buf(1:j),(crp(i),i=1,np)
!
  write(ft,45) c8,np,c12(2)
  do i = 1,ntp
    write(iu,ft,err=200) htv(i),(hst(i,j),j=1,np)
  end do
!
  write(iu,25,err=200)
!
  if (.not. std) then
!       close output
    close(iu)
  else
    call marksec(sec,1)
  end if
!
!     plot
  if (plt) then
    if (.not. std) then
!         output file
      call tmntfnm(-20,m,ofn)
    end if
!
    call tpeforp(ntp,pnui,np,cip,crp,mth,mxb,mxr,&
    &htv,hit,hav,htq,hst,hta,anrd,std,ofn)
  end if
!
!     return ok
  ok = 0
  return
!
100 errmsg = fomsgf(99, nmm,1,ofn,1)
  return
!
!     close even std i/o
200 close(iu)
  errmsg = fomsgf(99, nmm,13,ofn,1)
!
  return
!
5 format(a)
15 format(a,f8.4)
25 format()
35 format(a,EN16.6)
45 format(a,i2,a)
55 format(f7.4)
65 format(i2,a,EN16.6,a,EN16.6,a,EN16.6)
!
end subroutine ts_pefor
!
!     ==================================================================
!>    @brief torsion transient numerical integration output
!
!>    @param[in] iit initial integration time
!>    @param[in] fit final integration time
!>    @param[in] its integration step
!>    @param[in] vts view step
!>    @param[in] epi excitation position index
!>    @param[in] nte number of load indices
!>    @param[in] ndd excitations position vector
!>    @param[in] tpf kind of exc
!>    @param[in] no number of output positions
!>    @param[in] imt numerical integration method
!>     0 = Newmark 1 = Wilson-theta
!>    @param[in] py output position vecor
!>    @param[in] ttrdsz size of transient time vector
!>    @param[in] ttrmd transient damping
!>    @param[in] ttrdtt transient load torque time vector
!>    @param[in] ttrdta transient load torque amplitude vector
!>    @param[in] nrstps number of result time vector
!>    @param[in] restm transient result time matrix
!>    @param[in] resum transient result angular displacement matrix
!>    @param[in] iptld result input loads
!>    @param[in] restq division transient result torque matrix
!>    @param[in] resst division transient result shear stress matrix
!>    @param[in] resad division transient result angular displacement ma
!>    @param[in] tqm input torque multiplier vector
!>    @param[in] mxttra input time torque matrix columns (pos.) dimensio
!>    @param[in] mxb excitation positions vector dimension
!>    @param[in] mxttrd input time torque matrix rows (time) dimension]
!>    @param[in] mxttrs output time torque matrix rows (time) dimension
!>    @param[in] mxr response position vector dimension
!>    @param[in] iseqt torque time equation flag
!>    @param[in] anrd plot output angular displacement in radians
!>    @param[in] std standard io flag
!>    @param[in] plt HPGL output
!>    @param[out] errmsg return an error message
!>    @param[out] ok return flag. unsuccessful if <0.
!
subroutine ts_nuint(iit,fit,its,vts,epi,nte,ndd,tpf,no,imt,py,&
&ttrdsz,ttrmd,ttrdtt,ttrdta,nrstps,restm,resum,iptld,&
&restq,resst,resad,tqm,&
&mxttra,mxb,mxttrd,mxttrs,mxr,&
&iseqt,anrd,std,plt,errmsg,ok)
  use rd_textfun, only: fomsgf, txtmsgf
  use com_sta, only: stm
  use rd_kinds, only: lrk
  implicit none
!
  integer :: epi, nte, tpf, no, imt, ttrdsz, nrstps, mxttra, mxb, mxttrd, mxttrs, mxr, ok
  real(lrk) :: iit, fit, its, vts, ndd, py, ttrmd, ttrdtt, ttrdta, restm, resum, iptld, restq, resst, resad, tqm
  dimension epi(mxttra),ndd(mxb),tpf(mxb),py(mxr),&
  &ttrmd(mxttra),ttrdtt(mxttrd),ttrdta(mxttra,mxttrd),&
  &restm(mxttrs),resum(mxttrs,mxr),iptld(mxttrs,mxr),&
  &restq(mxttrs,mxr),resst(mxttrs,mxr),resad(mxttrs,mxr),&
  &tqm(mxttra)
  character(len=99) :: errmsg
  logical :: iseqt, anrd, std, plt
!
  integer :: ii, jj, j, tid, iu, m
  real(lrk) :: pos
  character(len=8) :: nmm
  character(len=50) :: buf
  character(len=1) :: cf2
!     cid -> transient torque position index
  character(len=3) :: cid, cf1
  character(len=5) :: cfn
  character(len=9) :: cf3
  character(len=10) :: sec
  character(len=11) :: cf5
  character(len=12) :: cit
  character(len=13) :: cf4
  dimension cit(2)
  parameter (&
  &cf1 = '(a,',cf2 = '(',cf3 = '(en16.6))',&
  &cf4 = '(5x,f8.4,3x))',cf5= '(2x,a,11x))',&
  &cfn = 'stdio',nmm = 'ts_nuint',sec = 'ttimtran',&
  &cit = (/'Newmark     ','Wilson-theta'/))
  character(len=40) :: ft
  character(len=255) :: ofn
  dimension cid(mxb),tid(mxb)
!
!     stamp
!
  intrinsic :: abs, len_trim
!
!     return init
  ok = -1
!
!     not std i/o
  if (.not. std) then
!       output file
    iu = 12
    call tmntfnm(21,m,ofn)
    open(iu,file=ofn(1:m),err = 100)
  else
    ofn = cfn
    m = 5
    iu = 6
    call marksec(sec,0)
  end if
!
  write(iu,15,err = 200)stm
  call outdsc(iu)
  call fltout(iu,txtmsgf(50, 120))
  write(iu,5,err=200)
!
!     integration kind
  buf = txtmsgf(50, 143)
  j = len_trim(buf)
  if (imt .eq. 0) then
    jj = len_trim(cit(1))
    write(iu,55,err=200)buf(1:j),cit(1)(1:jj)
  else
    jj = len_trim(cit(2))
    write(iu,55,err=200)buf(1:j),cit(2)(1:jj)
  end if
!
!     initial integration time
  buf = txtmsgf(50, 121)
  j = len_trim(buf)
  write(iu,300,err=200)buf(1:j),iit
!     inal integration time
  buf = txtmsgf(50, 122)
  j = len_trim(buf)
  write(iu,300,err=200)buf(1:j),fit
!     integration step
  buf = txtmsgf(50, 123)
  j = len_trim(buf)
  write(iu,300,err=200)buf(1:j),its
!     view step
  buf = txtmsgf(50, 124)
  j = len_trim(buf)
  write(iu,300,err=200)buf(1:j),vts
!
!     transient position and damping
  buf = txtmsgf(50, 125)
  j = len_trim(buf)
  do ii = 1,nte
!       position index
    jj = epi(ii)
!       transient excitation position
    pos = ndd(jj)
!       torque load index
    tid(ii) = abs(tpf(jj))
!       position index
    write(cid(ii),25)tid(ii)
!       position and damping
    write(iu,35,err=200)&
    &buf(1:j),tid(ii),pos,ttrmd(tid(ii))
  end do
!
!     transient excitations - input data
  write(iu,5,err=200)
  call fltout(iu,txtmsgf(50, 116))
  call fltout(iu,txtmsgf(50, 95))
!     input transient torque at input positions
  buf = txtmsgf(50, 119)
  j = len_trim(buf)
!     prepare format
  write(ft,45)cf1,nte,cf5
  write(iu,ft,err=200)buf(1:j),(cid(jj),jj=1,nte)
!     prepare format
  write(ft,45)cf2,nte+1,cf3
  do ii = 1,ttrdsz
    write(iu,ft,err=200)ttrdtt(ii),(ttrdta(jj,ii)*tqm(jj),jj=1,nte)
  end do
!
!     transient interpolated input torque at input positions and
  write(iu,5,err=200)
  call fltout(iu,txtmsgf(50, 132))
  call fltout(iu,txtmsgf(50, 95))
!     time(s) y(m)->
  buf = txtmsgf(50, 117)
  j = len_trim(buf)
!     prepare format
  write(ft,45)cf1,nte,cf4
  write(iu,ft,err=200)buf(1:j),(ndd(epi(jj)),jj=1,nte)
!     prepare format
  write(ft,45)cf2,nte+1,cf3
  do ii = 1,nrstps
    write(iu,ft,err=200)restm(ii),(iptld(ii,jj),jj=1,nte)
  end do
!
!     output total angular displacement at output positions
  write(iu,5,err=200)
  call fltout(iu,txtmsgf(50, 126))
  call fltout(iu,txtmsgf(50, 95))
!     time(s) y(m)->
  buf = txtmsgf(50, 117)
  j = len_trim(buf)
!     prepare format
  write(ft,45)cf1,no,cf4
  write(iu,ft,err=200)buf(1:j),(py(jj),jj=1,no)
!     prepare format
  write(ft,45)cf2,no+1,cf3
  do ii = 1,nrstps
    write(iu,ft,err=200)restm(ii),(resum(ii,jj),jj=1,no)
  end do

!     output division angular displacement at output positions
  write(iu,5,err=200)
  call fltout(iu,txtmsgf(50, 133))
  call fltout(iu,txtmsgf(50, 95))
!     time(s) y(m)->
  buf = txtmsgf(50, 117)
  j = len_trim(buf)
!     prepare format
  write(ft,45)cf1,no,cf4
  write(iu,ft,err=200)buf(1:j),(py(jj),jj=1,no)
!     prepare format
  write(ft,45)cf2,no+1,cf3
  do ii = 1,nrstps
    write(iu,ft,err=200)restm(ii),(resad(ii,jj),jj=1,no)
  end do
!
!     output angular torque at output positions
  write(iu,5,err=200)
  call fltout(iu,txtmsgf(50, 128))
  call fltout(iu,txtmsgf(50, 95))
!     time(s) y(m)->
  buf = txtmsgf(50, 117)
  j = len_trim(buf)
!     prepare format
  write(ft,45)cf1,no,cf4
  write(iu,ft,err=200)buf(1:j),(py(jj),jj=1,no)
!     prepare format
  write(ft,45)cf2,no+1,cf3
  do ii = 1,nrstps
    write(iu,ft,err=200)restm(ii),(restq(ii,jj),jj=1,no)
  end do
!
!     output angular shear stress at output positions
  write(iu,5,err=200)
  call fltout(iu,txtmsgf(50, 136))
  call fltout(iu,txtmsgf(50, 95))
!     time(s) y(m)->
  buf = txtmsgf(50, 117)
  j = len_trim(buf)
!     prepare format
  write(ft,45)cf1,no,cf4
  write(iu,ft,err=200)buf(1:j),(py(jj),jj=1,no)
!     prepare format
  write(ft,45)cf2,no+1,cf3
  do ii = 1,nrstps
    write(iu,ft,err=200)restm(ii),(resst(ii,jj),jj=1,no)
  end do
!
  write(iu,5,err=200)
!
  if (.not. std) then
!       close output
    close(iu)
  else
    call marksec(sec,1)
  end if
!
!     plot
  if (plt) then
    if (.not. std) then
!         output file
      call tmntfnm(-21,m,ofn)
    end if
!
    call tnuintp(nte,epi,ndd,no,imt,py,&
    &ttrdsz,ttrdtt,ttrdta,iit,fit,&
    &nrstps,restm,resum,iptld,restq,resst,resad,tqm,&
    &mxb,mxttra,mxr,mxttrd,mxttrs,iseqt,anrd,std,ofn)
  end if
!
!     return ok
  ok = 0
  return
!
100 errmsg = fomsgf(99, nmm,1,ofn,1)
  return
!
!     close even std i/o
200 close(iu)
  errmsg = fomsgf(99, nmm,13,ofn,1)
  return
!
5 format()
15 format(a)
25 format(i3)
35 format(a,i3,' -> ',f8.4,' x ',EN14.6)
45 format(a,i3,a)
55 format(a,1x,a)
300 format(a,EN14.6)
  !
end subroutine ts_nuint
!
!     ==================================================================
!>    @brief print torsion options static equivalent stiffness
!>     restriction-torque definitions input.
!>     pairs of restriction-excitation integer numbers, coma
!>     separated values. Ex:1-2,2-10. Meaning:<br>
!>     1-2 from excitation number 2 to bearing number 1<br>
!>     2-10 from excitation number 10 to bearing number 2.<br>
!>     Numbers must exist on main input.
!
!
!     ==================================================================
!>    @brief torsion options file output.
!
!>    @param[in] eu output file unit number
!>    @param[in] nomopf options file name
!>    @param[in] kd return kind of options file
!>    @param[in] fto optional flexural-torsion data is present
!>    @param[out] errmsg return error message
!>    @param[out] ok return flag. unsuccessful if <0.
!
subroutine tparfile(eu,nomopf,kd,fto,errmsg,ok)
  use rd_textfun, only: fomsgf, sjoinif, sjoinrf, tpeqstidf
  use com_copcm, only: ops
  use com_couplings, only: cplgid, cplgst, cplgdp, cplgin, cplgsr, ncplg
  use com_fillets, only: ntfil, tfilid, tfilrd
  use com_knd, only: cknd, supr
  use com_lit, only: nrt, lnt
  use com_lot, only: nro, lno
  use com_mdamps, only: tdmrt, tdmmd, ntdmp
  use com_mdampso, only: mdoff
  use com_tcpbl, only: tcpn, tsmg, nteln
  use com_tcpbl1, only: teln
  use com_tflxtor, only: siv, nsi, nki, sfv, ftv, scy, tvl, kvl, nsc, nks, nnt
  use com_tmdn, only: tmdnb, ntmdn
  use com_touopt, only: telm, tlopt
  use com_tpfrt, only: fprt, fprts
  use com_tshyi, only: tshyipu
  use com_ttraneq, only: tequt
  use com_ttransc, only: ttris, ttrrs, ttrit, ttret
  use com_ttransd, only: ttrmd, ttrdtt, ttrdta, nttra, ttrdsz, iseqt
  use rd_kinds, only: lrk
  implicit none
!
!     arguments
  logical :: fto
  integer :: eu, ok
  character(len=1) :: kd
  character(len=99) :: errmsg
  character(len=255) :: nomopf
!
!     kind of parameters
!
!     torsion Campbell
  integer :: mxteln
  parameter (mxteln = 15)
!     nominal speed (rpm), separation margin (pu),
!     excitation lines pu speed, -grid frequency
!
!     mode numbers to calculate/present
!     overrides NBRMOD
  integer :: mxtmdn
  parameter (mxtmdn = 15)
!     periodic force response simulation time and time step
!
!     torsion couplings
  integer :: mxcplg
  parameter (mxcplg = 20)
!     number of couplings
!     section ids
!     stiffness, damping,inertia,speed ratio
!     torsion element type
!     logical output options lof and radians
!
!     modal dampings
  integer :: mxtdmp
  parameter (mxtdmp = 10)
!     number of modal dampings,mode offset
!     mode ids
!     damping ratio
!     mode offset
!
!     section fillets
  integer :: mxtfil
  parameter (mxtfil = 20)
!     number of section fillets
!     section ids
!     section fillets
!
!     torsion shear yield pu
!     yield strength multiplier
!
!     transient
  integer :: mxttra, mxttrd
  parameter (mxttra = 5,mxttrd = 10000)
!     number of transients, number of data points
!     integration step, result step, integration init,integration end
!     damping,time,torque amplitude
!     torque equation
!
!     mxscls dimension of section values
!     siv vector of section indices to check
!     nsi notch sections index vector
!     nki sections with keyway index vector
!     sfv surface finishing code values
!     scy section ultimate strength vector
!     ftv fatigue values vector
!      1) FTCNP fatigue reliability
!      2) FTNBCY fatigue number of cycles
!      3) FTSTPK fatigue temperature (K)
!      4) FTASM aditional safety margin
!     tvl torque values vector
!      1) ALTQP alternating torque PU
!      2) MXTQP maximum torque PU
!      3) TQSTF torque stress factor = 0 only bend
!      4) BDSTF bend stress factor = 0 only torsion
!     kvl key geometry values vector
!      1) distance from keyway straight length start, up to left section
!      2) keyway straight same height (m)
!      3) keyway straight same height and width length (m)
!      4) keyway milling bottom fillet radius (m)
!      5) keyway straight same width (m)
!      6) sled runner keyway milling radius (m)
!     nsc number of sections to check
!     nks number of keys
!     nnt number of notches
  integer :: mxscls
  parameter(mxscls = 20)
!
!     lnt string vector with the tags lines, each line.
!
!     lno string vector with the tags lines,
!     flexural-torsion optional data, each line.
!
!     calculation options
!
  real(lrk) :: dm
  integer :: ii, jj, kk, ll
  character(len=1) :: blank
  character(len=7) :: cfm
  character(len=8) :: nmm
  character(len=19) :: c19
  character(len=30) :: buf
  character(len=255) :: ofn, fmt
  dimension dm(mxttrd),cfm(4)
  parameter (blank = ' ',nmm = 'tparfile',&
  &cfm = (/'(f6.1) ','(i2)   ','(f12.7)','(E7.1) '/),&
  &c19 = 'TORSION OPTION FILE')
!
!     intrinsic functions
  intrinsic :: len_trim
!
!     init return
  ok = -1
!
!     kind of options file
  kd = cknd
!
  call marksep(eu,0,c19)
!
!     check if there is an options file name
  if (nomopf(1:1) .le. blank) then
    call marksep(eu,1,blank)
    ok = 0
    return
  end if
!
  do ii = 1,nrt
    jj = len_trim(lnt(ii))
    write(eu,300,err=200) lnt(ii)(1:jj)
!
!       output line index
    if (ii .eq. 1) then
!KIND
      write(eu,350,err=200) cknd
!
    else if (ii .eq. 2) then
!CPNOM     CPSM      [CPSP]
      fmt = sjoinrf(255, teln,cfm(1),nteln,mxteln)
      jj = len_trim(fmt)
      write(eu,5,err=200)&
      &tcpn,tsmg,fmt(1:jj)
!
    else if (ii .eq. 3) then
!COUPL     FPRT      FPRTS    [MODES]
      fmt = sjoinif(255, tmdnb,cfm(2),ntmdn,mxtmdn)
      jj = len_trim(fmt)
      write(eu,15,err=200)&
      &ncplg,fprt,fprts,fmt(1:jj)
!
    else if (ii .eq. 4) then
!INDEX     KC        DC        IC        RATIO
      do jj = 1,ncplg
        write(eu,25,err=200)&
        &cplgid(jj),cplgst(jj),cplgdp(jj),cplgin(jj),cplgsr(jj)
      end do
      write(eu,250,err=200)
!
    else if (ii .eq. 5) then
!DAMPING [MD_OFF] [LOG,RAD/S]
      write(eu,35,err=200)&
      &ntdmp,mdoff,telm,tlopt(1),tlopt(2)
!
    else if (ii .eq. 6) then
!MODE  RATIO
      do jj = 1,ntdmp
        write(eu,45,err=200)&
        &tdmmd(jj),tdmrt(jj)
      end do
      write(eu,250,err=200)
!
    else if (ii .eq. 7) then
!FILLET [SHSTR]  [STEQSF]
      buf = tpeqstidf(30)
      jj = len_trim(buf)
      if (jj .lt. 1) then
        write(eu,55,err=200) ntfil,tshyipu
      else
        write(eu,95,err=200) ntfil,tshyipu,buf(1:jj)
      end if
!
    else if (ii .eq. 8) then
!INDEX     RADIUS
      do jj = 1,ntfil
        write(eu,65,err=200)&
        &tfilid(jj),tfilrd(jj)
      end do
      write(eu,250,err=200)
!
    else if (ii .eq. 9) then
!TRANS     ITSTP     RSSTP     SIZE      INIT      FINT
      write(eu,75,err=200)&
      &nttra,ttris,ttrrs,ttrdsz,ttrit,ttret
!
    else if (ii .eq. 10) then
!DAMPING
      do jj = 1,nttra
        write(eu,85,err=200)&
        &ttrmd(jj)
      end do
      write(eu,250,err=200)
!
    else if (ii .eq. 11) then
!TRANSIENT DATA
!         torque equation
      if (.not. iseqt) then
!           time data
        fmt = sjoinrf(255, ttrdtt,cfm(3),ttrdsz,mxttrd)
        kk = len_trim(fmt)
        write(eu,300,err=200) fmt(1:kk)
      end if
!
      do jj = 1,nttra
!           torque equation
        if (.not. iseqt) then
!             torque data
          do kk = 1,ttrdsz
            dm(kk) = ttrdta(jj,kk)
          end do
          fmt = sjoinrf(255, dm,cfm(3),ttrdsz,mxttrd)
          kk = len_trim(fmt)
          write(eu,300,err=200) fmt(1:kk)
        else
!             torque equation
          kk = len_trim(tequt(jj))
          write(eu,300,err=200) tequt(jj)(1:kk)
        end if
      end do
      write(eu,250,err=200)
!
    else if (ii .eq. 12) then
!OPTIONS
      kk = len_trim(ops)
      write(eu,350,err=200) ops(1:kk)
!
    end if
!
  end do
!
!     check optional flexural-torsion data
  if (fto) then
!
    do ii = 1,nro
      jj = len_trim(lno(ii))
      write(eu,300,err=200) lno(ii)(1:jj)
!
!         output line index
      if (ii .eq. 1) then
!FTCNP     FTNBCY    FTSTPK    FTASM     SCLST
        fmt = sjoinif(255, siv,cfm(2),nsc,mxscls)
        kk = len_trim(fmt)
        write(eu,105,err=200) (ftv(ll),ll=1,4),fmt(1:kk)
      else if (ii .eq. 2) then
!SCUSTL
        fmt = sjoinrf(255, scy,cfm(4),nsc,mxscls)
        kk = len_trim(fmt)
        write(eu,350,err=200) fmt(1:kk)
      else if (ii .eq. 3) then
!FTSCSF
        fmt = sjoinif(255, sfv,cfm(2),nsc,mxscls)
        kk = len_trim(fmt)
        write(eu,350,err=200) fmt(1:kk)
      else if (ii .eq. 4) then
!KEYS      ALTQP     MXTQP     TQSTF     BDSTF     NTIND
        fmt = sjoinif(255, nsi,cfm(2),nnt,mxscls)
        kk = len_trim(fmt)
        write(eu,115,err=200) nks,(tvl(ll),ll=1,4),fmt(1:kk)
      else if (ii .eq. 5) then
!KIDX      DIST      HEIGHT    LENGTH    RF        WIDTH     RG
        do kk = 1,nks
          write(eu,125,err=200) nki(kk),(kvl(kk,ll),ll=1,6)
        end do
!
!           output line index
      end if
    end do
!       check optional flexural-torsion data
  end if
!
  call marksep(eu,1,blank)
!
!     return ok
  ok = 0
!
  return
!
200 inquire(unit=eu,name=ofn)
  ii = len_trim(ofn)
  errmsg = fomsgf(99, nmm,13,ofn(1:ii),1)
  return
!
5 format(f9.1,1x,f9.2,1x,a,/)
15 format(i9,1x,f9.2,1x,e9.4,1x,a,/)
25 format(i9,4(1x,e9.3))
35 format(i9,2(1x,i9),2(1x,L9),/)
45 format(i9,1(1x,f9.6))
55 format(i9,f9.4,/)
65 format(i9,1(1x,f9.4))
75 format(i9,2(1x,f9.5),1x,i9,2(1x,f9.5),/)
85 format(1(1x,f9.5))
95 format(i9,f9.4,2x,a,/)
105 format(f9.4,1x,e9.1,2(1x,f9.1),1x,a,/)
115 format(i9,4(1x,f9.2),1x,a,/)
125 format(i9,1x,6(f9.5,1x),/)
250 format()
300 format(a)
350 format(a,/)
!
end subroutine tparfile
!
!     ==================================================================
!>    @brief torsion export modal matrices FI and LEMDA.
!
!>    @param[in] job zero if HB, matrix market otherwise
!>    @param[in] std standard input output
!>    @param[out] errmsg error message
!>    @param[out] ok error flag
!
subroutine texport(job,std,errmsg,ok)
  use rd_textfun, only: fmmsgf, fomsgf
  use com_nmi, only: inm, sbv
  use com_tepm, only: rfi, rlam, ddm
  use rd_kinds, only: lrk, wp
  implicit none
!
  integer :: job, ok
  character(len=99) :: errmsg
  logical :: std
!
!     internal name
!
  integer :: mtg
  parameter (mtg = 500)
!
!     torsion modal space
!
  integer :: i, iu, ierr, m
  real(wp) :: a
  character(len=1) :: blank
  character(len=2) :: nm, nms
  character(len=3) :: c3
  character(len=5) :: cst
  character(len=6) :: secs
  character(len=7) :: nmm
  character(len=8) :: key, keys
  character(len=10) :: sec
  character(len=72) :: title
  character(len=255) :: ofn
!
  dimension a(mtg,mtg),nms(3),secs(2),keys(2)
!
  parameter (blank = ' ',c3 = ' - ',cst = 'stdio',&
  &nmm = 'texport',&
  &secs = (/'tphi_','tlam_'/),&
  &nms = (/'HB','mm','pl'/),keys = (/'PHI','LEM'/))
  data ierr/0/
!
  intrinsic :: len_trim
!
!     return
  ok = -1
  ierr = 0
!
!     check job, Harwell-Boeing, Matrix Market or plain
  if (job .lt. 0 .or. job .gt. 2) then
!       'export:invalid job id' !14
    errmsg = fmmsgf(99, nmm,14,blank,0)
    return
  end if
!
!     internal application name
  i = len_trim(inm)
  write(title,5) inm(1:i)
!     name suffix
  if (job .eq. 0) then
    nm = nms(1)
  else if (job .eq. 1) then
    nm = nms(2)
  else if (job .eq. 0) then
    nm = nms(3)
  end if
!
!     output
  if (.not. std) then
    iu = 12
  else
    iu = 6
    m = 5
    ofn = cst
  end if
!
!     copy PHI -> a
  call copmat2_rd(rfi,a,ddm,ddm,mtg,mtg,mtg,mtg)
  if (.not. std) then
    call tmntfnm(11,m,ofn)
    open(iu,file=ofn(1:m),err=100)
  else
    write(sec,150)secs(1),nm
    call marksec(sec,0)
  end if
!
  if (job .eq. 0) then
    key = keys(1)
!       Harwell/Boeing
    call dwritehb(title,key,iu,ddm,ddm,a,mtg,ierr)
  else
    write(title,200) inm(1:i),c3,keys(1)
    if (job .eq. 1) then
!         MatrixMarket
      call dwritemm(title,iu,ddm,ddm,a,mtg,ierr)
    else
!         plain
      call dwritepl(title,iu,ddm,a,mtg,ierr)
    end if
  end if
!     close PHI
  if (.not. std) then
    close(iu)
  else
    call marksec(sec,1)
  end if
  if (ierr .ne. 0) then
    errmsg = fmmsgf(99, nmm,ierr,blank,0)
    return
  end if
!
!     copy LEM -> a
  call copmat2_rd(rlam,a,ddm,ddm,mtg,mtg,mtg,mtg)
  if (.not. std) then
    call tmntfnm(15,m,ofn)
    open(iu,file=ofn(1:m),err=100)
  else
    write(sec,150) secs(2),nm
    call marksec(sec,0)
  end if
!
  if (job .eq. 0) then
    key = keys(2)
    call dwritehb(title,key,iu,ddm,ddm,a,mtg,ierr)
  else
    write(title,200) inm(1:i),c3,keys(2)
    if (job .eq. 1) then
      call dwritemm(title,iu,ddm,ddm,a,mtg,ierr)
    else
      call dwritepl(title,iu,ddm,a,mtg,ierr)
    end if
  end if
!     close LEM
  if (.not. std) then
    close(iu)
  else
    call marksec(sec,1)
  end if
  if (ierr .ne. 0) then
    errmsg = fmmsgf(99, nmm,ierr,blank,0)
    return
  end if
!
!     all done...
  ok = 0
  return
!
100 errmsg = fomsgf(99, nmm,1,ofn,1)
  return
!
5 format(a)
150 format(2a)
200 format(3a)
!
end subroutine texport
!
!     ==================================================================
!>    @brief flexural-torsion warning messages handing
!>     A octal based seven level its used 0-6.<br>
!>     dec = l0*8^0+l1*8^1+l2*8^2+l3*8^3+l4*8^4+l5*8^5+l6*8^6<br>
!
!>    @param[in] nin decimal warning code
!>    @param[out] nms number of warnig messages set
!>    @param[out] mou messages vector
!
!>    @see tflextor.f,messages.f
!
!>    \verbatim
!>    routine name - level
!>
!>    icval - 8^0 -> level 0
!>    cksnf - icval <= 8^0
!>    ftlmcf = icval*8^1+icval*8^2+icval*8^3 = 8^3 -> level 3
!>    stfcp - ftlmcf = 8^3 - level 3
!>    scstf - 8^4 - level 4
!>    grstf - 8^5 - level 5
!>    kpstf - 8^6 - level 6
!>    kwbstf - 8^7 - level 7
!>    kwscpf - kpstf+cksnf+scstf = 8^6 - level 6
!>    kwnstch - cksnf+scstf = 8^4  - level 4
!>
!>    reference error messages by level
!>    0 = crack factor, out of range
!>    1 = surface factor, out of range
!>    2 = sizing factor, out of range
!>    3 = reliab. factor, out of range
!>    4 = scstf:t/r out of range
!>    5 = grstf:he/re out of range
!>    6 = kpstf:dim<0,check dimensions
!>    7 = kwbstf:rc/do out of range
!>
!>    \endverbatim
!
subroutine flxwarn(nin,nms,mou)
  use rd_textfun, only: warnmsgf
  implicit none
!
  integer :: nin, nms
  character(len=*) :: mou
  dimension mou(8)
!
  logical :: ok
  integer :: i, j, k, n, m
  character(len=1) :: blank
  character(len=50) :: buf
  character(len=100) :: msg
  dimension buf(2)
  parameter (blank = ' ')
!
  intrinsic :: len, len_trim, min, mod
!
!     warning messages and indices, see messages.f
!     may have been changed, just for guidance.
!
!     'value out of expected range' !33
!     ' crack factor' !34
!     ' surface factor' !35
!     ' sizing factor' !35
!     ' reliability factor' !37
!     ' step change t/r' !38
!     ' notch h/r' !39
!     ' keyseat proximity dimension<0' !40
!     ' keyseat "B" r/d' !41
!
!     message buffer size
  m = 1
!     fixed message flag
  ok = .false.
!     get decimal code
  n = nin
!     level index
  i = 0
!     number of messages
  nms = 0
!     decimal -> octal
  do while (n .gt. 0)
    i = i+1
!       check if octal level is set
    if (mod(n,8) .gt. 0) then
!         just if not set
      if (.not. ok) then
!           message lenght
        m = len(mou(1))
!           initialize messages
        do k = 1,8
          mou(k) = ' '
        end do
!           set fixed message part
        buf(2) = warnmsgf(50, 33)
        k = len_trim(buf(2))
!           once flag
        ok = .true.
      end if
!
!         set level message -> i
      buf(1) = warnmsgf(50, 33+i)
      j = len_trim(buf(1))
!
!         write complete warning message -> msg
      write(msg,5) buf(1)(1:j),blank,buf(2)(1:k)
      j = len_trim(msg)
!         message/buffer size
      k = min(m,j)
!
!         levels count
      nms = nms+1
!
!         avoid lenght error
      if (k .gt. 0) mou(nms) = msg(1:k)
!
!         check if octal level is set if
    end if
!
!       next level
    n = n/8
!
!       level do
  end do
!
  return
!
5 format(3a)
!
end subroutine flxwarn
!
!     ==================================================================
!>    @brief flexural-torsion output
!
!>    @param[in] as sections aditional safety margin
!>    @param[in] tfa torsion stress factor
!>    @param[in] bfa bend stress factor
!>    @param[in] mxc dimension of columns output matrix vo,
!>     related to paramter mxscls
!>    @param[out] nc number of columns of output data matrix
!>    @param[out] vo output data matrix
!>    \verbatim
!>    1) si section index.
!>    2) kt: kind=1)notch,2)keyway "A",3)kw "B",4)step change+kw,5)step
!>    3) sd: sides: 0) no meaning, 1)right, 2)left.
!>    4) sy: yield strength.
!>    5) su: ultimate stress.
!>    6) st: torque stress.
!>    7) bz: bending stress.
!>    8) od: outer diameter.
!>    9) sf: surface finishing.
!>    10) sn: fatigue limit.
!>    11) sa: alternating torque stress.
!>    12) sg: mean torque stress.
!>    13) sx: maximum torque stress.
!>    14) bs: bending stress.
!>    15) fr: fillet radius.
!>    16) sh: step height.
!>    17) qs: crack sensitivity.
!>    18) ks(1): stress concentration factor torsion.
!>    19) ks(2): stress concentration factor bend.
!>    20) ks(3): stress concentration factor axial.
!>    21) ks(4): stress concentration factor keyway proximity.
!>    22) ka: fatigue correction factor surface.
!>    23) kb: fatigue correction factor sizing.
!>    24) kc: fatigue correction factor reliability.
!>    25) kd: fatigue correction factor temperature.
!>    26) vm1: Von-Mises equivalent stress, alternating.
!>    27) vm2: Von-Mises equivalent stress, meaning.
!>    28) vm3: Von-Mises equivalent stress, maximum.
!>    29) sf1: fatigue safety factors "Goodman".
!>    30) sf2: fatigue safety factor "ASME".
!>    31) wr: warning decimal code.
!>    \endverbatim
!>    @param[in] std standard input output
!>    @param[in] plt HPGL output
!>    @param[out] errmsg error message
!>    @param[out] ok error flag
!
subroutine ts_flxtor(as,tfa,bfa,nc,mxc,vo,std,plt,errmsg,ok)
  use rd_textfun, only: fomsgf, txtmsgf
  use com_sta, only: stm
  use rd_kinds, only: lrk
  implicit none
!
!     arguments
  real(lrk) :: as, tfa, bfa, vo
  integer :: nc, mxc, ok
  character(len=99) :: errmsg
  logical :: std, plt
  dimension vo(31,mxc)
!
!     stamp
!
  real(lrk) :: sy, kf
  integer :: ii, jj, kk, kt, sd, m, iu
  character(len=1) :: blank, c1, cp
  character(len=5) :: cst
  character(len=7) :: sec
  character(len=8) :: sct
  character(len=9) :: nmm, c9
  character(len=50) :: buf, mou
  character(len=255) :: ofn
  dimension c1(2),mou(8)
!     dm = Pa -> MPa
  parameter (blank = ' ',&
  &c1 = (/'A','B'/),c9 = 'warning :',&
  &cst = 'stdio',sec = 'tflxtor',nmm = 'ts_flxtor')
!
  intrinsic :: len, len_trim, nint
!
  ok = -1
!
  if (.not. std) then
    iu = 12
    call tmntfnm(23,m,ofn)
!       open output file
    open(iu,file=ofn(1:m),err=100)
  else
    iu = 6
    ofn = cst
    call marksec(sec,0)
  end if
!     stamp
  write(iu,15,err=200) stm
  call outdsc(iu)
!     new line
  write(iu,5,err=200)
!     Flexural - Torsion Analysis
  call fltout(iu,txtmsgf(50, 154))
!     new line
  write(iu,5,err=200)
!
!     add.factor    tor.facor   bnd.factor
  call fltout(iu,txtmsgf(50, 171))
  write(iu,75,err=200) as,tfa,bfa
  write(iu,5,err=200)
!
!     sections loop
  do jj = 1,nc
!
!       kt section kind code
!       1)notch, 2)keyway "A", 3)kw "B", 4)step change and kw, 5)step.
    kt = nint(vo(2,jj))
!       sides: 0) no meaning, 1)right, 2)left.
    sd = nint(vo(3,jj))
!       section yield strenght
    sy = vo(4,jj)
!       step change position text
    if (sd .eq. 1 .or. sd .eq. 2) then
!         at right - 172
!         at left  - 173
      buf = txtmsgf(50, 171+sd)
      sct = buf(1:len(sct))
    else
      sct = blank
    end if
!       section index
    ii = nint(vo(1,jj))
!       section index
    buf = txtmsgf(50, 155)
    kk = len_trim(buf)
    write(iu,25,err=200) buf(1:kk),ii
!       kind
    buf = txtmsgf(50, 168)
    kk = len_trim(buf)
!       1)notch, 2)keyway "A", 3)kw "B", 4)step change and kw, 5)step.
    cp = blank
    if (kt .eq. 1) then
!         notch
      buf = buf(1:kk)//txtmsgf(50, 156)
    else if (kt .eq. 2 .or. kt .eq. 3) then
!         keyway
      buf = buf(1:kk)//txtmsgf(50, 157)
!         A or B
      cp = c1(kt-1)
    else if (kt .eq. 4) then
!         keyway + step
      buf = buf(1:kk)//txtmsgf(50, 157)
      kk = len_trim(buf)
      buf = buf(1:kk)//txtmsgf(50, 158)
    else if (kt .eq. 5) then
!         step
      buf = buf(1:kk)//txtmsgf(50, 158)
    else
      buf = blank
    end if
    kk = len_trim(buf)
    if (kt .eq. 4 .or. kt .eq. 5) then
      write(iu,35,err=200) buf(1:kk),blank,sct
    else
      write(iu,35,err=200) buf(1:kk),blank,cp
    end if
!       warning code
    ii = nint(vo(31,jj))
    if (ii .gt. 0) then
!         get warning messages -> kk,mou
      call flxwarn(ii,kk,mou)
!         warning code
      write(iu,105,err=200) c9,ii
!         output warning messages
      do ii = 1,kk
        m = len_trim(mou(ii))
        write(iu,15,err=200) mou(ii)(1:m)
      end do
    end if
!       main dimensions
    call fltout(iu,txtmsgf(50, 169))
!       outer     step   radius
    call fltout(iu,txtmsgf(50, 170))
!        8) od: outer diameter.
!       16) sh: step height.
!       15) fr: fillet radius.
    write(iu,75,err=200)  vo(8,jj),vo(16,jj),vo(15,jj)
!       section stress sx,sa,sg,bs
!       stress (Pa)
    call fltout(iu,txtmsgf(50, 159))
!       tqmx    tqal    tav     bend
    call fltout(iu,txtmsgf(50, 160))
    kk = 11
    write(iu,45,err=200)&
    &vo(kk+2,jj),vo(kk,jj),vo(kk+1,jj),vo(kk+3,jj)
!       fatigue kf = ka*kb*kc*kd/as, sn
    kk = 22
    kf = vo(kk,jj)*vo(kk+1,jj)*vo(kk+2,jj)*vo(kk+3,jj)/as
!       ft.fact    res (MPa)
    call fltout(iu,txtmsgf(50, 161))
    kk = 10
    write(iu,55,err=200) kf,vo(kk,jj)
!       stress concentration factors
    call fltout(iu,txtmsgf(50, 162))
!       crack     torsion  bend     axial
    call fltout(iu,txtmsgf(50, 163))
    kk = 18
    write(iu,95,err=200)&
    &vo(kk-1,jj),vo(kk,jj),vo(kk+1,jj),vo(kk+2,jj)
!       Von-Mises eq.st (Pa)
    call fltout(iu,txtmsgf(50, 164))
!       max      alt      avg
    call fltout(iu,txtmsgf(50, 165))
    kk = 26
    write(iu,65,err=200)&
    &vo(kk+2,jj),vo(kk,jj),vo(kk+1,jj)
!       fatigue satic factors
    call fltout(iu,txtmsgf(50, 166))
!       static  Goodman     ASME
    call fltout(iu,txtmsgf(50, 167))
    kk = 28
    write(iu,85,err=200)&
    &sy/vo(kk,jj),vo(kk+1,jj),vo(kk+2,jj)
!       new line
    write(iu,5,err=200)
!
!       section loop
  end do
!
  ok = 0
!
!     sartori - 20220215: added if close files/blocks
  if (.not. std) then
    close(iu) ! close output file
  else
    call marksec(sec,1)
  end if
!
  return
!
100 errmsg = fomsgf(99, nmm,1,ofn,1)
  return
!
200 close(iu)
  errmsg = fomsgf(99, nmm,13,ofn,1)
  return
!
5 format()
15 format(a)
25 format(a,i2)
35 format(3a)
45 format(4(e12.4,1X))
55 format(1(f12.4,1X),e12.4)
65 format(3(e12.4,1X))
75 format(3(f12.5,1X))
85 format(3(f12.2,1X))
95 format(4(f12.3,1X))
105 format(1x,a,i8)
!
end subroutine ts_flxtor
!
