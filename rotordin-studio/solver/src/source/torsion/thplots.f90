!     $Id$
!     ==================================================================
!
!>    @file thplots.f
!>    @author
!>    francisco
!>    @date
!>    16-dec-19
!>    @brief torsion plot outputs, last changes:<br>
!>    new module - francisco - dec-19<br>
!>    added argument mtg on tmshp - francisco - dec-21.
!
!     ==================================================================
!>    @brief plot torsion mode shapes.
!
!>    @param[in] qtm number of modes
!>    @param[in] npv number of shaft sections
!>    @param[in] idm desired mode index vector
!>    @param[in] fmd mode frequencies rad/s
!>     already in desired mode index
!>    @param[in] py section axial coordinates (m)
!>    @param[in] tm  mode shape matrix
!>     already in desired mode index
!>    @param[in] xmd frequency, orbit and mode shape lines dimension
!>    @param[in] mts position and orbit dimension
!>    @param[in] mtg mode shape matrix, columns dimension
!>    @param[in] stdio flag for standard input and output
!>    @param[in] pfn output file name, used for non standard output
!
subroutine tmshp(qtm,npv,idm,fmd,py,tm,&
&xmd,mts,mtg,stdio,pfn)
  use rd_textfun, only: pltmsgf
  use com_opt, only: std
  use rd_kinds, only: lrk
  implicit none
!
!     arguments
  integer :: qtm, npv, idm, xmd, mts, mtg
  real(lrk) :: fmd, tm, py
  dimension idm(xmd),fmd(xmd),tm(xmd,mtg),py(mts)
  character(len=255) :: pfn
  logical :: stdio
!
!     locals
  integer :: i, j, k
  real(lrk) :: pyl, hz, rad2hzf
  character(len=1) :: c1
  character(len=4) :: c4
  character(len=5) :: pmm
  character(len=7) :: c7
  character(len=10) :: sec
  character(len=20) :: frq
!     plot messages
  character(len=30) :: ptxt
  parameter (c1 = ':',c4 = '.plt',pmm = 'tmshp',c7 = ': >100k')
!
!     malhap stuff
  integer :: ft, format, xachse, yachse, color
  real(lrk) :: nul, maxrig, maxtop, minbot, minlef
  real(lrk) :: xwert(npv+2), ywert(npv+2)
  character(len=8) :: zeit, prname
  character(len=10) :: datum
  character(len=30) :: name, abszis, ordina
  character(len=31) :: titelo, titelu
  character(len=70) :: reke
!
!     std plot output
!
  intrinsic :: nint
!
!     standard io
  std = stdio
!
!     malhap settings
  format = 5
  xachse = 1
  yachse = 1
  nul = 1E-12_lrk
!
!     titles
  call header(reke,zeit,datum,name,titelu)
!     y(m)
  abszis = pltmsgf(30, 46)
  ordina = pltmsgf(30, 54)
  prname = pmm
!
!     standard plot marker
  if (stdio) then
    write(sec,5)pmm,c4
    call marksec(sec,0)
  end if
!
!     initialize hpgl plot
  call plots(0,0,pfn)
!
!     plot colors: 1:Black, 2:Red, 3:Green, 4:Yellow, 5:Blue, 6:Magenta
!
!     plot title
  ptxt = pltmsgf(30, 35)
  j = len_trim(ptxt)
  write(titelo,15)ptxt(1:j)
!
!     plot axis size
  maxrig = py(npv)
  minlef = 0
!
!     y data
  do j = 1,npv
    xwert(j) = py(j)
  end do
!
!     loop on the modes
  do i = 1,qtm
!
    maxtop = -1
    minbot = 1
!
!       plot new page
    ft = 1
!
!       black
    color = 1
    call pen(color)
!
!       mode index
    k = idm(i)
!
    do j = 1,npv
      ywert(j) = tm(i,j)
      if (ywert(j) .gt. maxtop) maxtop = ywert(j)
      if (ywert(j) .lt. minbot) minbot = ywert(j)
    end do
!       zero line
    if (minbot .gt. 0) minbot = 0
!
!       mode line
    call malhap(npv, format, ft, npv, xachse, yachse,&
    &nul, maxrig, maxtop, minbot, minlef,&
    &xwert, ywert,&
    &reke, zeit, datum, abszis, ordina,&
    &name, titelo, titelu, prname,color)
!
!       mode legend header
    pyl = 17.8_lrk
    ptxt = pltmsgf(30, 36)
    frq = ptxt(1:20)
    j = len_trim(frq)
    call symbol(17.8_lrk,pyl,0.2_lrk,frq,0,0._lrk,j)
!
!       mode and frequecy
    hz = rad2hzf(fmd(i))
!       k = mode index
    if (hz .lt. 100) write(frq,10) k,c1,hz
    if (hz .ge. 100  .and.  hz .lt. 1000)&
    &write(frq,20)k,c1,hz
    if (hz .ge. 1000  .and. hz .lt. 99999)&
    &write(frq,30) k,c1,nint(hz)
    if (hz .ge. 100000) write(frq,40) k,c7
!
    pyl = 17.2_lrk
    call legend(17._lrk,pyl,-1,0.16_lrk,color,20,frq)
!
!       shaft center line
    do j = 1,npv
      ywert(j) = 0
    end do
!
!       blue
    color = 5
!
!       plot center line
    call malhap(npv, format, ft, npv, xachse, yachse,&
    &nul, maxrig, maxtop, minbot, minlef,&
    &xwert, ywert,&
    &reke, zeit, datum, abszis, ordina,&
    &name, titelo, titelu, prname,color)
!
  end do
!
!     standard plot marker
  if (stdio) then
    call bflush
    call marksec(sec,1)
  else
    call plots(1,0,pfn)
  end if
!
  return
!
5 format(2a)
10 format(i2,a,f5.2)
15 format(a)
20 format(i2,a,f5.1)
30 format(i2,a,i5)
40 format(i2,a)
!
end subroutine tmshp
!
!     ==================================================================
!>    @brief given section id search for a range row that contains it.
!
!>    @param[in] ids section id
!>    @param[in] rnvl range matrix index of vin in each range
!>    @param[in] hval number of elements in each range
!>    @param[in] mts output vector dimension
!>    @param[in] nrdm range dimension
!>    @return range row id that contains given section id or zero if not
!
integer function rgnidf(ids,rnvl,hval,mts,nrdm)
  implicit none
!
  integer :: ids, rnvl, hval, mts, nrdm
  dimension rnvl(nrdm,mts),hval(nrdm)

  integer :: rgnid, ii, jj, kk
!
  rgnid = 0
  do ii = 1,nrdm
    jj = hval(ii)
    do kk = 1,jj
      if (rnvl(ii,kk) .eq. ids) then
        rgnid = ii
        exit
      end if
    end do
    if (rgnid .gt. 0) exit
  end do
!
  rgnidf = rgnid
!
  return
!
end function rgnidf
!
!     ==================================================================
!>    @brief plot section geometry for torsion static stress.
!
!>    @param[in] stdio flag for standard input and output
!>    @param[in] rnvl range matrix index of vin in each range
!>    @param[in] hval number of elements in each range
!>    @param[in] rgvl range values matrix minimum and maximum each range
!>    @param[in] mts output vector dimension
!>    @param[in] nrdm range dimension and number of divisions
!
subroutine tsgeom(stdio,rnvl,hval,rgvl,mts,nrdm)
  use rd_textfun, only: fomsgf, pltmsgf
  use com_exgeo, only: cc, ik, cx, cy, mxx, mnx, last
  use com_opt, only: std
  use rd_kinds, only: lrk
  implicit none
!
  integer :: mts, nrdm, rnvl, hval
  real(lrk) :: rgvl
  dimension rnvl(nrdm,mts),hval(nrdm),rgvl(nrdm,2)
  logical :: stdio
!
  real(lrk) :: mm, fns, rsx, rsy, re, si, se, trextf
  integer :: i, j, k, idk, idv, idr, rgnidf
  character(len=1) :: ddot
  character(len=3) :: c3(2)
  character(len=4) :: c4
  character(len=5) :: pmm
  character(len=6) :: nmm
!     millimeter scale, font size offset
  parameter (mm = 1000._lrk,fns = 45._lrk,ddot = ':',c3 = (/'mxw','>99'/),pmm = 'tsstr',c4 = '.plt',nmm = 'tsgeom')
  character(len=40) :: ccm
!     plot messages,truncated, check messages.f
  character(len=30) :: buf
!     error messages
  character(len=99) :: errmsg
!     output section name
  character(len=10) :: sec
!     output file name, used for non standard output
  character(len=255) :: pfn
!     malhap stuff
  integer :: ft, format, xachse, yachse, color, mxw
  parameter (mxw = 20)
  real(lrk) :: nul, maxrig, maxtop, minbot, minlef
  real(lrk) :: xwert(mxw+2), ywert(20+2)
  character(len=8) :: zeit, prname
  character(len=10) :: datum
  character(len=30) :: name, abszis, ordina
  character(len=31) :: titelo, titelu
  character(len=70) :: reke
!     std plot output
!     geometry
  integer :: npgx, npix
  parameter(npgx = 999,npix = 9)
!     index and order,1->2->3->4->1
!     number of elements
!     ik = geometry kind index,see expgeo
!     1=section,2=division,3=disk,4=lateral bearing,5=torsion restrictio
!     6=unbalance excitation,7=static force,8=static torque,9=transitory
!     10=harmonic torque,11=frequency response,12=concentrated
!
  intrinsic :: abs, len_trim
!
!     standard io
  std = stdio
!
  if (.not. std) then
!       output file name
    call tmntfnm(-8,i,pfn)
  end if
!
!     malhap settings
  format = 5
  xachse = 1
  yachse = 1
  nul = 1E-12_lrk
!     titles
  call header(reke,zeit,datum,name,titelu)
!     length (mm)
  abszis = pltmsgf(30, 61)
!     diameter (mm)
  ordina = pltmsgf(30, 21)
!     static torsion stress
  titelo = pltmsgf(30, 37)
  prname = pmm
!
!     standard plot
  if (stdio) then
    write(sec,5)pmm,c4
    call marksec(sec,0)
  end if
!
!     maximum and minimum
  maxrig = mxx
  minlef = mnx
  maxtop = mxx/2
  minbot = -mxx/2
!
!     blue
  color = 5
!
!     initialize hpgl plot
  call plots(0,0,pfn)
!     remove axis grid
  call gridsoff()
!
!     division index
  idv = 1
!
  do i = 1,last
!       kind of geometry
    idk = ik(i)
!
!       number of coordinates to plot in a path
    k = abs(cc(i))
    if (k .gt. mxw) then
!         upperbound
      errmsg = fomsgf(99, nmm,4,c3(1),0)
!         stop
      call lmsg(1,errmsg)
    end if
!
    do j = 1,k
      xwert(j) = cx(i,j)
      ywert(j) = cy(i,j)
    end do
    if (.not. cc(i) .lt. 0) then
!         "close" geometry
      k = k+1
      xwert(k) = cx(i,1)
      ywert(k) = cy(i,1)
    end if
!       if number of point not the same, adjust
    call rscale(xwert,ywert,mxw,k)
    ft = i
!       changed to accomodate "k" number of points
!       plot k points
    call malhap(k, format, ft, mxw, xachse, yachse,&
    &nul, maxrig, maxtop, minbot, minlef,&
    &xwert, ywert,&
    &reke, zeit, datum, abszis, ordina,&
    &name, titelo, titelu, prname,color)
!
!       kind of element
    if (idk .eq. 2) then
!         division id is ordered
      idv = idv+1
!         external section radius,section start, section end
      re = trextf(idv,si,se)
!         range id for this division < nrdm
      idr = rgnidf(idv,rnvl,hval,mts,nrdm)
!
!         check for feasibility
      if (.not. (idr .gt. nrdm .or. idr .lt. 1)) then
!           idr in bound
        if (idr.lt. 10) then
          write(ccm,15) idr
        else if (idr.lt. 100) then
          write(ccm,25) idr
        else
!             should not happen, too much ranges?
          write(ccm,35) c3(2)
        end if
!           range id output
        k = len_trim(ccm)
!           mid section, mm => millimeter
        rsx = (se-(se-si)/2)*mm
!           bellow section radius
        rsy = -1.1_lrk*re*mm-fns
!           symbol on data scale
        call csymbol (rsx,rsy,.28_lrk,ccm(1:k),0,0._lrk,k)
      end if
    end if
!
  end do
!
!     range output
!
!     black
  call pen(1)
!
!     range title
  buf = pltmsgf(30, 38)
  k = len_trim(buf)
!     x position centimeter
  rsx = 15._lrk
!     y position centimeter
  rsy = 17._lrk+0.5_lrk
  call symbol (rsx,rsy,.22_lrk,buf(1:k),0,0._lrk,k)
!
  idv = 0
  do i = 1,nrdm
    j = hval(i)
    if (j .gt. 0) then
!         range values, min:max
      write(ccm,100)i,rgvl(i,1),ddot,rgvl(i,2)
      k = len_trim(ccm)
!         x position centimeter
      rsx = 15._lrk
!         y position centimeter
      rsy = 17._lrk-idv*0.5_lrk
      call symbol (rsx,rsy,.22_lrk,ccm(1:k),0,0._lrk,k)
      idv =idv+1
    end if
  end do
!
!     put axis grid back
  call gridson()
!
!     standard plot marker
  if (stdio) then
    call bflush
    call marksec(sec,1)
  else
    call plots(1,0,pfn)
  end if
!
  return
!
5 format(2a)
15 format(i1)
25 format(i2)
35 format(a)
100 format(i2,1x,en8.0,a,en8.0)
!
end subroutine tsgeom
!
!     ==================================================================
!>    @brief torsion frequency response plot.
!
!>    @param[in] rpg speed rpm
!>    @param[in] amp complex amplitudes matrix in rad
!>    @param[in] rdr angular displacement on output division with
!>     speed ratio (rad)
!>    @param[in] tqr torque on output division with speed ratio (N.m)
!>    @param[in] str shear stress on output division
!>    @param[in] nap number of amplitude points
!>    @param[in] nrp number of speeds
!>    @param[in] mxp amplitude first and rpg dimension
!>    @param[in] mtr amplitude second dimension
!>    @param[in] anrd plot output angular displacement in radian
!>    @param[in] lgp plot output logarithmic angular displacement
!>    @param[in] stdio flag for standard input and output
!>    @param[in] pfn output file name, used for non standard output
!
subroutine tfrqr(rpg,amp,rdr,tqr,str,nap,nrp,mxp,mtr,&
&anrd,lgp,stdio,pfn)
  use rd_textfun, only: ciff, pltmsgf
  use com_opt, only: std
  use com_unb0, only: pr, desp, ori, np, nm
  use rd_kinds, only: lrk, wp
  implicit none
!
  integer :: nap, nrp, mxp, mtr
  real(lrk) :: rpg, rdr, tqr, str
  complex(wp) :: amp
  dimension rpg(mtr),rdr(mxp,mtr),tqr(mxp,mtr),&
  &str(mxp,mtr),amp(mxp,mtr)
  character(len=255) :: pfn
  logical :: anrd, lgp, stdio
!
  integer :: i, j, k, mki, mxki
  real(lrk) :: an, am, mn, mx, dl, hpi, hdg, scl
!     stress scale
  parameter (scl = 1)
  real(lrk) :: riff, todegf, todegpf, rpm2hzf, rpif, rargf
!     output section name
  character(len=4) :: c4
  character(len=5) :: nmm
  parameter(nmm = 'tfrqr',c4 = '.plt')
  character(len=10) :: sec
  character(len=16) :: crd
!     plot messages
!
!     malhap stuff
  integer :: ft, format, xachse, yachse, color
  real(lrk) :: nul, maxrig, maxtop, minbot, minlef, xwert, ywert
  dimension xwert(nrp+2),ywert(nrp+2)
  character(len=8) :: zeit, prname
  character(len=10) :: datum
  character(len=30) :: name, abszis, ordina
  character(len=31) :: titelo, titelu
  character(len=70) :: reke
!
!     unbalance
  integer :: mxr
  parameter (mxr = 9)
!
!     std plot output
!
  intrinsic :: abs, log10, real
!
!     initialize
!     pi or 180
  hpi = rpif()
  hdg = riff(anrd,hpi,todegf(hpi))
!
!     standard io
  std = stdio
!
!     malhap settings
  format = 5
  xachse = 1
  yachse = 1
  nul = 1E-12_lrk
!
!     speed range -> Hz
  minlef = rpm2hzf(rpg(1))
  maxrig = rpm2hzf(rpg(nrp))
!
!     titles
  call header(reke,zeit,datum,name,titelu)
!     x -> frequency
  abszis = pltmsgf(30, 2)
  prname = nmm
!
!     standard plot marker
  if (stdio) then
    write(sec,'(2a)')nmm,c4
    call marksec(sec,0)
  end if
!
!     kind index
  mki = 0
!
!     max kind index
  call mxmarks(mxki)
!
!     line colors
  color = 1
!
!     initialize hpgl plot
  call plots(0,0,pfn)
!
!     angular displacement amplitude
  mn = 1E9_lrk
  mx = 0._lrk
  do i = 1,nap
    do j = 1,nrp
      an = real(abs(amp(i,j)), lrk)
      dl = riff(anrd,an,todegf(an))
      am = riff(lgp,log10(dl),dl)
      if(am .le. mn) mn = am
      if(am .ge. mx) mx = am
    end do
  end do
!
!     y margins
  minbot = mn
  maxtop = mx
!
!     title
  titelo = pltmsgf(30, 39)
!     angular displacement
  ordina = ciff(30, anrd,pltmsgf(30, 50),pltmsgf(30, 41))
!
  ft = 1
!     plot frequency lines
  do i = 1,nap
    do j = 1,nrp
!         angular displacement
      an = real(abs(amp(i,j)), lrk)
      dl = riff(anrd,an,todegf(an))
      am = riff(lgp,log10(dl),dl)
!         frequency Hz
      xwert(j) = rpm2hzf(rpg(j))
      ywert(j) = am
    end do
!       log/linear graph (lgp)
    call malhapax (nrp, format, ft, nrp, xachse, yachse,&
    &nul, maxrig, maxtop, minbot, minlef,&
    &xwert, ywert,&
    &reke, zeit, datum, abszis, ordina,&
    &name, titelo, titelu, prname,color,.false.,lgp)
!       marks
    call marks(xwert,ywert,80._lrk,nrp,nrp+2,8,mki)
    if (ft .eq. 2) then
      crd = 'pos. (mm)'
      k = len_trim(crd)
      call symbol (1._lrk,18._lrk,.28_lrk,crd(1:k),0,0._lrk,k)
    end if

!       response position info (*1000 = mm)
    write(crd,'(f8.2)')pr(i)*1000._lrk
    k = len_trim(crd)
    dl = 17.6_lrk-(i-1)*0.38_lrk
    call legend (1._lrk,dl,mki,.3_lrk,color,k,crd(1:k))
!
!       marker id
    mki = mki+1
    if (mki .gt. mxki) mki = 0
!
  end do
!
!     phase of angular displacement
  mn = 1E9_lrk
  mx = 0._lrk
  do i = 1,nap
    dl = 0._lrk
    do j = 1,nrp
!         angle of angular displacement
      an = rargf(amp(i,j))
      am = riff(anrd,an,todegpf(an,1))
!         angle sequence correction
      if (j .gt. 1) then
        do while (am-dl .gt. hdg)
          am = am-2*hdg
        end do
        do while (am-dl .lt. -hdg)
          am = 2*hdg+am
        end do
      end if
      dl = am
      if(am .le. mn) mn = am
      if(am .ge. mx) mx = am
    end do
  end do
!
  minbot = mn
  maxtop = mx
!     angle deg/rad
  titelo = pltmsgf(30, 40)
  ordina = ciff(30, anrd,pltmsgf(30, 8),pltmsgf(30, 9))
!
!     reset scales
  call sreset(3)
!     kind index
  mki = 0
  ft = 1
  do i = 1,nap
    dl = 0._lrk
    do j = 1,nrp
      an = rargf(amp(i,j))
      am = riff(anrd,an,todegpf(an,1))
!         angle sequence correction
      if (j .gt. 1) then
        do while (am-dl .gt. hdg)
          am = am-2*hdg
        end do
        do while (am-dl .lt. -hdg)
          am = 2*hdg+am
        end do
      end if
      dl = am
!         frequency Hz
      xwert(j) = rpm2hzf(rpg(j))
      ywert(j) = am
    end do
!       linear graph
    call malhap (nrp, format, ft, nrp, xachse, yachse,&
    &nul, maxrig, maxtop, minbot, minlef,&
    &xwert, ywert,&
    &reke, zeit, datum, abszis, ordina,&
    &name, titelo, titelu, prname,color)
!       marks
    call marks(xwert,ywert,80._lrk,nrp,nrp+2,8,mki)
    if (ft .eq. 2) then
      crd = 'pos. (mm)'
      k = len_trim(crd)
      call symbol (1._lrk,18._lrk,.28_lrk,crd(1:k),0,0._lrk,k)
    end if
    write(crd,'(f8.2)')pr(i)*1000._lrk
    k = len_trim(crd)
    dl = 17.6_lrk-(i-1)*0.38_lrk
    call legend (1._lrk,dl,mki,.3_lrk,color,k,crd(1:k))
!
!       marker id
    mki = mki+1
    if (mki .gt. mxki) mki = 0
!
  end do
!
!     torsion angle on division
  mn = 1E9_lrk
  mx = 0._lrk
  do i = 1,nap
    do j = 1,nrp
!         torsion angle
      an = rdr(i,j)
      am = riff(anrd,an,todegf(an))
      if(am .le. mn) mn = am
      if(am .ge. mx) mx = am
    end do
  end do
!
  minbot = mn
  maxtop = mx
!     angle deg/rad
  titelo = pltmsgf(30, 40)
  ordina = ciff(30, anrd,pltmsgf(30, 51),pltmsgf(30, 53))
!
!     reset scales
  call sreset(3)
!     kind index
  mki = 0
  ft = 1
  do i = 1,nap
    do j = 1,nrp
      an = rdr(i,j)
      am = riff(anrd,an,todegf(an))
!         frequency Hz
      xwert(j) = rpm2hzf(rpg(j))
      ywert(j) = am
    end do
!
!       linear graph
    call malhap (nrp, format, ft, nrp, xachse, yachse,&
    &nul, maxrig, maxtop, minbot, minlef,&
    &xwert, ywert,&
    &reke, zeit, datum, abszis, ordina,&
    &name, titelo, titelu, prname,color)
!       marks
    call marks(xwert,ywert,80._lrk,nrp,nrp+2,8,mki)
    if (ft .eq. 2) then
      crd = 'pos. (mm)'
      k = len_trim(crd)
      call symbol (1._lrk,18._lrk,.28_lrk,crd(1:k),0,0._lrk,k)
    end if
    write(crd,'(f8.2)')pr(i)*1000._lrk
    k = len_trim(crd)
    dl = 17.6_lrk-(i-1)*0.38_lrk
    call legend (1._lrk,dl,mki,.3_lrk,color,k,crd(1:k))
!
!       marker id
    mki = mki+1
    if (mki .gt. mxki) mki = 0
!
  end do
!
!     torque on division
  mn = 1E9_lrk
  mx = 0._lrk
  do i = 1,nap
    do j = 1,nrp
!         torsion torque on division
      am = tqr(i,j)
      if(am .le. mn) mn = am
      if(am .ge. mx) mx = am
    end do
  end do
!
  minbot = mn
  maxtop = mx
!     torque
  titelo = pltmsgf(30, 52)
  ordina = pltmsgf(30, 48)
!
!     reset scales
  call sreset(3)
!     kind index
  mki = 0
  ft = 1
  do i = 1,nap
    do j = 1,nrp
      am = tqr(i,j)
!         frequency Hz
      xwert(j) = rpm2hzf(rpg(j))
      ywert(j) = am
    end do
!
!       log/linear graph (lgp)
    call malhap (nrp, format, ft, nrp, xachse, yachse,&
    &nul, maxrig, maxtop, minbot, minlef,&
    &xwert, ywert,&
    &reke, zeit, datum, abszis, ordina,&
    &name, titelo, titelu, prname,color)
!       marks
    call marks(xwert,ywert,80._lrk,nrp,nrp+2,8,mki)
    if (ft .eq. 2) then
      crd = 'pos. (mm)'
      k = len_trim(crd)
      call symbol (1._lrk,18._lrk,.28_lrk,crd(1:k),0,0._lrk,k)
    end if
    write(crd,'(f8.2)')pr(i)*1000._lrk
    k = len_trim(crd)
    dl = 17.6_lrk-(i-1)*0.38_lrk
    call legend (1._lrk,dl,mki,.3_lrk,color,k,crd(1:k))
!
!       marker id
    mki = mki+1
    if (mki .gt. mxki) mki = 0
!
  end do
!
!     shear stress on division
  mn = 1E9_lrk
  mx = 0._lrk
  do i = 1,nap
    do j = 1,nrp
!         torsion shear stress/1e6 -> MPa
      am = str(i,j)/scl
      if(am .le. mn) mn = am
      if(am .ge. mx) mx = am
    end do
  end do
!
  minbot = mn
  maxtop = mx
!     shear stress
  titelo = pltmsgf(30, 55)
  ordina = pltmsgf(30, 56)
!
!     reset scales
  call sreset(3)
!     kind index
  mki = 0
  ft = 1
  do i = 1,nap
    do j = 1,nrp
!         scaled to MPa with scl
      am = str(i,j)/scl
!         frequency Hz
      xwert(j) = rpm2hzf(rpg(j))
      ywert(j) = am
    end do
!
!       linear graph
    call malhap (nrp, format, ft, nrp, xachse, yachse,&
    &nul, maxrig, maxtop, minbot, minlef,&
    &xwert, ywert,&
    &reke, zeit, datum, abszis, ordina,&
    &name, titelo, titelu, prname,color)
!       marks
    call marks(xwert,ywert,80._lrk,nrp,nrp+2,8,mki)
    if (ft .eq. 2) then
      crd = 'pos. (mm)'
      k = len_trim(crd)
      call symbol (1._lrk,18._lrk,.28_lrk,crd(1:k),0,0._lrk,k)
    end if
    write(crd,'(f8.2)')pr(i)*1000._lrk
    k = len_trim(crd)
    dl = 17.6_lrk-(i-1)*0.38_lrk
    call legend (1._lrk,dl,mki,.3_lrk,color,k,crd(1:k))
!
!       marker id
    mki = mki+1
    if (mki .gt. mxki) mki = 0
!
  end do
!
!     standard plot marker
  if (stdio) then
    call bflush
    call marksec(sec,1)
  else
    call plots(1,0,pfn)
  end if
!
  return
!
end subroutine tfrqr
!
!     ==================================================================
!>    @brief Tosion Campbell diagram plot.
!
!>    @param[in] teln speed time vector, 1x, 2x, ...
!      negative value represents 2 x grid frequency (slip)
!>    @param[in] px intersections vector 1x, 2x, 0.5x
!>    @param[in] py critical frequencies
!>    @param[in] git critical frequencies by speed matrix (rad/s)
!>    @param[in] nini initial speed (rpm)
!>    @param[in] nfin initial speed (rpm)
!>    @param[in] tcpn Campbell nominal speed (rpm)
!>    @param[in] tsmg nominal speed separation margin (pu)
!>    @param[in] ncn number of intersctions vector
!>    @param[in] nteln number of speed time vector
!>     elements on teln vector
!>    @param[in] nnf number of critical speed
!>    @param[in] mtg git vector dimension
!>    @param[in] mxteln dimension of speed time vector
!>    @param[in] mxpt dimension of intersection vectors
!>    @param[in] stdio flag for standard input and output
!>    @param[in] pfn output file name, used for non standard output
!
subroutine tcpblp(teln,px,py,git,nini,nfin,&
&tcpn,tsmg,&
&ncn,nteln,nnf,mtg,mxteln,mxpt,stdio,pfn)
  use rd_textfun, only: pltmsgf
  use com_opt, only: std
  use rd_kinds, only: lrk
  implicit none
!
!     arguments
  integer :: ncn, nteln, nnf, mtg, mxteln, mxpt
  real(lrk) :: teln, git, px, py, nini, nfin, tcpn, tsmg
  dimension teln(mxteln),&
  &px(mxpt),py(mxpt),git(mtg)
  character(len=255) :: pfn
  logical :: stdio
!
!     locals
  real(lrk) :: thz
  parameter (thz = 60)
  real(lrk) :: dr, rad2hzf

  integer :: l, k, m, mid, midmx, pdm, mxf
!     plot dimension,max frequency
  parameter (pdm =2,mxf = 2)
  character(len=4) :: c4
  character(len=5) :: nmm
  parameter(nmm = 'tcpbl',c4 = '.plt')
!     output section name
  character(len=10) :: sec
  character(len=20) :: out
!     plot messages
  logical :: pok
!
!     malhap stuff
  integer :: ft, format, xachse, yachse, color
  real(lrk) :: nul, maxrig, maxtop, minbot, minlef
  real(lrk) :: xwert(pdm+2), ywert(pdm+2)
  character(len=8) :: zeit, prname
  character(len=10) :: datum
  character(len=30) :: name, abszis, ordina
  character(len=31) :: titelo, titelu
  character(len=70) :: reke
!
!     std plot output
!
  intrinsic :: abs, len_trim, nint
!
!     standard io
  std = stdio
!
!     malhap settings
  format = 5
  xachse = 1
  yachse = 1
  nul = 0.001_lrk
!
!     margins
!     x max
  maxrig = nfin
!     xmin
  minlef = nini
!
!     y min
  minbot = 0._lrk
!     ymax
  maxtop = mxf*nfin/thz
!
!     titles
  call header(reke,zeit,datum,name,titelu)
  abszis = pltmsgf(30, 1)
  ordina = pltmsgf(30, 2)
  titelo = pltmsgf(30, 42)
  prname = nmm
!     standard plot marker
  if (stdio) then
    write(sec,'(2a)') nmm,c4
    call marksec(sec,0)
  end if
!     line colors
!     default Pen colors: 1:Black, 2:Red, 3:Green, 4:Yellow, 5:Blue,
!     6:Magenta
!
!     black
  color = 1
!     initialize hpgl plot
  call plots(0,0,pfn)
!
!     plot frequency lines
  ft = 1
  xwert(1) = nini
  xwert(2) = nfin
  do k = 1,nnf
    ywert(1) = rad2hzf(git(k))
    ywert(2) = ywert(1)
!       check natural frequency value
    if (ywert(2) .gt. maxtop) exit
    call malhap (2, format, ft, pdm, xachse, yachse,&
    &nul, maxrig, maxtop, minbot, minlef,&
    &xwert, ywert,&
    &reke, zeit, datum, abszis, ordina,&
    &name, titelo, titelu, prname,color)

!       legends
    if (ywert(1) .lt. 99.99_lrk) then
      write(out,'(f5.2,1x,''(hz)'')')ywert(1)
    else if (ywert(1) .lt. 999.9_lrk) then
      write(out,'(f5.1,1x,''(hz)'')')ywert(1)
    else
      write(out,'(i5,1x,''(hz)'')')nint(ywert(1))
    end if
    l = len_trim(out)
!       y position
    dr = 17.8_lrk-(k-1)*0.38_lrk
    call legend(17.3_lrk,dr,-1,0.18_lrk,color,l,out)
  end do
!
!     maximum available marker kind id
  call mxmarks(midmx)
!     init marker kind
  mid = 1
!
!     x rpm lines
!
  m = 0
  do l = 1,nteln
!       normal x lines
!
!       blue
    color = 5
!
!       ok to plot
!
    if (teln(l) .gt. 0) then
      ywert(1) = minlef*teln(l)/thz
      xwert(2) = nfin
      ywert(2) = teln(l)*nfin/thz
      if (ywert(2) .gt. maxtop) then
        xwert(2) = maxtop/teln(l)*thz
        ywert(2) = maxtop
      end if
    else if (tcpn .gt. 0) then
!         2x line frequency
!
!         green
      color = 3
!
      xwert(2) = tcpn
      ywert(2) = tcpn*teln(l)/tcpn+abs(teln(l))
      if (xwert(2) .gt. nfin) then
!           2x frequency speed is not in range
        xwert(2) = nfin
        ywert(2) = nfin*teln(l)/tcpn+abs(teln(l))
      end if
      xwert(1) = nini
      ywert(1) = nini*teln(l)/tcpn+abs(teln(l))
      if (ywert(1) .gt. maxtop) then
!           2x frequency is not in range
        ywert(1) = maxtop
        xwert(1) = (maxtop+teln(l))*tcpn/teln(l)
      end if
    end if
!       ok to plot
    pok = xwert(2) .gt. minlef .and. ywert(1) .le. maxtop
!
    if (pok) then
      call malhap (2, format, ft, pdm, xachse, yachse,&
      &nul, maxrig, maxtop, minbot, minlef,&
      &xwert, ywert,&
      &reke, zeit, datum, abszis, ordina,&
      &name, titelo, titelu, prname,color)
!
!         marks
      call marks(xwert,ywert,60._lrk,2,pdm+2,3,mid)
!
!         legends
      if (teln(l) .gt. 0) then
        write(out,'(f4.1,''x'')')teln(l)
      else
        write(out,'(f4.0)')abs(teln(l))
      end if
      k = len_trim(out)
!         y position
      dr = 17.8_lrk - m*0.38_lrk
      call legend(0.5_lrk,dr,mid,0.18_lrk,color,k,out)
      m = m+1
!
!       rotate marker kind
      mid = mid+1
      if (mid .gt. midmx) mid = 2
    end if
!
  end do
!
!     separation margins
!     have nominal speed (rpm)
  if (tcpn .gt. 0) then
!
!       nominal speed in on campbell nini-nfin range
    if (tcpn .lt. maxrig .and. tcpn .gt. minlef) then
!         nominal speed
!
!         black
      color = 1
!
!         vertical lines
      xwert(1) = tcpn
      xwert(2) = tcpn
      ywert(1) = 0
      ywert(2) = maxtop
      call malhap (2, format, ft, pdm, xachse, yachse,&
      &nul, maxrig, maxtop, minbot, minlef,&
      &xwert, ywert,&
      &reke, zeit, datum, abszis, ordina,&
      &name, titelo, titelu, prname,color)
!
      if (tsmg .gt. 0 .and. tcpn*(1-tsmg) .gt. minlef) then
!           margin value is set
!           lower separation margin
!
!           red
        color = 2
!           dashed line type
        call ltype(2,1)
!
        xwert(1) = tcpn*(1-tsmg)
        xwert(2) = xwert(1)
        call malhap (2, format, ft, pdm, xachse, yachse,&
        &nul, maxrig, maxtop, minbot, minlef,&
        &xwert, ywert,&
        &reke, zeit, datum, abszis, ordina,&
        &name, titelo, titelu, prname,color)
      end if
    end if
!
    if (tsmg .gt. 0) then
!         upper separation margin
      dr = tcpn*(1+tsmg)
      if (dr .lt. maxrig .and. dr .gt. minlef) then
!           separation margin
!
!           red
        color = 2
!           dashed line type
        call ltype(2,1)
!
        xwert(1) = tcpn*(1+tsmg)
        xwert(2) = xwert(1)
        call malhap (2, format, ft, pdm, xachse, yachse,&
        &nul, maxrig, maxtop, minbot, minlef,&
        &xwert, ywert,&
        &reke, zeit, datum, abszis, ordina,&
        &name, titelo, titelu, prname,color)
      end if
    end if
!       restore default line type
    call ltype(-1,0)
  end if
!
!     intersection points
!
!     magenta
  color = 6
  call pen(color)
!
  do k = 1,ncn
    if (px(k) .lt. nfin .and. px(k) .gt. nini&
    &.and. py(k) .le. maxtop .and. py(k) .gt. minbot) then
!
!         circle
      call mark(px(k),py(k),50.0_lrk,4)
!
    end if
  end do
!
!     standard plot marker
  if (stdio) then
    call bflush
    call marksec(sec,1)
  else
    call plots(1,0,pfn)
  end if
!
  return
!
end subroutine tcpblp
!
!     ==================================================================
!>    @brief Tosion harmonic response plot.
!>     Torsion periodic forced response.
!
!>    @param[in] ntp number of time steps
!>    @param[in] pnui number of unique input positions
!>    @param[in] np number of response positions
!>    @param[in] cip unique input positions (m)
!>    @param[in] crp response positions (m)
!>    @param[in] mth time output input row dimension
!>    @param[in] mxb harmonic dimension
!>    @param[in] mxr respose position vector dimension
!>    @param[in] htv time vector (s)
!>    @param[in] hit input time torque per unique excitation position (N
!>    @param[in] hav output time angle each response position (rad)
!>    @param[in] htq output time torque each response position (rad)
!>    @param[in] hst output stress each response position (Pa)
!>    @param[in] hta output time angle each response position division (
!>    @param[in] anrd plot output angular displacement in radian
!>    @param[in] stdio flag for standard input and output
!>    @param[in] pfn output file name, used for non standard output
!
subroutine tpeforp(ntp,pnui,np,cip,crp,mth,mxb,mxr,&
&htv,hit,hav,htq,hst,hta,anrd,stdio,pfn)
  use rd_textfun, only: pltmsgf
  use com_opt, only: std
  use rd_kinds, only: lrk
  implicit none
!
  integer :: ntp, pnui, np, mth, mxb, mxr
  real(lrk) :: htv, hit, hav, htq, hst, hta
  character(len=8) :: cip, crp
  dimension htv(mth),hit(mth,mxb),hav(mth,mxr),htq(mth,mxr),&
  &hst(mth,mxr),hta(mth,mxr),cip(mxb),crp(mxb)
  character(len=255) :: pfn
  logical :: anrd, stdio
!
  integer :: i, j, k
  real(lrk) :: mxv, scl, todegjf
  parameter (mxv = 1e16_lrk,scl = 1e6_lrk)
  character(len=4) :: c4
  character(len=5) :: nmm
  parameter(nmm = 'tpfrs',c4 = '.plt')
!     output section name
  character(len=10) :: sec
  character(len=30) :: out, pos
!     plot messages
!
!     malhap stuff
  integer :: ft, format, xachse, yachse, color
  real(lrk) :: nul, maxrig, maxtop, minbot, minlef
  real(lrk) :: xwert(ntp+2), ywert(ntp+2)
  character(len=8) :: zeit, prname
  character(len=10) :: datum
  character(len=30) :: name, abszis, ordina
  character(len=31) :: titelo, titelu
  character(len=70) :: reke
!
!     std plot output
!
  intrinsic :: len_trim
!
!     standard io
  std = stdio
!
!     malhap settings
  format = 5
  xachse = 1
  yachse = 1
  nul = 1e-12_lrk
!
!     margins
!     xmin
  minlef = 0
!     x max
  maxrig = htv(ntp)
!
!     titles
  call header(reke,zeit,datum,name,titelu)
!     time
  abszis = pltmsgf(30, 44)
!     input torque
  ordina = pltmsgf(30, 45)
!     main title
  titelo = pltmsgf(30, 43)
  prname = nmm
!     standard plot marker
  if (stdio) then
    write(sec,'(2a)') nmm,c4
    call marksec(sec,0)
  end if
!
!     line colors
!     default Pen colors: 1:Black, 2:Red, 3:Green, 4:Yellow, 5:Blue,
!     6:Magenta
!
!     black
  color = 1
!     initialize hpgl plot
  call plots(0,0,pfn)
!
!     time vector
  do i = 1,ntp
    xwert(i) = htv(i)
  end do
!
!     input time torque
  do i = 1,pnui
!       new page
    ft = 1
!       y min
    minbot = mxv
!       y max
    maxtop =-mxv
!       time vector
    do j = 1,ntp
      ywert(j) = hit(j,i)
!         min / max
      if (ywert(j) .lt. minbot) minbot = ywert(j)
      if (ywert(j) .gt. maxtop) maxtop = ywert(j)
    end do
    call malhap (ntp, format, ft, ntp, xachse, yachse,&
    &nul, maxrig, maxtop, minbot, minlef,&
    &xwert, ywert,&
    &reke, zeit, datum, abszis, ordina,&
    &name, titelo, titelu, prname,color)
!       output position text
    out =  pltmsgf(30, 46)
    k = len_trim(out)
    write(pos,'(3a)')out(1:k),' ', cip(i)
    k = len_trim(pos)
    call symbol (14._lrk,17.2_lrk,.28_lrk,pos(1:k),0,0._lrk,k)
!       position loop
  end do
!
!     output time total angular displacement at division (deg)
  if (.not. anrd) then
!       deg
    ordina = pltmsgf(30, 41)
  else
!       rad
    ordina = pltmsgf(30, 50)
  end if
!
  do i = 1,np
!       new page
    ft = 1
!       y min
    minbot = mxv
!       y max
    maxtop =-mxv
!       time vector
    do j = 1,ntp
      if (.not. anrd) then
!           convert rad to degree
        ywert(j) = todegjf(hav(j,i))
      else
!           angle in rad
        ywert(j) = hav(j,i)
      end if
!         min / max
      if (ywert(j) .lt. minbot) minbot = ywert(j)
      if (ywert(j) .gt. maxtop) maxtop = ywert(j)
    end do
    call malhap (ntp, format, ft, ntp, xachse, yachse,&
    &nul, maxrig, maxtop, minbot, minlef,&
    &xwert, ywert,&
    &reke, zeit, datum, abszis, ordina,&
    &name, titelo, titelu, prname,color)
!       output position text
    out =  pltmsgf(30, 46)
    k = len_trim(out)
    write(pos,'(3a)')out(1:k),' ', crp(i)
    k = len_trim(pos)
    call symbol (14._lrk,17.2_lrk,.28_lrk,pos(1:k),0,0._lrk,k)
!       position loop
  end do
!
!     output angular displacement at division (deg)
  if (.not. anrd) then
!       deg
    ordina = pltmsgf(30, 53)
  else
!       rad
    ordina = pltmsgf(30, 51)
  end if
!
  do i = 1,np
!       new page
    ft = 1
!       y min
    minbot = mxv
!       y max
    maxtop =-mxv
!       time vector
    do j = 1,ntp
      if (.not. anrd) then
!           convert rad to degrees
        ywert(j) = todegjf(hta(j,i))
      else
!           angle in rad
        ywert(j) = hta(j,i)
      end if
!         min / max
      if (ywert(j) .lt. minbot) minbot = ywert(j)
      if (ywert(j) .gt. maxtop) maxtop = ywert(j)
    end do
    call malhap (ntp, format, ft, ntp, xachse, yachse,&
    &nul, maxrig, maxtop, minbot, minlef,&
    &xwert, ywert,&
    &reke, zeit, datum, abszis, ordina,&
    &name, titelo, titelu, prname,color)
!       output position text
    out =  pltmsgf(30, 46)
    k = len_trim(out)
    write(pos,'(3a)')out(1:k),' ', crp(i)
    k = len_trim(pos)
    call symbol (14._lrk,17.2_lrk,.28_lrk,pos(1:k),0,0._lrk,k)
!       position loop
  end do
!
!     output time torque at division
  ordina = pltmsgf(30, 48)
!
  do i = 1,np
!       new page
    ft = 1
!       y min
    minbot = mxv
!       y max
    maxtop =-mxv
!       time vector
    do j = 1,ntp
      ywert(j) = htq(j,i)
!         min / max
      if (ywert(j) .lt. minbot) minbot = ywert(j)
      if (ywert(j) .gt. maxtop) maxtop = ywert(j)
    end do
    call malhap (ntp, format, ft, ntp, xachse, yachse,&
    &nul, maxrig, maxtop, minbot, minlef,&
    &xwert, ywert,&
    &reke, zeit, datum, abszis, ordina,&
    &name, titelo, titelu, prname,color)
!       output position text
    out =  pltmsgf(30, 46)
    k = len_trim(out)
    write(pos,'(3a)')out(1:k),' ', crp(i)
    k = len_trim(pos)
    call symbol (14._lrk,17.2_lrk,.28_lrk,pos(1:k),0,0._lrk,k)
!       position loop
  end do
!
!     output time shear stress at division
  ordina = pltmsgf(30, 56)
!
  do i = 1,np
!       new page
    ft = 1
!       y min
    minbot = mxv
!       y max
    maxtop =-mxv
!       time vector
    do j = 1,ntp
!         shear stress / scl -> MPa
      ywert(j) = hst(j,i)/scl
!         min / max
      if (ywert(j) .lt. minbot) minbot = ywert(j)
      if (ywert(j) .gt. maxtop) maxtop = ywert(j)
    end do
    call malhap (ntp, format, ft, ntp, xachse, yachse,&
    &nul, maxrig, maxtop, minbot, minlef,&
    &xwert, ywert,&
    &reke, zeit, datum, abszis, ordina,&
    &name, titelo, titelu, prname,color)
!       output position text
    out =  pltmsgf(30, 46)
    k = len_trim(out)
    write(pos,'(3a)')out(1:k),' ', crp(i)
    k = len_trim(pos)
    call symbol (14._lrk,17.2_lrk,.28_lrk,pos(1:k),0,0._lrk,k)
!       position loop
  end do
!
!     standard plot marker
  if (stdio) then
    call bflush
    call marksec(sec,1)
  else
    call plots(1,0,pfn)
  end if
!
  return
!
end subroutine tpeforp
!
!     ==================================================================
!>    @brief torsion transient numerical integration plot.
!
!>    @param[in] nte number of load indices
!>    @param[in] epi excitation position index
!>    @param[in] ndd excitations position vector
!>    @param[in] no number of output positions
!>    @param[in] imt numerical integration method
!>     0 = Newmark 1 = Wilson-theta
!>    @param[in] py output position vecor
!>    @param[in] ttrdsz size of transient time vector
!>    @param[in] ttrdtt transient load torque time vector
!>    @param[in] ttrdta transient load torque amplitude vector
!>    @param[in] iit initial integration time
!>    @param[in] fit final integration time
!>    @param[in] nrstps number of result time vector
!>    @param[in] restm transient result time matrix
!>    @param[in] resum transient result angular displacement matrix
!>    @param[in] iptld result input loads
!>    @param[in] restq division transient result torque matrix
!>    @param[in] resst division transient result shear stress matrix
!>    @param[in] resad division transient result angular displacement ma
!>    @param[in] tqm input torque multiplier vector
!>    @param[in] mxb excitation positions vector dimension
!>    @param[in] mxttra input time torque matrix columns (pos.) dimensio
!>    @param[in] mxr response position vector dimension
!>    @param[in] mxttrd input time torque matrix rows (time) dimension
!>    @param[in] mxttrs output time torque matrix rows (time) dimension
!>    @param[in] iseqt torque time equation flag
!>    @param[in] anrd plot output angular displacement in radian
!>    @param[in] stdio flag for standard input and output
!>    @param[in] pfn output file name, used for non standard output
!
subroutine tnuintp(nte,epi,ndd,no,imt,py,&
&ttrdsz,ttrdtt,ttrdta,iit,fit,&
&nrstps,restm,resum,iptld,restq,resst,resad,tqm,&
&mxb,mxttra,mxr,mxttrd,mxttrs,iseqt,anrd,stdio,pfn)
  use rd_textfun, only: ciff, pltmsgf
  use com_opt, only: std
  use rd_kinds, only: lrk
  implicit none
!
  integer :: nte, epi, no, imt, ttrdsz, nrstps, mxb, mxttra, mxr, mxttrd, mxttrs
  real(lrk) :: ndd, py, ttrdtt, ttrdta, iit, fit, restm, resum, iptld, restq, resst, resad, tqm
  dimension epi(mxttra),ndd(mxb),py(mxr),&
  &ttrdtt(mxttrd),ttrdta(mxttra,mxttrd),&
  &restm(mxttrs),resum(mxttrs,mxr),&
  &iptld(mxttrs,mxr),restq(mxttrs,mxr),resst(mxttrs,mxr),&
  &resad(mxttrs,mxr),tqm(mxttra)
  character(len=255) :: pfn
  logical :: iseqt, anrd, stdio
!
  integer :: i, j, k
  real(lrk) :: mxv, scl, todegjf
  parameter (mxv = 1e12_lrk,scl = 1e6_lrk)
  character(len=1) :: blank
  character(len=4) :: cfn
  character(len=5) :: nmm
!     output section name
  character(len=10) :: sec
  character(len=12) :: cit, cct
  character(len=30) :: out
!     plot messages
  dimension cit(2)
!     (t)ransient (n)umerical (i)ntegration (r)esponse
  parameter(blank = ' ',cfn = '.plt',nmm = 'ttnir',&
  &cit = (/'Newmark     ','Wilson-theta'/))
!
!     malhap stuff
  integer :: ft, format, xachse, yachse, color
  real(lrk) :: nul, maxrig, maxtop, minbot, minlef
  real(lrk) :: xwert(nrstps+2), ywert(nrstps+2)
  character(len=8) :: zeit, prname
  character(len=10) :: datum
  character(len=30) :: name, abszis, ordina
  character(len=31) :: titelo, titelu
  character(len=70) :: reke
!
!     std plot output
!
  intrinsic :: len_trim
!
!     standard io
  std = stdio
!
!     malhap settings
  format = 5
  xachse = 1
  yachse = 1
  nul = 1e-12_lrk
!
!     margins
!     xmin
  minlef = iit
!     x max
  maxrig = fit
!
!     titles
  call header(reke,zeit,datum,name,titelu)
!     time
  abszis = pltmsgf(30, 44)
!     input torque
  ordina = pltmsgf(30, 45)
!     transient
  titelo = pltmsgf(30, 47)
  prname = nmm
!
!     integration kind
!     0 = Newmark 1 = Wilson-theta
  cct = ciff(12, imt .eq. 0,cit(1),cit(2))
!
!     standard plot marker
  if (stdio) then
    write(sec,15) nmm,cfn
    call marksec(sec,0)
  end if
!
!     line colors
!     default Pen colors: 1:Black, 2:Red, 3:Green, 4:Yellow, 5:Blue,
!     6:Magenta
!
!     black
  color = 1
!     initialize hpgl plot
  call plots(0,0,pfn)
!
!     input
  do i = 1,nte
!       new page
    ft = 1
!       y min
    minbot = mxv
!       y max
    maxtop =-mxv
    do j = 1,nrstps
!         time vector
      if (i .eq. 1) xwert(j) = restm(j)
      ywert(j) = iptld(j,i)
!         min / max
      if (ywert(j) .lt. minbot) minbot = ywert(j)
      if (ywert(j) .gt. maxtop) maxtop = ywert(j)
    end do
    call malhap (nrstps, format, ft, nrstps, xachse, yachse,&
    &nul, maxrig, maxtop, minbot, minlef,&
    &xwert, ywert,&
    &reke, zeit, datum, abszis, ordina,&
    &name, titelo, titelu, prname,color)
!       input position text
    out =  pltmsgf(30, 46)
    k = len_trim(out)
    write(out,5)out(1:k),blank, ndd(epi(i))
    k = len_trim(out)
    call symbol (14._lrk,17.2_lrk,.28_lrk,out(1:k),0,0._lrk,k)
!
!       check for time equation
    if (.not. iseqt) then
!         input data
      do j = 1,ttrdsz
        call mark(ttrdtt(j),ttrdta(i,j)*tqm(i),60._lrk,4)
      end do
!         input data
      out =  pltmsgf(30, 49)
      j = len_trim(out)
      call legend(1._lrk,17.2_lrk,4,0.4_lrk,color,j,out)
    end if
!
  end do
!
!     total angular displacement
  if (.not. anrd) then
!       degree
    ordina = pltmsgf(30, 41)
  else
!       radian
    ordina = pltmsgf(30, 50)
  end if
!
!     output total displacement angle
  do i = 1,no
!       new page
    ft = 1
!       output
!       y min
    minbot = mxv
!       y max
    maxtop =-mxv
    do j = 1,nrstps
!         time vector
      if (i .eq. 1) xwert(j) = restm(j)
!         result in degrees or radian
      if (.not. anrd) then
!           convert rad to deg
        ywert(j) = todegjf(resum(j,i))
      else
!           radian
        ywert(j) = resum(j,i)
      end if
!         min / max
      if (ywert(j) .lt. minbot) minbot = ywert(j)
      if (ywert(j) .gt. maxtop) maxtop = ywert(j)
    end do
    call malhap (nrstps, format, ft, nrstps, xachse, yachse,&
    &nul, maxrig, maxtop, minbot, minlef,&
    &xwert, ywert,&
    &reke, zeit, datum, abszis, ordina,&
    &name, titelo, titelu, prname,color)
!       input position text
    out = pltmsgf(30, 46)
    k = len_trim(out)
    write(out,5)out(1:k),blank, py(i)
    k = len_trim(out)
    call symbol (14._lrk,17.2_lrk,.28_lrk,out(1:k),0,0._lrk,k)
!       integration kind
    out = pltmsgf(30, 57)
    k = len_trim(out)
    j = len_trim(cct)
    write(out,15)out(1:k),cct(1:j)
    k = len_trim(out)
    call symbol (0.5_lrk,17.2_lrk,.28_lrk,out(1:k),0,0._lrk,k)
  end do
!
!     division angular displacement
!     angle deg/rad
  if (.not. anrd) then
!       degree
    ordina = pltmsgf(30, 53)
  else
!       radian
    ordina = pltmsgf(30, 51)
  end if
!
!     output division angular displacement
  do i = 1,no
!       new page
    ft = 1
!       output
!       y min
    minbot = mxv
!       y max
    maxtop =-mxv
    do j = 1,nrstps
!         time vector
      if (i .eq. 1) xwert(j) = restm(j)
      if (.not. anrd) then
!           convert rad to deg
        ywert(j) = todegjf(resad(j,i))
      else
!           radian
        ywert(j) = resad(j,i)
      end if
!!         min / max
      if (ywert(j) .lt. minbot) minbot = ywert(j)
      if (ywert(j) .gt. maxtop) maxtop = ywert(j)
    end do
    call malhap (nrstps, format, ft, nrstps, xachse, yachse,&
    &nul, maxrig, maxtop, minbot, minlef,&
    &xwert, ywert,&
    &reke, zeit, datum, abszis, ordina,&
    &name, titelo, titelu, prname,color)
!       input position text
    out =  pltmsgf(30, 46)
    k = len_trim(out)
    write(out,5)out(1:k),blank, py(i)
    k = len_trim(out)
    call symbol (14._lrk,17.2_lrk,.28_lrk,out(1:k),0,0._lrk,k)
!       integration kind
    out = pltmsgf(30, 57)
    k = len_trim(out)
    j = len_trim(cct)
    write(out,15)out(1:k),cct(1:j)
    k = len_trim(out)
    call symbol (0.5_lrk,17.2_lrk,.28_lrk,out(1:k),0,0._lrk,k)
!
  end do
!
!     division torque
  ordina = pltmsgf(30, 48)
!
!     output division torque
  do i = 1,no
!       new page
    ft = 1
!       output
!       y min
    minbot = mxv
!       y max
    maxtop =-mxv
    do j = 1,nrstps
!         time vector
      if (i .eq. 1) xwert(j) = restm(j)
      ywert(j) = restq(j,i)
!         min / max
      if (ywert(j) .lt. minbot) minbot = ywert(j)
      if (ywert(j) .gt. maxtop) maxtop = ywert(j)
    end do
    call malhap (nrstps, format, ft, nrstps, xachse, yachse,&
    &nul, maxrig, maxtop, minbot, minlef,&
    &xwert, ywert,&
    &reke, zeit, datum, abszis, ordina,&
    &name, titelo, titelu, prname,color)
!       input position text
    out =  pltmsgf(30, 46)
    k = len_trim(out)
    write(out,5)out(1:k),blank,py(i)
    k = len_trim(out)
    call symbol (14._lrk,17.2_lrk,.28_lrk,out(1:k),0,0._lrk,k)
!       integration kind
    out = pltmsgf(30, 57)
    k = len_trim(out)
    j = len_trim(cct)
    write(out,15)out(1:k),cct(1:j)
    k = len_trim(out)
    call symbol (0.5_lrk,17.2_lrk,.28_lrk,out(1:k),0,0._lrk,k)
!
  end do
!
!     division shear stress
  ordina = pltmsgf(30, 56)
!
!     output division torque
  do i = 1,no
!       new page
    ft = 1
!       output
!       y min
    minbot = mxv
!       y max
    maxtop =-mxv
    do j = 1,nrstps
!         time vector
      if (i .eq. 1) xwert(j) = restm(j)
!         shear stress / scl -> MPa
      ywert(j) = resst(j,i)/scl
!         min / max
      if (ywert(j) .lt. minbot) minbot = ywert(j)
      if (ywert(j) .gt. maxtop) maxtop = ywert(j)
    end do
    call malhap (nrstps, format, ft, nrstps, xachse, yachse,&
    &nul, maxrig, maxtop, minbot, minlef,&
    &xwert, ywert,&
    &reke, zeit, datum, abszis, ordina,&
    &name, titelo, titelu, prname,color)
!       input position text
    out =  pltmsgf(30, 46)
    k = len_trim(out)
    write(out,5)out(1:k),blank, py(i)
    k = len_trim(out)
    call symbol (14._lrk,17.2_lrk,.28_lrk,out(1:k),0,0._lrk,k)
!       integration kind
    out = pltmsgf(30, 57)
    k = len_trim(out)
    j = len_trim(cct)
    write(out,15)out(1:k),cct(1:j)
    k = len_trim(out)
    call symbol (0.5_lrk,17.2_lrk,.28_lrk,out(1:k),0,0._lrk,k)
!
  end do
!
!     standard plot marker
  if (stdio) then
    call bflush
    call marksec(sec,1)
  else
    call plots(1,0,pfn)
  end if
!
  return
!
5 format(2a,f8.4)
15 format(2a)
!
end subroutine tnuintp
!
