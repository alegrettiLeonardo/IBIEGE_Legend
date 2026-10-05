!     $Id$
!     ==================================================================
!
!>    @file saidas.f
!>    @author francisco
!>    @date 28-may-15
!>    @brief program outputs, last changes:<br>
!>    added internal diameter coordinates - francisco - 03/12/2008<br>
!>    added undamped critical speed option - francisco - 08/06/2009<br>
!>    changed rtinfo, added bsf - francisco - 17/11/2009<br>
!>    changed outsup, support parametero vector - francisco - 05/03/2010
!>    added center of mass output - francisco - 11/10/2010<br>
!>    added desp in s_resp_f - francisco - 11/10/2010<br>
!>    added param st in s_resp_f - francisco - 16/06/2011<br>
!>    added ori block /unb/, column orient - francisco - 29/06/2011<br>
!>    added standard i/o capabilities - francisco - 14/02/2012<br>
!>    added marksec output section marks, for stdio - francisco - 20/02/
!>    changed emsg moved from saidas.f - francisco - 20/02/2012<br>
!>    changed emsg output to a buffer - francisco - 20/02/2012<br>
!>    added sprint, screen messages - francisco 20/02/2012<br>
!>    added tpf (kind of unbalance) on saidas - franciso 20/02/2014<br>
!>    changed loop limit s_resp_t from k-1 to k - francisco 29/09/2014<b
!>    added output of module of orbit - francisco 01/10/2014<br>
!>    added export names vector - francisco 18/11/2014<br>
!>    added modal matrices export  - francisco 19/11/2014<br>
!>    added system speed on critical speed map - francisco 25/05/2015<br
!>    changed mntfnm, added plot names for knd lt 0 - francisco 28/05/20
!>    added hpgl plots - francisco 28/05/2015<br>
!>    changed export error messages variable name was emsg - francisco -
!>    etime was moved from matfun.f - francisco - jul-15<br>
!>    added center of mass on gemetory export - francisco - jul-15<br>
!>    changed number of eigenvalues to same of campbell in s_campbl - fr
!>    added bearing rotational stiffness parameter - francisco - oct-15<
!>    added sok on saidas, support data on main input file - francisco -
!>    added af check on s_resp_f auto. resp. analysis - francisco - oct-
!>    added bearing parameter scale sc - francisco - oct-15<br>
!>    added support parameter scale sps - francisco - nov-15<br>
!>    added angular critical speed map support - francisco - nov-17<br>
!>    changed format and add line break on emsg francisco feb-19
!>    added disk offset suport - francisco - feb-19<br>
!>    added output angle and g in saidas - francisco - feb-19<br>
!>    added calculation summary in saidas - francisco - feb-19<br>
!>    added variable speed bearing parameters plot - francisco - feb-19<
!>    added central messages, functions - francisco - mar-19<br>
!>    added label formats, on repeating outputs - francisco - apr-19<br>
!>    changed sbeapar, add std flag on plot subroutine - francisco - apr
!>    removed ori angle guess s_resp_f, always in radians - francisco -
!>    added convenience subroutines elmsge,elmsgw and elmsga - francisco
!>    added user and description text output sub calls outdsc - francisc
!>    added diamond geometry for static torque on geometry<br>
!>    moved source to export geometry to expgeo.f - francisco - jan-20<b
!>    added torsion option on saidas.f  - francisco - mar-20<br>
!>    changed mxb = 99 - francisco - apr-20<br>
!>    changed parameter todo to wtd - francisco apr-20<br>
!>    added message id on scexems - franciso - may-20<br>
!>    added message dumping s_dumo - francisco - may-20<br>
!>    added angle unit handling in saida - francisco - sep-20<br>
!>    added bearing reaction, displacement and summ on elastic output -
!>    added brprint for screen feedback with backspace - francisco - oct
!>    added bending moment and shear force output at s_lin_el - francisc
!>    added isf on section common, bend stress median filter window leng
!>    message functions moved to message.f - francisco - jan-21<br>
!>    added arg frl on s_resp_f - flag for log plot on unbalace on s_res
!>    added nmd arg. on s_resp_t - number of modes - francisco - feb-21<
!>    added better control on find angle and discard speed if angle not
!>    added nit number of crossings on s_campbl - francisco mar-21<br>
!>    added setable crossing lines on Campbell - francisco - apr-21<br>
!>    added text output for bearing parameters - francisco - may-21<br>
!>    added flexural-torsion optional data flag on saidas - francisco ju
!>    updated s_resp_t support output - francisco aug-21<br>
!>    updated s_resp_f support output - francisco aug-21<br>
!>    added pgycscidf, output index-gyroscopic factor -  francisco aug-2
!>    updated s_maprig bearing stiffness output - francisco - sep-21<br>
!>    updated sbeapar, add param bearing check - francisco - sep-21<br>
!>    updated s_campbl, added extrapolation message - franciso - sep-21<
!>    updated call name from umpforf to disforf - francisco - sep-21<br>
!>    bugfix s_respt_t, max detection indices - francisco - sep-21<br>
!>    bugfix bprint added format detection - francisco - sep-21<br>
!>    added iex export kind mode on export and saidas, job = 2 - francis
!>    updated export file names to handle plain type - francisco - nov-2
!>    added torsion element kind on saidas and s_summ - francisco - nov-
!>    added distributed force direction output on s_lin_el - francisco -
!>    added fxstf fixed stiffnes for no displacement on saidas - francis
!>    updated sbeapar, changed parameter column caption to same as plot
!
!     ==================================================================
!>    @brief prepare the output file name.
!
!>    @param[in] knd output file id, lt 0 for plot
!>    \verbatim
!>    1 = campbell, 2 = frequency, 3 = time, 4 = basic output,
!>    5 = log, 6=coordinates, 7 = elements, 8 = static elastic line,
!>    9 = undamped critical speed map, 10 = bearing suport
!>    > 10 and < 17 = matrix export PHIr,PSIr,LAMr,PHIi,PSIi,LAMi
!>    17 = undamped angular critical speed map
!>    18 = bearing variable speed parameters output plot file default su
!>    19 = element id, see blockd.f, 20 = torsion harmonic response
!>    21 = torsion harmonic response, 22 = dump messages
!>    23 = flexural-torsion output
!>    \endverbatim
!>    @param[out] lng returns the file name length without spaces
!>    @param[out] ofn returns the output file name.
!>    @see init.f, tsaidas.f
!
subroutine mntfnm(knd,lng,ofn)
  use rd_textfun, only: ciff, femsgf
  use com_fbn, only: bsn
  use com_fnm, only: onm, mtx
  use com_pst, only: wrl
  implicit none
!
!     arguments
  integer :: knd, lng
  character(len=255) :: ofn
!
!     locals
  integer :: i, j, k, akn
  character(len=1) :: blank
  character(len=3) :: c3
  character(len=4) :: c4, ext
  character(len=6) :: nmm
  dimension c4(3)
  parameter (blank = ' ',c3 = '_dm',&
  &c4 = (/'rdin','.plt','.txt'/),nmm = 'mntfnm')
!     error messages
  character(len=99) :: msg
  character(len=128) :: nma, bas
!
!     base name
!
!     i/o folder - work folder
!
!     outputs modes, campbell, frequency, time (init.f)
  integer :: onmx
  parameter (onmx = 17)
!
!     intrinsic functions
  intrinsic :: abs, len_trim
!
!     init
!     append type
  nma = blank
!     base name, from input file name
  bas = bsn
!
!     added - francisco feb-19
  akn = abs(knd)
!     -0 workaround = -99
  if (akn .eq. 0 .or. knd .eq. -99) then
!       modes
    nma = onm(1)
  else if (akn .gt. 0 .and. akn .lt. 10) then
!       1=campbell,2=frequency,3=time,4=basic output,5=log, 6=coordinate
!       7=elements,8=static elastic line,9=undamped critical speed map
    nma = onm(akn+1)
  else if (akn .eq. 10) then
!       bearing suport
    nma = onm(12)
  else if (knd .gt. 10 .and. knd .lt. 17) then
!       matrix export PHIr,PSIr,LAMr,PHIi,PSIi,LAMi
    nma = mtx(knd-10)
  else if (akn .eq. 17) then
!       undamped angular critical speed map
    nma = onm(11)
  else if (akn .eq. 18) then
!       bearing variable speed parameters output plot file default sufix
    nma = onm(13)
  else if (knd .eq. 19) then
!       element id, see blockd.f
    nma = onm(14)
  else if (akn .eq. 20) then
!       torsion harmonic response
    nma = onm(15)
  else if (akn .eq. 21) then
!       torsion harmonic response
    nma = onm(16)
  else if (akn .eq. 23) then
!       flexural-torsion output
    nma = onm(17)
  else if (knd .eq. 22) then
!       dump messages
    nma = c3
    bas = c4(1)
  else
    msg = femsgf(99, nmm,6,13,0)
    call emsg(0,1,msg)
  end if
!     check for name
!     file name length space striped
  j = len_trim(nma)
  if (j .eq. 0) then
    msg = femsgf(99, nmm,6,14,0)
    call emsg(0,1,msg)
  end if
!
!     file extension
  ext = ciff(4, knd .lt. 0,c4(2),c4(3))
!
!     complete file name
!     work folder string length space striped
  i = len_trim(wrl)
!     base name string length space striped
  k = len_trim(bas)
  write(ofn,5)wrl(1:i),bas(1:k),nma(1:j),ext
!
!     output file name length space striped
  lng = len_trim(ofn)
!
  return
!
5 format(4a)
!
end subroutine mntfnm

!     ==================================================================
!>    @brief output progress value to screen terminal.
!>     will back on screen using backspace.
!
!>    @param[in] rv real value, iv is zero
!>    @param[in] iv integer value rv is zero
!>    @param[in] fm output format to use
!>    @param[in] is size to back
!
subroutine bprint(rv,iv,fm,is)
  use rd_kinds, only: lrk
  implicit none
!
  real(lrk) :: rv
  integer :: iv, is
  character(len=*) :: fm
!
  real(lrk) :: prec
  logical :: isreal, isint, isboth
  integer :: ii, jj
  character(len=1) :: bs, ci, blank
  character(len=5) :: cf
  character(len=50) :: tx
  dimension ci(6)
  parameter (prec = 1e-18_lrk,bs = char(8),blank = ' ',ci = (/'i','I','f','F','e','E'/),cf = '(a,$)')
!
  intrinsic :: abs, char, index, len_trim

!     init output with blank
  do ii = 1,is
    tx(ii:ii) = blank
  end do
  ii = len_trim(fm)
!     format detection
  isint = .false.
  isreal = .false.
  if (ii .gt. 0) then
    isint = index(fm,ci(1)) .gt. 0&
    &.or. index(fm,ci(2)) .gt. 0
    isreal = index(fm,ci(3)) .gt. 0&
    &.or. index(fm,ci(4)) .gt. 0&
    &.or. index(fm,ci(5)) .gt. 0&
    &.or. index(fm,ci(6)) .gt. 0
  end if
  isboth = isint .and. isreal
!     check for integer
  if ((abs(iv) .gt. prec .and. abs(rv) .lt. prec&
  &.and. .not. isboth)&
  &.or. (isint .and. .not. isreal)) then
!       integer passed
    write(tx,fm(1:ii)) iv,(bs,jj=1,is)
  else if((abs(rv) .gt. prec .and. abs(iv) .lt. prec&
  &.and. .not. isboth)&
  &.or. (isreal .and. .not. isint)) then
!       real passed
    write(tx,fm(1:ii)) rv,(bs,jj=1,is)
  else if((abs(rv) .gt. prec .and. abs(iv) .gt. prec)&
  &.or. isboth) then
!       both passed
    write(tx,fm(1:ii)) rv,iv,(bs,jj=1,is)
  end if
  call sprint(.false.,tx,cf)
!
  return
!
end subroutine bprint
!
!     ==================================================================
!>    @brief output screen terminal messages.
!
!>    @param[in] std standard input output
!>    @param[in] txt message string
!>    @param[in] fmt output format to use
!
subroutine sprint(std,txt,fmt)
  implicit none
!
!     arguments
  character(len=*) :: fmt, txt
  logical :: std
!
!     locals
  character(len=1) :: blank
  parameter(blank = ' ')
  integer :: i, j
!
!     intrinsic
  intrinsic :: len_trim
!
!     not std i/o
  if (.not. std) then
    i = len_trim(txt)
    if (fmt(1:1) .gt. blank) then
      j = len_trim(fmt)
      print fmt(1:j),txt(1:i)
    else
      print *,txt(1:i)
    end if
  end if
!
  return
!
end subroutine sprint
!
!     ==================================================================
!>    @brief screen executable messages
!
!>    @param[in] lm message ignored if id >0
!>    @param[in] std standard output flag
!>    @param[in] lgn log flag
!>    @param[in] kd kind 0 wait, 1 ok, lt 0 ignore saved message
!>    @param[in] id screen message id, zero will use lm
!>    @see messages.f
!
subroutine scexems(lm,std,lgn,kd,id)
  use rd_textfun, only: scmsgf
  implicit none
!
  character(len=*) :: lm
  logical :: std
  integer :: lgn, kd, id
!
  integer :: i, mn
  character(len=1) :: blank, dl
  character(len=5) :: mf1
  character(len=7) :: lm1
  character(len=9) :: lm2
!     screen messages by id, see messages.f
  character(len=15) :: llm
  character(len=99) :: msg
  dimension mn(2)
  parameter(blank = ' ',dl = '$',mf1 ='(a,$)',&
  &lm1 = ' -> ok!',mn = (/10,15/))
  data lm2/' '/
  save lm2
!
  intrinsic :: len, min
!
  if (kd .eq. 0) then
    if (id .gt. 0) then
!         screen message, see messages
      llm = scmsgf(10, id)
    else
!         length = 15
      i = min(len(lm),mn(2))
      llm = lm(1:i)
    end if
    call sprint(std,llm,mf1)
!       save for subsequent call with kd=1, length = 9
    lm2 = llm(1:mn(1)-1)
  else if (kd .ne. 0) then
    if (kd .lt. 0) then
!         ignore saved message, length = 9
      lm2 = lm(1:mn(1)-1)
!         message + ok, length = 11
      i = min(len(lm),mn(1)+1)
      write(msg,5) lm(2:i),lm1
      call sprint(std,msg,blank)
    else
!         just ok, was previously saved
      call sprint(std,lm1,blank)
    end if
!       log
    write(msg,15) dl,lm2,lm1
    call emsg(lgn,0,msg)
  end if
!
  return
!
5 format(2a)
15 format(3a)
!
end subroutine scexems
!
!     ==================================================================
!>    @brief application end messages
!
!>    @param[in] std standard output flag
!>    @param[in] lgn log flag
!
subroutine sfinms(std,lgn)
  use rd_textfun, only: xtime
  implicit none
!
  integer :: lgn
  logical :: std
!
  integer :: ok
  character(len=1) :: cdl
  character(len=2) :: csg
  character(len=3) :: cmf
  character(len=6) :: crt
  character(len=9) :: ela
  character(len=11) :: fin
!     messages
  character(len=40) :: clm
  character(len=99) :: msg
  parameter (cdl = '$',csg = ' s',cmf = '(a)',&
  &ela = ' elapsed ', fin = ' finish  : ')
!
!     time consumption
  crt = xtime(6)
  call petim(crt,std,msg,ok)
  if (ok .lt. 0) call emsg(lgn,1,msg)
!     screen
  write(clm,5) ela,crt,csg
  call sprint(std,clm,cmf)
!     log
  write(msg,15) cdl,clm(2:)
  call emsg(lgn,0,msg)
!
  clm = fin
  call scexems(clm,std,lgn,-1,0)
!
  return
!
5 format(3a)
15 format(2a)
!
end subroutine sfinms
!
!     ==================================================================
!>    @brief stdio section marker, only for standard output 6.
!
!>    @param[in] sec sction name
!>    @param[in] kd 0 begin section, 1 end section
!
subroutine marksec(sec,kd)
  use rd_textfun, only: fomsgf
  implicit none
!
!     arguments
  character(len=*) :: sec
  integer :: kd
!
!     locals
  integer :: i, k, l, m, n
  character(len=1) :: dot, sep, add
  character(len=6) :: c6
  character(len=7) :: nmm
  character(len=10) :: mrk1, mrk2, out
  character(len=72) :: lsp
  parameter (dot = '.', sep = '=',add = ' ',c6 = 'stdout',&
  &nmm = 'marksec',mrk1 = '#BEGIN',mrk2 = '#END',out = '.out',&
  &lsp = repeat(sep,72))
  character(len=99) :: msg
!
!     intrinsic
  intrinsic :: len_trim, index, repeat
!
!     section size space striped
  i = len_trim(sec)
  k = len_trim(mrk1)
!     check for point in the section name
  n = index(sec,dot)
!
  if (kd .eq. 0) then
!       begin
    write(6,5,err=200) lsp
!       section has suffix "."
    if(n .gt. 0) then
      write(6,100,err=200) mrk1(1:k),add,sec(1:i)
    else
      m = len_trim(out)
      write(6,150,err=200) mrk1(1:k),add,sec(1:i),out(1:m)
    end if
  else
!       end
    l = len_trim(mrk2)
    if(n .gt. 0) then
      write(6,100,err=200) mrk2(1:l),add,sec(1:i)
    else
      m = len_trim(out)
      write(6,150,err=200) mrk2(1:l),add,sec(1:i),out(1:m)
    end if
  end if
!
  return
!
200 msg = fomsgf(99, nmm,13,c6,1)
!     unrecoverable, stop
  call lmsg(1,msg)
!
5 format(a)
100 format(3a)
150 format(4a)
!
end subroutine marksec
!
!     ==================================================================
!>    @brief output separators.
!
!>    @param[in] iu file handle number
!>    @param[in] iop operation 0=begin other=end
!>    @param[in] sec section name. need only on begin, on end will
!>     use last if blank.
!
subroutine marksep(iu,iop,sec)
  use rd_textfun, only: fomsgf
  implicit none
!
  integer :: iu, iop
  character(len=*) :: sec
!
  integer :: i
  character(len=9) :: end
  character(len=11) :: beg
  parameter(beg = '===> BEGIN ',end='<=== END ')
  character(len=7) :: nmm
  parameter(nmm = 'marksep')
  character(len=80) :: ssec
  data ssec/' '/
  save ssec
  character(len=99) :: msg
  character(len=255) :: fn
!
  intrinsic :: len_trim
!
  i = len_trim(sec)
  if (iop .ne. 0) then
    if (i .gt. 0) then
      write(iu,300,err=200)end,sec(1:i)
    else
      i = len_trim(ssec)
      write(iu,300,err=200)end,ssec(1:i)
    end if
  else
    ssec = sec
    write(iu,300,err=200)beg,sec(1:i)
  end if
!
  return
!
200 inquire(unit=iu,name=fn)
!     unrecoverable, stop
  msg =  fomsgf(99, nmm,13,fn,1)
  call lmsg(1,msg)
!
300 format(2a)
!
end subroutine marksep
!
!     ==================================================================
!>    @brief output log data.
!
!>    @param[in] iu output device handle number
!>    @param[in] errmsg return an error message
!>    @param[in] ok return flag. unsuccessful if <0.
!
subroutine oplg(iu,errmsg,ok)
  use rd_textfun, only: fomsgf
  use com_msb, only: lid, msl
  use com_sta, only: stm
  implicit none
!
!     arguments
  integer :: iu, ok
  character(len=99) :: errmsg
!
!     locals
  logical :: std
  integer :: i, j, k
  character(len=4) :: nmm
  character(len=5) :: cst
  character(len=10) :: sec
  parameter(nmm = 'oplg',sec = 'msglog',cst = 'stdio')
!
!     error messages
  character(len=255) :: ofn
!
!     stamp
!
!     log
!
!     intrinsic
  intrinsic :: len_trim
!
!     return init
  ok = -1
!
!     std i/o
  std = iu .eq. 6
!     not std i/o
  if (.not. std) then
!       get the log file name -> ofn
    call mntfnm(5,i,ofn)
    open(iu,file=ofn(1:i),err=100)
  else
    ofn = cst
    i = len_trim(ofn)
    call marksec(sec,0)
  end if
!
!     write the stamp
  write(iu,300,err=200) stm
!
!     loop log array
  do j = 1,lid-1
    k = len_trim(msl(j))
    write(iu,300,err=200) msl(j)(1:k)
  end do
!
!     section mark
  if (std) then
    call marksec(sec,1)
  else
!       close output
    close (iu)
  endif
!
!     ok return
  ok = 0
  return
!
!     file open error
100 errmsg = fomsgf(99, nmm,1,ofn,1)
  return
!
!     close even std i/o
200 close(iu)
  errmsg = fomsgf(99, nmm,13,ofn,1)
  return
!
300 format(a)
!
end subroutine oplg
!
!     ==================================================================
!>    @brief File not empty text output.
!
!>    @param[in] iu file handle number
!>    @param[in] tout text to output

subroutine fltout(iu,tout)
  use rd_textfun, only: fomsgf
  implicit none
!
  integer :: iu
  character(len=*) :: tout
!
  integer :: i
  character(len=6) :: nmm
  parameter (nmm = 'fltout')
  character(len=99) :: msg
  character(len=255) :: fn
!
  intrinsic :: len_trim
!
  i = len_trim(tout)
  write(iu,5,err=200) tout(1:i)
!
  return
!
5 format(a)
200 inquire(unit=iu,name=fn)
  msg = fomsgf(99, nmm,13,fn,1)
!     unrecoverable, stop
  call lmsg(1,msg)
!
end subroutine fltout
!
!     ==================================================================
!>    @brief Campbell output.
!
!>    @param[in] ncf number of calculated speeds
!>    @param[in] ncc number of critical speed
!>    @param[in] npi number of speed intervals (npi - 1)
!>    @param[in] ncs number of critical speeds 1x,2x and 1/2x
!>    @param[in] nit number of crossing lines 1, 2, 0.5 x
!>    @param[in] gi critical frequencies by speed matrix
!>    @param[in] rcr intersections vector 1x, 2x, 0.5x
!>    @param[in] fcr critical frequencies
!>    @param[in] dmp crossing rpm times pu vector (1,2,0.5)
!>    @param[in] av complex eigenvalues matrix j x dim
!>    @param[in] omg speed vector j x 1 (rad)
!>    @param[in] rpmi initial calculation speed (rpm)
!>    @param[in] rpmf final calculation speed (rpm)
!>    @param[in] spdn rated speed (rpm)
!>    @param[in] nini initial speed (rpm)
!>    @param[in] ndw speed delta (rad/s)
!>    @param[in] mtg critical freq (gi) and speed (omg) dimension
!>    @param[in] mxc rcr(mxm,mxc),fcr(mxm,mxc) dimension
!>    @param[in] mxm rcr(mxm,mxc), fcr(mxm,mxc), ncs(mxm),dmp(mxm)
!>     dimension
!>    @param[in] std standard io flag
!>    @param[in] plt HPGL output
!>    @param[out] errmsg return an error message
!>    @param[out] ok return flag. unsuccessful if <0.
!
subroutine s_campbl(ncf,ncc,npi,ncs,nit,gi,rcr,fcr,dmp,&
&av,omg,rpmi,rpmf,spdn,nini,ndw,mtg,mxc,mxm,&
&std,plt,errmsg,ok)
  use rd_textfun, only: fomsgf, txtmsgf
  use com_sta, only: stm
  use rd_kinds, only: lrk, wp
  use rd_modal_metrics, only: modal_metrics_t,prescribed_metrics
  implicit none
  type(modal_metrics_t) :: audit_metric
!
!     locals
  integer :: k, l, m, n, iu, oki
  real(lrk) :: xv, pi, ldm, prec, rpm2radf, rad2rpmf, rad2hzf, rpif
  real(wp) :: dd, ld, wd, ldplot
  character(len=1) :: cop
  character(len=2) :: c2
  character(len=3) :: cf7
  character(len=5) :: cst
  character(len=6) :: cf1
  character(len=7) :: cf2
  character(len=8) :: nmm, cf6
  character(len=9) :: cf3
  character(len=10) :: sec
  character(len=11) :: cf5
  character(len=12) :: cf8
  character(len=13) :: cf4
  character(len=50) :: txt, ft
  character(len=255) :: ofn
  dimension c2(2), cop(4), txt(2), cf7(2)
!     ldm -> max log dec
  parameter (ldm = 20,prec = 1e-12_lrk,cop = (/'=','(',')','x'/),cst = 'stdio',c2 = (/' (','a,'/),cf1 = 'E18.10', &
    & cf2='E18.10,',cf3 = '(E18.10))',cf4 = '(a,2E18.10,a)',cf5='a18,a54,1x,',cf6='(i2,16x)',cf7 = (/'2x,','max'/), &
    & cf8 = '(f8.4,a,9x)',nmm = 's_campbl',sec = 'campbell')
!
!     arguments
  integer :: ncf, ncc, npi, ncs, nit, mtg, mxc, mxm, ok
  dimension ld(ncf,ncc),ldplot(ncf,ncc)
  real(lrk) :: rpmi, rpmf, spdn, nini, ndw, gi, omg, rcr, fcr, dmp
  complex(wp) :: av
  dimension gi(mtg,mtg),omg(mtg),av(mtg,mtg),&
  &rcr(mxm,mxc),fcr(mxm,mxc),ncs(mxm),dmp(mxm)
!
  character(len=99) :: errmsg
  logical :: std, plt, safe_plot
!
!     stamp
!
  intrinsic :: abs, aimag, len_trim, real
!
!     return init
  ok = -1
  pi = rpif()
  safe_plot=.true.
!     not std i/o
  if (.not. std) then
!       output file
    iu = 12
    call mntfnm(1,m,ofn)
    open(iu,file=ofn(1:m),err=100)
  else
    ofn = cst
    iu = 6
    call marksec(sec,0)
  end if
!
  write(iu,25,err=200) stm
  call outdsc(iu)
  write(iu,'(a)') 'EIGENVALUE_CONVENTION A_NEGATES_PHYSICAL_S'
  write(iu,'(a)') 'FREQUENCY_KIND DAMPED_IMAGINARY_PART'
  write(iu,'(a,*(1x,es24.16e3))') 'EXCITATION_ORDERS',dmp(1:nit)
  write(iu,'(a)') 'CROSSING_KIND CAMPBELL_INTERSECTION_NOT_BODE_PEAK'
  call fltout(iu,txtmsgf(50, 1))
!     eventual speed extrapolation message, see parmanv.f
  call eplwrg(-1,txt(1))
  l = len_trim(txt(1))
  if (l .gt. 0) write(iu,25,err=200) txt(1)(1:l)
!     calculation range
  txt(1) = txtmsgf(50, 5)
  l = len_trim(txt(1))
  write(iu,35,err=200) txt(1)(1:l),rpmi,rpmf
!     rated speed rpm
  txt(1) = txtmsgf(50, 152)
  l = len_trim(txt(1))
  write(iu,45,err=200) txt(1)(1:l),spdn
!
  write(iu,5,err=200)
!
!     critical speeds
  call fltout(iu,txtmsgf(50, 2))
!     prep format
  write(ft,300) cf7(1),nit,cf8
  write(iu,ft,err=200) (dmp(k),cop(4),k = 1,nit)
!
!     maximum number of critical speeds
  m = 0
  do l = 1,nit
    if (ncs(l) .gt. m) m = ncs(l)
  end do
!
!     prep format
  write(ft,65) cop(2),nit,cf3
  do l = 1,m
!       conv rad/s -> rpm
    write(iu,ft,err=200) (rad2rpmf(rcr(k,l)),k = 1,nit)
  end do
  write(iu,5,err=200)
!
!     critical frequecies
!     prep format
  write(ft,300) cf7(1),nit,cf8
  call fltout(iu,txtmsgf(50, 3))
  write(iu,ft,err=200) (dmp(k),cop(4),k = 1,nit)
!     prep format
  write(ft,65) cop(2),nit,cf3
  do l = 1,m
!       conv rad/s -> Hz
    write(iu,ft,err=200) (rad2hzf(fcr(k,l)),k = 1,nit)
  end do
  write(iu,5,err=200)
!
!     diagram points
  call fltout(iu,txtmsgf(50, 4))
  call fltout(iu,txtmsgf(50, 95))
!     prep format
  write(ft,350) c2(2),nit,cf8,ncc,cf6
  txt(2) = txtmsgf(50, 23)
  write(iu,ft,err=200) txt(2)(1:20),&
  &(dmp(k),cop(4),k = 1,nit),(k,k = 1,ncc)
!
  write(ft,15) cop(2),ncc+nit+1,cf1,cop(3)
!     convert rpm to rad/s -> xv
  xv = rpm2radf(nini)
!     convert to rpm and Hz
  do l = 1,npi
    write(iu,ft,err=200) rad2rpmf(xv),&
    &(rad2hzf(xv*dmp(k)),k=1,nit),(rad2hzf(gi(k,l)),k = 1,ncc)
!       rad/s
    xv = xv+ndw
  end do
!
  write(iu,5,err=200)
!
!     number of eigenvalues
  n = ncc
!     prep format
  write(ft,250) n
  call fltout(iu,txtmsgf(50, 6))
  txt(1) = txtmsgf(50, 8)
  l = len_trim(txt(1))
  write(iu,ft,err=200) txt(1)(1:l),(k,k=1,n)
!
!     prep format
  write(ft,300) cf2,n,cf4
  do l = 1,ncf
    write(iu,ft,err=200)omg(l),(c2(1),real(av(l,k), wp),aimag(av(l,k)),cop(3),k=1,n)
  end do
!
  write(iu,5)
!
!     logarithmic decrement API 684
  txt(1) = txtmsgf(50, 7)
  l = len_trim(txt(1))
  write(iu,'(a)',err=200) 'Logarithmic Decrement raw (not clipped)'
!     prep format
  write(ft,275) n
!     s=p+/-iw -> ld = 2*pi*p/|w|
  txt(1) = txtmsgf(50, 9)
  l = len_trim(txt(1))
  write(iu,ft,err=200) txt(1)(1:l),(k,k=1,n)
!
!     prepare format
  write(ft,300) cf2,n,cf3
  do l = 1,ncf
    do k = 1,n
      audit_metric=prescribed_metrics(av(l,k))
      ld(l,k)=audit_metric%delta
      if(audit_metric%delta_defined) then
        ldplot(l,k)=max(-real(ldm,wp),min(real(ldm,wp),ld(l,k)))
      else
        safe_plot=.false.
        ldplot(l,k)=0._wp ! Never plotted: SAFE_PLOT suppresses invalid legacy HPGL output.
      end if
    end do
    write(iu,ft,err=200) rad2rpmf(omg(l)),(ld(l,k),k = 1,n)
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
  if(plt.and..not.safe_plot) write(*,'(a)') &
    'RD_AUDIT_V1 NOTE HPGL_SUPPRESSED_UNDEFINED_DECREMENT'
  if (plt.and.safe_plot) then
    if (.not. std) then
!         output file -> ofn
      call mntfnm(-1,m,ofn)
    end if
!
!     added errmsg,oki - francisco - feb-19
    call cpbl(n,ncf,npi,ncc,ncs,nit,spdn,nini,ndw,gi,&
    &rcr,fcr,dmp,omg,ldplot,ldm,mxc,mxm,mtg,std,ofn,errmsg,oki)
    if (oki .lt. 0) return
  end if
!     clear extrapolation warning, see parmanv.f
  call eplwrg(0,ft)
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
15 format(a,i3,2a)
25 format(a)
35 format(a,E18.10,' - ',E18.10)
45 format(a,E18.10)
55 format(3(a,1x),f5.1)
65 format(a,i3,a)
250 format('(a,',i3,'(i18,21x))')
275 format('(a,',i3,'(i9,9x))')
300 format('(',a,i3,a,')')
350 format('(',a,i3,a,',',i3,a,')')
!
end subroutine s_campbl
!
!     ==================================================================
!>    @brief undamped critical speed map output.
!
!>    @param[in] nk number of stiffnesses
!>    @param[in] ncc number of critical speed
!>    @param[in] spd system speed (rpm)
!>    @param[in] spdn rated speed (rpm)
!>    @param[in] vr stiffnesses vector
!>    @param[in] rcr 1x intercections vector
!>    @param[in] par bearing stiffness array
!>    @param[in] mxk stiffnes vector max dimension
!>    @param[in] mxc critical speed max dimension
!>    @param[in] ang true for angular stiffness map
!>    @param[in] std standard input output
!>    @param[in] plt HPGL output
!>    @param[in] prv true if bearing variable parameters
!>    @param[out] errmsg return error message
!>    @param[out] ok return flag. unsuccessful if <0.
!
subroutine s_maprig(nk,ncc,spd,spdn,vr,rcr,par,mxk,&
&mxc,ang,std,plt,prv,errmsg,ok)
  use rd_textfun, only: fomsgf, txtmsgf
  use com_sta, only: stm
  use com_ymc, only: nbrg, rks, pc
  use rd_kinds, only: lrk
  implicit none
!
!     locals
  logical :: beastiff, bok
  integer :: bn, k, l, m, iu
  character(len=2) :: c2
  character(len=5) :: c5
  character(len=6) :: c6
  character(len=8) :: nmm, c8
  character(len=10) :: sec
  character(len=12) :: c12
  character(len=18) :: ft, c18
  character(len=20) :: c20
  character(len=50) :: txt
  character(len=255) :: ofn
  real(lrk) :: am, rad2rpmf, sped, xx, zz, pu
!     input parameter
  integer :: mxm, npi
  parameter (mxm = 9,npi = 6)
  dimension c6(2),c8(3),&
!     bearing curves
  &sped(mxm,npi),xx(mxm,npi),zz(mxm,npi),pu(npi)
  parameter (nmm = 's_maprig',c2 = 'a,',c5 = 'stdio',&
  &c6 = (/'E18.10','(a,7x)'/),&
  &c8 = (/'ucspdmap','uacspmap','(i2,16x)'/),&
  &c12 = '(i3,3E18.10)',&
  &c18 = '(''('',i3,a,'')'')',&
  &c20 = '(''('',a,i3,a,'')'')')
!
!     arguments
  integer :: nk, ncc, mxk, mxc, ok
  real(lrk) :: spd, spdn, vr, rcr, par
  dimension vr(mxk),rcr(mxc,mxk),par(mxm,10)
  character(len=99) :: errmsg
  logical :: ang, std, plt, prv
!
!     bearings
!
!     stamp
!
!     speed pu data
  data pu/0.55_lrk,0.8_lrk,1.0_lrk,1.15_lrk,1.3_lrk,1.35_lrk/
!
  intrinsic :: len_trim
!
  ok = -1
!
!     not std i/o
  if (.not. std) then
    iu = 12
    if (.not. ang) then
      call mntfnm(9,m,ofn)
    else
      call mntfnm(17,m,ofn)
    end if
!       output file
    open(iu,file=ofn(1:m),err=100)
  else
!       std i/o
    iu = 6
    ofn = c5
    if (.not. ang) then
      sec = c8(1)
    else
      sec = c8(2)
    end if
    call marksec(sec,0)
  end if
!
!     calculate bearing stiffness
  bn = 0
!     bok will be true when all pu points where calculated ok
  do k = 1,nk
    am = rad2rpmf(rcr(1,k))
!       bearing stiffnes curves
!       check for non null or negative values
!       for stiffness->vr and speed->am
    if (vr(k) .gt. 0 .and. am .gt. 0) then
      bok = beastiff(vr(k),am,pu,sped,xx,zz,bn,mxm,npi,prv)
    end if
  end do
!
  write(iu,35,err=200) stm
  call outdsc(iu)
  if (.not. ang) then
    call fltout(iu,txtmsgf(50, 10))
  else
    call fltout(iu,txtmsgf(50, 11))
  end if
!
  if (.not. ang .and. spd .eq. 0) then
!       gyroscopic message
    call fltout(iu,txtmsgf(50, 12))
  else
!       speed
    txt = txtmsgf(50, 13)
    l = len_trim(txt)
    write(iu,5,err=200) txt(1:l),spd
  end if
!     rated speed rpm
  txt = txtmsgf(50, 152)
  l = len_trim(txt)
  write(iu,5,err=200) txt(1:l),spdn
  write(iu,25,err=200)
!
  if (ang) then
!       radial bearing stiffness
    call fltout(iu,txtmsgf(50, 14))
    call fltout(iu,txtmsgf(50, 15))
    k = 0
    do l = 1,nbrg
      k = k+1
!         kxx and kzz
      write(iu,15,err=200) k,par(k,1),par(k,4)
    end do
    write(iu,25,err=200)
  end if
!
!     diagram points
  call fltout(iu,txtmsgf(50, 35))
  call fltout(iu,txtmsgf(50, 95))
!
!     prepare format
  write(ft,c20) c2,ncc,c8(3)
  txt = txtmsgf(50, 16)
  l = len_trim(txt)
  write(iu,ft,err=200) txt(1:l),(k,k=1,ncc)
!     prepares output format
  write(ft,c18) ncc+1,c6(1)
  do l = 1,nk
    write(iu,ft,err=200) vr(l),(rad2rpmf(rcr(k,l)),k = 1,ncc)
  end do
!
!     bearing stiffness x speed
  write(iu,25,err=200)
!     radial bearing stiffness
  call fltout(iu,txtmsgf(50, 14))
!     bea speed (rpm)       kxx               kzz
  txt = txtmsgf(50, 150)
!     prepare format
  write(ft,c18) bn,c6(2)
  write(iu,ft,err=200) (txt,l=1,bn)
!
!     prepare format
  write(ft,c18) bn,c12
  do k = 1,npi
    write(iu,ft,err=200) (l,sped(l,k),xx(l,k),zz(l,k),l=1,bn)
  end do
!
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
!         output file -> ofn
      if (.not. ang) then
        call mntfnm(-9,m,ofn)
      else
        call mntfnm(-17,m,ofn)
      end if
    end if
    call csmp(spd,vr,rcr,sped,spdn,xx,zz,nk,npi,bn,&
    &ncc,mxc,mxk,mxm,ang,std,bok,ofn)
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
5 format(a,E18.10)
15 format(i5,2E18.10)
25 format()
35 format(a)
!
end subroutine s_maprig
!
!     ==================================================================
!>    @brief unbalance response output.
!
!>    @param[in] nm number of considered modes
!>    @param[in] np number of output points
!>    @param[in] nr number of speeds
!>    @param[in] cn amplitude maximums vector
!>    @param[in] py y output vector
!>    @param[in] desp coordinate 1=horizontal, 2 vertical
!>    @param[in] ori coordinate rotation angle (rad)
!>    @param[in] rpg speeds vector  (rpm)
!>    @param[in] amp complex amplitude vector (m)
!>    @param[in] vf amplification fators vector
!>    @param[in] vr maximum speeds vector
!>    @param[in] vm maximum values vector
!>    @param[in] st status: (i)nitial (f)inal (F)ail
!>    @param[in] mxp position vector dimension
!>    @param[in] mte speed vector dimension
!>    @param[in] std standard input output
!>    @param[in] frl true if plot vertical scale is logarithmic
!>    @param[in] plt HPGL output
!>    @param[out] errmsg return error message
!>    @param[out] ok return flag. unsuccessful if <0.
!
!     added pv - francisco - feb-19
subroutine s_resp_f(nm,np,nr,cn,py,desp,ori,rpg,amp,&
&vf,vr,vm,st,mxp,mte,std,frl,plt,errmsg,ok)
  use rd_textfun, only: fomsgf, txtmsgf
  use com_mfa, only: ma, rm, au
  use com_sta, only: stm
  use rd_kinds, only: lrk, wp
  implicit none
!
!     locals
  integer :: i, j, k, m, iu, oki
  real(lrk) :: arg, riff, rpif, rargf
  character(len=1) :: c1
  character(len=4) :: aun
  character(len=5) :: c5
  character(len=6) :: c6
  character(len=7) :: c7
  character(len=8) :: nmm, c8
  character(len=10) :: sec
  character(len=12) :: c12
  character(len=18) :: urp
  character(len=19) :: c19
  character(len=22) :: c22
  character(len=35) :: ft
  character(len=50) :: txt, txv
  character(len=255) :: ofn
  logical :: fail
  dimension k(6), txv(6), c5(3), c8(2)
  parameter(c1 = '(',c5 = (/'(20x,','(a20,','stdio'/),&
  &c7 = 'E18.10)',c8 = (/'(a19,1x,','(a20,1x,'/),&
  &c6 = '(a18))',c12 = '(a14,i2,2x))',&
  &c19 = '('' y'',a10,i2,4x))',&
  &c22 = '(i2,'' @'',f6.2,8x),a)',&
  &nmm = 's_resp_f',aun = 'rad.',sec = 'unblresp')
!
!     arguments
  integer :: nm, np, nr, mxp, mte, ok
  dimension urp(np)
  real(lrk) :: py, desp, ori, rpg
  complex(wp) :: amp
  character(len=99) :: errmsg
  logical :: std, frl, plt
  dimension py(mxp),desp(mxp),ori(mxp),rpg(mte),&
  &amp(mxp,mte)
!
!     response analysis
  integer :: cn
  real(lrk) :: ang, vf, vr, vm
  character(len=1) :: st
  dimension cn(mxp),&
  &ang(mxp),vf(mxp,2*mxp),vr(mxp,2*mxp),vm(mxp,2*mxp),&
  &st(mxp,2*mxp)
!
!     main amplification and modal rpm
!     angle unit r -> radian, default degree
!
!     stamp
!
  intrinsic :: abs, int, len_trim, nint, real, aimag
!
  ok = -1
!
  if (.not. std) then
    iu = 12
    call mntfnm(2,m,ofn)
!       open output file
    open(iu,file=ofn(1:m),err = 100)
  else
    iu = 6
    ofn = c5(3)
    call marksec(sec,0)
  end if
!
  write(iu,5,err=200) stm
  call outdsc(iu)
!     '  Unbalance Response - modes :'
  txt = txtmsgf(50, 17)
  i = len_trim(txt)
  write(iu,15,err=200) txt(1:i),nm
!     eventual speed extrapolation message, see parmanv.f
  call eplwrg(-1,txt)
  j = len_trim(txt)
  if (j .gt. 0) write(iu,5,err=200) txt(1:j)
  write(iu,220,err=200)
!
!     prepare format
  write(ft,300) c5(1),np,c12
!     '  point'
  txt = txtmsgf(50, 18)
  write(iu,ft,err=200) (txt(1:18),i,i = 1,np)
!     response positions
  do i = 1,np
    if (.not. py(i) .lt. 0) then
      write(urp(i),25) py(i)
    else
!         ' supp = '
      txt = txtmsgf(50, 175)
      j = len_trim(txt)
      write(urp(i),55) txt(1:j),nint(abs(py(i)))
    end if
  end do
!     prepare format
  write(ft,300)c8(1),np,c6
!     '  y (m)'
  txt = txtmsgf(50, 42)
  write(iu,ft,err=200) txt(1:20),(urp(i),i = 1,np)
!     prepare format
  write(ft,300) c8(2),np,c22
!     '  coordinate'
  txt = txtmsgf(50, 20)
  write(iu,ft,err=200) txt,(int(desp(i)),ori(i),i = 1,np),aun
  write(iu,220,err=200)
!
  call fltout(iu,txtmsgf(50, 21))
!
  call fltout(iu,txtmsgf(50, 22))
  write(iu,25,err=200) ma
  write(iu,220,err=200)
  write(iu,35,err=200) (txtmsgf(50, 22+i),i=1,5)
  fail = .false.
  do i = 1,np
    do j = 1,cn(i)
!         amplification factor check
      if (vf(i,j) .ge. ma) then
        write(iu,250,err=200) vr(i,j),vm(i,j),vf(i,j),i,st(i,j)
      else
        fail = .true.
      end if
    end do
  end do
!
!     failed by means of af
  if (fail) then
    write(iu,220)
    do i = 1,np
      do j = 1,cn(i)
!           amplification factor check
        if (vf(i,j) .lt. ma) then
          write(iu,250,err=200) vr(i,j),vm(i,j),vf(i,j),i,st(i,j)
        end if
      end do
    end do
  endif
!
  write(iu,220)
  do i = 1,6
    txv(i) = txtmsgf(50, 26+i)
    k(i) = len_trim(txv(i))
  end do
  write(iu,45,err=200)(txv(i)(1:k(i)),i = 1,6)
  write(iu,220,err=200)
!
  call fltout(iu,txtmsgf(50, 33))
  call fltout(iu,txtmsgf(50, 95))
!     prep format
  write(ft,300)c5(2),np,c19
  write(iu,ft,err=200)txtmsgf(50, 23),(txtmsgf(50, 18),i,i = 1,np)
  write(ft,300) c1,np+1,c7
!
  do i = 1,nr
    write(iu,ft,err=200)rpg(i),(abs(amp(j,i)),j = 1,np)
  end do
!
  write(iu,220,err=200)
  call fltout(iu,txtmsgf(50, 34))
!
  write(ft,300) c5(2),np,c19
  write(iu,ft,err=200) txtmsgf(50, 23),(txtmsgf(50, 18),i,i = 1,np)
  write(ft,300) c1,np+1,c7
!
!     angle in radian
  do i = 1,nr
    do j = 1,np
!         angle
      arg = rargf(amp(j,i))
!         if negative add + 2*pi
      ang(j) = riff(arg .lt. 0,arg+2*rpif(),arg)
    end do
    write(iu,ft,err=200)rpg(i),(ang(j),j = 1,np)
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
!         output file -> ofn
      call mntfnm(-2,m,ofn)
    end if
!
!     added errmsg,oki - francisco - feb-19
    call unbr(rpg,amp,np,nr,mxp,mte,frl,std,ofn,errmsg,oki)
    if (oki .lt. 0) return
  end if
!     clear extrapolation warning, see parmanv.f
  call eplwrg(0,txt)
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
25 format(E18.10)
35 format(3a20,2a10)
45 format(2x,6(a,1x))
55 format(1x,a,i2)
220 format()
250 format(3(E18.10),i10,7x,a)
300 format(a,i2,a)
!
end subroutine s_resp_f
!
!     ==================================================================
!>    @brief modes output.
!
!>    @param[in] rpm speed variable bearing parameters
!>    @param[in] qtm number of desired modes
!>    @param[in] npv number of sections
!>    @param[in] fmd mode frequency vector  (hz)
!>    @param[in] py section position vector (m)
!>    @param[in] u1  mode shape vector x
!>    @param[in] w1 mode shape vector  z
!>    @param[in] orx x orbits vector
!>    @param[in] orz z orbits vector
!>    @param[in] di whirl direction vector (FW/BW)
!>    @param[in] npo number of orbit points
!>    @param[in] xmd position vector dimension
!>     and u1,w1 matrix first dimension
!>    @param[in] mts section vector dimension
!>    @param[in] mtf u1,w1 matrix second dimension
!>    @param[in] std standard input output
!>    @param[in] plt HPGL output
!>    @param[out] errmsg return error message
!>    @param[out] ok return flag. unsuccessful if <0.
!
subroutine s_modos(rpm,qtm,npv,fmd,py,u1,w1,orx,&
&orz,di,npo,xmd,mts,mtf,std,plt,errmsg,ok)
  use rd_textfun, only: fomsgf, txtmsgf
  use com_mdd2, only: imd, nsm
  use com_sta, only: stm
  use rd_kinds, only: lrk
  implicit none
!
!     arguments
  integer :: qtm, npv, xmd, mts, mtf, npo, ok
  real(lrk) :: rpm, u1, w1, fmd, py, orx, orz
  character(len=2) :: di
  dimension u1(xmd,mtf),w1(xmd,mtf),fmd(xmd),py(mts),&
  &orx(xmd,mts,npo),orz(xmd,mts,npo),di(xmd)
  character(len=99) :: errmsg
  logical :: std, plt
!
!     locals
  integer :: i, iu, im, j, k, l, n, nm
  character(len=1) :: c1
  character(len=2) :: c2
  character(len=5) :: c5
  character(len=7) :: nmm, c7
  character(len=10) :: sec
  character(len=14) :: c14
  character(len=25) :: ft
  character(len=50) :: txt
  character(len=255) :: ofn
  dimension im(xmd),c2(2),c5(2),k(3),txt(3)
  parameter (sec = 'modeshp',nmm = 's_modos',&
  &c1 = '(',c2 = (/' x',' z'/),c5 = (/'(a20,','stdio'/),&
  &c7='E18.10)',c14 = '(a,a10,i2,4x))')
!
!     modes to show
!     max number of modes,search for commons
!     nmx = xmd
  integer :: nmx
  parameter (nmx = 19)
!
!     stamp
!
  intrinsic :: len_trim
!
!     init return
  ok = -1
!     not std i/o
  if (.not. std) then
    iu = 12
    call mntfnm(0,i,ofn)
    open(iu,file=ofn(1:i),err=100)
  else
!       std i/o
    iu = 6
    ofn = c5(2)
    call marksec(sec,0)
  end if
!
!     check modes to show
  if (nsm .eq. 0) then
!       modes to show not set
!       print all
    nm = qtm
    do i = 1,nm
      im(i) = i
    end do
  else
!       modes to show set
    nm = nsm
!       check for number of calculated modes
    if (nm  .gt. qtm) nm = qtm
    do i = 1,nm
      im(i) = imd(i)
    end do
  end if
!
  write(iu,5,err=200)stm
  call outdsc(iu)
  write(iu,'(a)') 'FREQUENCY_KIND DAMPED_IMAGINARY_PART; PHYSICAL_POLE s=-a'
  write(iu,'(a)') 'DIR: FW=forward BW=backward MX=mixed LN=linear ND=negligible UD=undefined'
!
  txt(1) = txtmsgf(50, 36)
  txt(2) = txtmsgf(50, 100)
  k(1) = len_trim(txt(1))
  k(2) = len_trim(txt(2))
  write(iu,35,err=200)txt(1)(1:k(1)),txt(2)(1:k(2)),rpm
  write(iu,220)
!
  call fltout(iu,txtmsgf(50, 37))
!
!     mode =
  txt(1) = txtmsgf(50, 38)
!     f =
  txt(2) = txtmsgf(50, 96)
!     (Hz) dir =
  txt(3) = txtmsgf(50, 40)
  do i = 1,3
    k(i) = len_trim(txt(i))
  end do
!
  do j = 1,nm
!       mode to show
    i = im(j)
    write(iu,1000,err=200)txt(1)(1:k(1)),i,&
    &txt(2)(1:k(2)),fmd(i),txt(3)(1:k(3)),di(i)
  end do
!     eventual speed extrapolation message, see parmanv.f
  call eplwrg(-1,txt(1))
  j = len_trim(txt(1))
  if (j .gt. 0) write(iu,5,err=200) txt(1)(1:j)
  write(iu,220,err=200)
!
  call fltout(iu,txtmsgf(50, 41))
  call fltout(iu,txtmsgf(50, 95))
!     format prep
  write(ft,15) c5(1),2*nm,c14
  write(iu,ft,err=200)txtmsgf(50, 42),(c2(1),txtmsgf(50, 38),im(i),i=1,nm),(c2(2),txtmsgf(50, 38),im(i),i=1,nm)
!
  write(ft,15) c1,(2*nm)+1,c7
!
!     note mode indices im(i)
  do j = 1,npv
    write(iu,ft,err=200) py(j),&
    &(u1(im(i),j),i = 1,nm),(w1(im(i),j),i = 1,nm)
  end do
!
  write(iu,220,err=200)
  call fltout(iu,txtmsgf(50, 43))
  write(iu,220,err=200)
!
  do n = 1,nm
!       mode to show
    i = im(n)
    write(iu,1000,err=200)txt(1)(1:k(1)),i,&
    &txt(2)(1:k(2)),fmd(i),txt(3)(1:k(3)),di(i)
    write(iu,220,err=200)
    do j = 1,npv
      write(iu,1100,err=200)txtmsgf(50, 44),j,txtmsgf(50, 97),py(j)
      write(iu,25,err=200)txtmsgf(50, 46),txtmsgf(50, 47)
      do l = 1,npo
        write(iu,55,err=200)orx(i,j,l),orz(i,j,l)
      end do
      write(iu,220,err=200)
    end do
    write(iu,220,err=200)
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
!         output file, 0=>-99 -> ofn
      call mntfnm(-99,i,ofn)
    end if
    call mshp(rpm,fmd,py,orx,orz,npv,npo,qtm,xmd,mts,std,di,ofn)
  end if
!     clear extrapolation warning, see parmanv.f
  call eplwrg(0,ft)
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
25 format(2a18)
35 format(2a,E18.10)
55 format(2E18.10)
220 format()
1000 format(a,1x,i2,1x,a,E18.10,1x,a,1x,a)
1100 format(a10,i2,a12,e18.10)
!
end subroutine s_modos
!
!     ==================================================================
!>    @brief search for a point on a line between two x,y points
!>     with a given angle.
!
!>    @param[in] x1 first x point
!>    @param[in] y1 first y point
!>    @param[in] x2 second x point
!>    @param[in] y2 second y point
!>    @param[in] ang desired angle deg
!>    @param[out] x fit point x coordinate
!>    @param[out] y fit point y coordinate
!>    @return completion status
!
logical function findanf(x1,y1,x2,y2,ang,x,y)
  use rd_kinds, only: lrk
  implicit none
!     arguments
  real(lrk) :: x1, y1, x2, y2, ang, x, y
!     locals
  integer :: i, mx
  real(lrk) :: a, an, ar, b, da, dx, dy, nx1, nx2
  real(lrk) :: toradf, todegpf
  logical :: go, rpeqf
  character(len=1) :: blank
  character(len=6) :: nmm
  parameter(blank = ' ',nmm = 'findan')
!     parameters
  real(lrk) :: er, prec
  parameter (er = 1E-2_lrk,prec = 1e-9_lrk,mx = 150)
!
  intrinsic :: atan2, tan, abs
!
!     calculate line equation deltas
  dx = x2-x1
  dy = y2-y1
!     estimate a x media
  x = x1+(dx/2._lrk)
!     check for constant x
  if (rpeqf(dx,0._lrk,prec)) then
!       x is constant
    x = x1
!       deg -> rad
    an = toradf(ang)
    y = x*tan(an)
  else
!       having a line equation y = a * x + b
    a = dy/dx
    b = y1-a*x1
!       try to find the desired angle
    i = 0
    go = .true.
!       initial search interval
    nx1 = x1
    nx2 = x2
!       apply interval x media on the equation
    do while (go)
      y = a*x+b
      ar = atan2(y,x)
!         rad -> deg >0
      an =  todegpf(ar,1)
      da = an-ang
!         check for calculated angle in the desired error range
      go = abs(da) .gt. er
      if (go) then
!           estimate new x
!           "an" can stay between say, 350 and 10 degree
        if (da .lt. 0 .or. da .gt. 180._lrk) then
          dx = nx2-x
          nx1 = x
          x = x+dx/2
        else
          dx = x-nx1
          nx2 = x
          x = x-dx/2
        end if
!           counter
        i = i+1
        if (i .gt. mx) then
!             no convergence
          findanf = .false.
          return
        end if
      end if
!         end find loop
    end do
  end if
!
  findanf = .true.
!
  return
!
end function findanf
!
!     ==================================================================
!>    @brief get the modulus value at a angle from the ellipse orbit vec
!
!>    @param[in] x x orbit component vector (m)
!>    @param[in] z z orbit component vector (m)
!>    @param[in] rang desired angle for the modulus calculation (deg)
!>    @param[in] n number of x and z points
!>    @param[in] dm dimension of x and z vectors
!>    @param[out] value modulus at the desired angle (m)
!>    @return completion status, on false value = 0
!
logical function getvlatf(x,z,rang,n,dm,value)
  use rd_kinds, only: lrk
  implicit none
!     arguments
  integer :: n, dm
  real(lrk) :: x, z, rang, value
  dimension x(dm),z(dm)
!     locals
  integer :: i, j, k, l, iord
  logical :: ok, findanf
  real(lrk) :: an, xi, zi, prec, xrdif, ang, angles, todegpf, adjangf
  dimension angles(n),iord(n)
  parameter (prec = 1e-8_lrk,xrdif = 1e-4_lrk)
!
  intrinsic :: abs, atan2, sqrt
!
  ok = .false.
  value = 0._lrk
!     check for angle range 0-360
  ang = adjangf(rang)
!     calculate the orbit point angles deg
  do i = 1,n
    an = atan2(z(i),x(i))
!       rad -> deg>0
    angles(i) = todegpf(an,1)
  end do
!     note that first = last
!     order ascending angle, indices -> iord
  call ord_ir(angles,iord,n,n,0)
!     first point
  k = iord(1)
!     angle at first point
  an = angles(k)
!     check for angle before first point
  if (ang .lt. an) then
!       first angle may be equals to last
    i = 0
!       loop limit
    j = n-2
!       last angle index
    l = iord(n-i)
!       relative difference last and first
    zi = abs((x(l)-x(k))/x(k))
!       find x with some min x separation
    do while(zi .lt. xrdif .and. i .lt. j)
      i = i+1
      l = iord(n-i)
      zi = abs((x(l)-x(k))/x(k))
    end do
!       check for loop limit
    if (i .lt. j) then
!         limit ok
      j = iord(n-i-1)
!         uses last-i and first point -> xi,zi
      ok = findanf(x(j),z(j),x(k),z(k),ang,xi,zi)
      if (ok) then
!           amplitude
        value = sqrt(xi**2+zi**2)
      end if
    else
!         out on loop limit
      ok = .false.
    end if
  else
!       look for output angle between to calculated
    do i = 1,n-1
      j = iord(i)
      k = iord(i+1)
      an = angles(k)
      if (ang .ge. angles(j) .and. ang .le. an) then
!           find -> xi,zi
        ok = findanf(x(j),z(j),x(k),z(k),ang,xi,zi)
        if (ok) then
          value = sqrt(xi**2+zi**2)
          exit
        end if
      end if
    end do
  end if
!
  getvlatf = ok
!
  return
!
end function getvlatf
!
!     ==================================================================
!>    @brief time response output.
!
!>    @param[in] shp speed shape flag, not zero speed shape
!>    @param[in] np number of output(/sections, speed shape) points
!>    @param[in] py y output position vector
!>    @param[in] rpg speed vector (rpm)
!>    @param[in] u orbit x vector
!>    @param[in] w orbit z vector
!>    @param[in] di whirl dirction vector (FW/BW)
!>    @param[in] ns number of speeds
!>    @param[in] npo number of orbit points
!>    @param[in] nmd number of considered modes
!>    @param[in] mxp position vector dimension (mts)
!>    @param[in] mte speed vector dimension
!>    @param[in] rang desired angle to calculate orbit modulus
!>    @param[in] std standard input output
!>    @param[in] plt HPGL plot output
!>    @param[out] errmsg return error message
!>    @param[out] ok return flag. unsuccessful if <0.
!
subroutine s_resp_t(shp,np,py,rpg,u,w,di,ns,npo,nmd,&
&mxp,mte,rang,std,plt,errmsg,ok)
  use rd_textfun, only: cadjf, femsgf, fomsgf, txtmsgf
  use com_mdd1, only: ru
  use com_sta, only: stm
  use rd_kinds, only: lrk
  implicit none
!     arguments
  integer :: np, ns, npo, nmd, mxp, mte, ok
  real(lrk) :: rpg, u, w, py, rang
  dimension rpg(mte),u(mxp,mte,npo),w(mxp,mte,npo),py(mxp)
!     forward whril FW / backward BW
  character(len=2) :: di(mxp,mte)
  character(len=99) :: errmsg
  logical :: shp, std, plt
!
!     response angle unit 'd' or 'D' for degree, default radian
!     time response angle unit 'r' or 'R' for radian, default degree
!
!     locals
  logical :: isr
  integer :: i, j, k, l, m, nk, nw, iu
  real(lrk) :: da, dm, d1, a1, value, x, z, xm, ym, cm, rp, riff, todegf
  character(len=1) :: crd, c1
  character(len=4) :: c4
  character(len=5) :: c5
  character(len=8) :: nmm
  character(len=10) :: sec
  character(len=50) :: txt
  character(len=255) :: ofn
  dimension value(np,ns),x(npo),z(npo),rp(np,ns),&
  &xm(3),ym(3),cm(3),c1(2),d1(np),a1(np)
  parameter(c1 = (/'R','r'/),c4 = '(m)',c5 = 'stdio',&
  &sec = 'timeresp',nmm = 's_resp_t')
!     function
  logical :: okl, getvlatf
!     stamp
!
  intrinsic :: abs, atan2, len_trim, nint, sqrt
!
  ok = -1
!
!     check for time response angle in radian, default degree
  crd = cadjf(1, ru(2),1)
  isr = crd .eq. c1(1) .or. crd .eq. c1(2)
!     response angle in degree
  da = riff(isr,todegf(rang),rang)
!
  if (.not. std) then
    iu = 12
    call mntfnm(3,m,ofn)
!       open output file
    open(iu,file=ofn(1:m),err=100)
  else
    iu = 6
    ofn = c5
    call marksec(sec,0)
  end if
!
  write(iu,5,err=200) stm
  call outdsc(iu)
!     time response
  txt = txtmsgf(50, 48)
  j = len_trim(txt)
!     number of modes
  write(iu,35,err=200) txt(1:j),nmd
!     eventual speed extrapolation message, see parmanv.f
  call eplwrg(-1,txt)
  j = len_trim(txt)
  if (j .gt. 0) write(iu,5,err=200) txt(1:j)
!
  write(iu,220,err=200)
!     number of orbit points
  txt = txtmsgf(50, 49)
  j = len_trim(txt)
  write(iu,35,err=200) txt(1:j),npo
!
  nk = 0
  do i = 1,ns
!       speeds
    write(iu,220,err=200)
!       (rpm)
    txt = txtmsgf(50, 99)
    j = len_trim(txt)
!       speed =
    write(iu,1000,err=200) txtmsgf(50, 45),rpg(i),txt(1:j)
!       number of output(/ sections, speed shape) points
    do l = 1,np
      d1(l) = 0
      write(iu,220,err=200)
!         check for support response
      if (.not. py(l) .lt. 0) then
!           shaft response
!           'sec','y =',c4,' dir ='
        write(iu,1100,err=200) txtmsgf(50, 44),l,txtmsgf(50, 97),py(l),c4,txtmsgf(50, 98),di(l,i)
      else
!           support response
!           'sec',' supp = '
        write(iu,1150,err=200) txtmsgf(50, 44),l,txtmsgf(50, 175),nint(abs(py(l))),txtmsgf(50, 98),di(l,i)
      end if
!         '  x (m)','  z (m)'
      write(iu,15,err=200) txtmsgf(50, 46),txtmsgf(50, 47)
!
      do j = 1,npo
        write(iu,1400,err=200) u(l,i,j),w(l,i,j)
!           first one speed
        if (i .eq. 1) then
!             orbit amplitude
          dm = sqrt(u(l,i,j)**2+w(l,i,j)**2)
!             search max
          if (dm .gt. d1(l)) then
            d1(l) = dm
            m = j
          end if
        end if
      end do
!         first one speed
      if (i .eq. 1) then
!           find maximum by three points parabola
!           check mean point
        if (m .eq. 1) m = m+1
        if (m .eq. npo) m = npo-1
!           first
        k = m-1
!           last point
        j = m+1
!           last point = 50, next = 1, take 2
        if (j .gt. npo) j = 2
!           prepare vectors x and y
        xm(1) = u(l,i,k)
        xm(2) = u(l,i,m)
        xm(3) = u(l,i,j)
        ym(1) = w(l,i,k)
        ym(2) = w(l,i,m)
        ym(3) = w(l,i,j)
!           calculate quadratic coefficients -> cm
        call  qcfr(xm,ym,cm)
!           calculate max x and y max point -> dm,d1
        call mxqr(dm,d1(l),cm)
!           orbit maximum value angle
        a1(l) = atan2(d1(l),dm)
!           orbit maximum displacement value
        d1(l) = sqrt(dm**2+d1(l)**2)
      end if
!         output loop
    end do
  end do
!
!     displacement at angle
  nw = 0
!     response points
  do l = 1,np
    nk = 0
!       speeds
    do i = 1,ns
!         orbit point vectors
      do j = 1,npo
        x(j) = u(l,i,j)
        z(j) = w(l,i,j)
      end do
!         modulus value at given angle on this orbit->dm
      okl = getvlatf(x,z,da,npo,npo,dm)
!         check if could get
      if (okl) then
!           got amplitude
        nk = nk+1
        value(l,nk) = dm
        rp(l,nk) = rpg(i)
      else
!           angle not found -> warning message
        nw = nw+1
      end if
    end do
  end do
!
!     if warning count >0 -> message
  if (nw .gt. 0) then
!       unable to determine time response amplitude
    errmsg = femsgf(99, nmm,33,31,0)
!       just warning message
    call wlmsg(errmsg)
  end if
!
  write(iu,220,err=200)
!     module response
  txt = txtmsgf(50, 51)
  l = len_trim(txt)
  write(iu,25,err=200) txt(1:l),rang
  write(iu,220,err=200)
!     modules output
  if (ns .gt. 1) then
!       number of output(/ sections, speed shape) points
    do l = 1,np
      write(iu,1200,err=200) txtmsgf(50, 44),l,txtmsgf(50, 97),py(l)
      write(iu,220,err=200)
      write(iu,1300,err=200) txtmsgf(50, 45),txtmsgf(50, 52)
!         number of speeds
      do i = 1,nk
        write(iu,1400,err=200 )rp(l,i),value(l,i)
      end do
      write(iu,220,err=200)
    end do
  else
!       number of speeds = 1
    do l = 1,nk
!         (rpm)
      txt = txtmsgf(50, 99)
      j = len_trim(txt)
!         speed =
      write(iu,1000,err=200) txtmsgf(50, 45),rpg(l),txt(1:j)
      write(iu,220,err=200)
!         y (m),module (m)',max. disp. (m),(x^2+z^2)^.5 (m)
      write(iu,1300,err=200) txtmsgf(50, 42),txtmsgf(50, 52),txtmsgf(50, 66),txtmsgf(50, 67)
!         number of points
      do i = 1,np
        write(iu,1400,err=200) py(i),value(i,l),a1(i),d1(i)
      end do
      write(iu,220,err=200)
    end do
  end if
!
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
!         output file, 0=>-99 -> ofn
      call mntfnm(-3,m,ofn)
    end if
    if (ns .eq. 1) then
!         single speed
      if (.not. shp) then
!            orbit plot
        call tmsor(rpg(1),py,u,w,a1,d1,np,&
        &npo,nmd,mxp,mte,std,di,ofn)
      else
!           speed shape
        call tmssh(rpg(1),py,u,w,np,npo,nmd,mxp,mte,std,ofn)
      end if
    else
!         orbit against speed and module response
      call tmors(rang,rpg,rp,py,u,w,value,&
      &ns,nk,np,npo,nmd,mxp,mte,std,di,ofn)
    end if
  end if
!     clear extrapolation warning, see parmanv.f
  call eplwrg(0,txt)
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
15 format(2a18)
25 format(a,f10.2)
35 format(a,i3)
220 format()
1000 format(a15,E18.10,a)
1100 format(a10,i2,a10,e18.10,a,a12,a)
1150 format(a10,i2,a10,i2,a12,a)
1200 format(a10,i2,a10,e18.10,1x,e18.10)
1300 format(4a18)
1400 format(4E18.10)
!
end subroutine s_resp_t
!
!     ==================================================================
!>    @brief static elastic line output.
!
!>    @param[in] y section positions vector (m)
!>    @param[in] dl generalized displacement vector x,z,fi,theta
!>    @param[in] rm bearing reaction vector
!>    @param[in] pl maximum positions vector y (m)
!>    @param[in] vl maximum displacement values vector x (m)
!>    @param[in] cl coordinate vector
!>    @param[in] bs bending stress on all sections (Pa)
!>    @param[in] mi bending moment on section coordinates y (Nm)
!>    @param[in] ys shear force on section coordinates (m)
!>    @param[in] si constant shear force on ys section coordinates (m)
!>    @param[in] rkp bearing and support stiffness xx e zz
!>    @param[in] rks pu of nominal speed. <0 use only support values
!>    @param[in] spdn rated speed (rpm)
!>    @param[in] nt number of sections
!>    @param[in] nc number of lateral bearings
!>    @param[in] nl number of maximums x
!>    @param[in] mts y dimension
!>    @param[in] mtg displacement vector (dl) dimension
!>    @param[in] mxr bearing reaction vector (rm) dimension
!>    @param[in] mxb maximum positions (pl), displacement (vl) and
!>     coordinates (cl) vector dimension
!>    @param[in] std standard input output
!>    @param[in] plt HPGL plot flag
!>    @param[in] sf gravity*cos(slope angle)
!>    @param[in] isr slope angle flag rad if true deg else
!>    @param[out] errmsg return error message
!>    @param[out] ok return flag. unsuccessful if <0
!
subroutine s_lin_el(y,dl,rm,pl,vl,cl,bs,&
&mi,ys,si,&
&rkp,rks,spdn,nt,nc,&
&nl,mts,mtg,mxr,mxb,std,plt,sf,isr,errmsg,ok)
  use rd_textfun, only: ciff, fomsgf, txtmsgf
  use com_hang, only: hangle, accg, isf, ha
  use com_sta, only: stm
  use rd_kinds, only: lrk, wp
  implicit none
!
!     locals
  logical :: lst
  integer :: ii, jj, m, iu, id, ifidxof
  real(lrk) :: an, df, dm, ng, bst
  real(wp) :: v1
  character(len=1) :: cori
  character(len=3) :: cdgo, cdg
  character(len=5) :: cstd
  character(len=8) :: nmm
  character(len=10) :: sec
  character(len=50) :: txt, tx1
  character(len=255) :: ofn
  dimension dm(4), cori(5), cdgo(2), id(4)
  parameter (cori = (/'1','2','0',',','='/),cstd = 'stdio',ng = -1,&
  &nmm = 's_lin_el',sec = 'selaline',cdgo = (/'rad','deg'/))
!     function
  real(wp) :: cxzf
!     sum functions, see predad.f
  real(lrk) :: sectmaof, dskmasf, conmasf, disforf
!
!     arguments
  logical :: isr
  integer :: nt, nc, nl, mts, mtg, mxr, mxb
  dimension bst(mxb,2)
  real(lrk) :: y, rm, pl, vl, rkp, rks, spdn, bs, mi, ys, si
  real(wp) :: sf, dl
  integer :: cl
  integer :: ok
  character(len=99) :: errmsg
  logical :: std, plt
!
!     dimension
  dimension y(mts),dl(mtg),&
  &rm(mxr),rkp(mxb,2,2),&
  &pl(mxb),vl(mxb),cl(mxb),bs(mts),&
  &mi(mts,2),ys(mts),si(mts,2)
!
!     slope e gravity
!     angle unit r -> radian, default degree
!     bending stress median filter width
!
!     carimbo
!
  intrinsic :: atan2, len_trim, real
!
!     return init
  ok = -1
!     output kind
  if (.not. std) then
!       regular file
    iu = 12
    call mntfnm(8,m,ofn)
!       open output file
    open(iu,file=ofn(1:m),err=100)
  else
!       standard input/output
    iu = 6
    ofn = cstd
    call marksec(sec,0)
  end if
!
  write(iu,95,err=200)stm
  call outdsc(iu)
  call fltout(iu,txtmsgf(50, 53))
!
  write(iu,10)
!
!     angle rad/deg
  cdg = ciff(3, isr,cdgo(1),cdgo(2))
  txt = txtmsgf(50, 54)
  ii = len_trim(txt)
  write(iu,105,err=200) txt(1:ii),hangle,cdg
!     gravity
  txt = txtmsgf(50, 55)
  ii = len_trim(txt)
  write(iu,20,err=200) txt(1:ii),accg
!     bearing speed
  if (rks .gt. 0) then
!       speed
    txt = txtmsgf(50, 45)
    ii = len_trim(txt)
!       (rpm)
    tx1 = txtmsgf(50, 99)
    jj = len_trim(tx1)
    write(iu,105,err=200) txt(1:ii),rks*spdn,tx1(1:jj)
  end if
!
  write(iu,10,err=200)
!
!     bearing and support stiffness
  call fltout(iu,txtmsgf(50, 146))
!     z
  txt = txtmsgf(50, 63)
  ii = len_trim(txt)
  write(iu,55,err=200)txtmsgf(50, 57),txtmsgf(50, 62),txtmsgf(50, 63),txtmsgf(50, 62),txt(1:ii)
!     bearing loop
  do ii = 1,nc
    write(iu,65,err=200)&
    &ii,rkp(ii,1,1),rkp(ii,2,1),rkp(ii,1,2),rkp(ii,2,2)
!       sum x and z in series
    bst(ii,1) = 1._lrk/(1._lrk/rkp(ii,1,1)+1._lrk/rkp(ii,1,2))
    bst(ii,2) = 1._lrk/(1._lrk/rkp(ii,2,1)+1._lrk/rkp(ii,2,2))
  end do
!     sum
  txt = txtmsgf(50, 144)
  ii = len_trim(txt)
  write(iu,95,err=200) txt(1:ii)
!     bearing loop
  do ii = 1,nc
    write(iu,125,err=200) ii, bst(ii,1), bst(ii,2)
  end do
!
  write(iu,10,err=200)
!
!     bearing / support reactions
  call fltout(iu,txtmsgf(50, 61))
  txt = txtmsgf(50, 63)
  ii = len_trim(txt)
  write(iu,55,err=200)txtmsgf(50, 57),txtmsgf(50, 62),txtmsgf(50, 63),txtmsgf(50, 62),txt(1:ii)
  dm(1) = 0
  dm(2) = 0
!     bearing loop
  do ii = 1,nc
!       reaction index
    jj = ifidxof(ii,1)
    write(iu,65,err=200) ii,rm(jj),rm(jj+1),rm(jj+2),rm(jj+3)
!       sum x and z
    dm(1) = dm(1)+rm(jj)
    dm(2) = dm(2)+rm(jj+1)
!       end bearing loop
  end do
!     sum
  write(iu,115,err=200) txtmsgf(50, 144),dm(1),dm(2)
!
  write(iu,10,err=200)
!
!     bearing / support displacement
  call fltout(iu,txtmsgf(50, 138))
  txt = txtmsgf(50, 63)
  ii = len_trim(txt)
  write(iu,55,err=200)txtmsgf(50, 57),txtmsgf(50, 62),txtmsgf(50, 63),txtmsgf(50, 62),txt(1:ii)
!     bearing loop
  do ii = 1,nc
!       reaction index
    jj = ifidxof(ii,1)
    write(iu,65,err=200) ii,&
    &-rm(jj)/rkp(ii,1,1),-rm(jj+1)/rkp(ii,2,1),&
    &-rm(jj)/rkp(ii,1,2),-rm(jj+1)/rkp(ii,2,2)
  end do
!     sum
  txt = txtmsgf(50, 144)
  ii = len_trim(txt)
  write(iu,95,err=200) txt(1:ii)
!     bearing loop
  do ii = 1,nc
!       reaction index
    jj = ifidxof(ii,1)
    write(iu,125,err=200) ii,&
!       bearing x + support x
    &-rm(jj)/rkp(ii,1,1)-rm(jj)/rkp(ii,1,2),&
!       bearing z + support z
    &-rm(jj+1)/rkp(ii,2,1)-rm(jj+1)/rkp(ii,2,2)
  end do
!
  write(iu,10,err=200)
!
!     mass
!
!     ' section mass  (kg) = '
  txt = txtmsgf(50, 88)
  ii = len_trim(txt)
!     sum of section mass
  an = sectmaof()
  write(iu,20,err=200) txt(1:ii),an
!     sum of section mass -> dm(2)
  dm(2) = an
!     z component of weight force due slope
  dm(1) = ng*an*real(sf, lrk)
!     ' disk mass     (kg) = '
  txt = txtmsgf(50, 89)
  ii = len_trim(txt)
!     sum of disk masses
  an = dskmasf()
  write(iu,20,err=200) txt(1:ii),an
!     z component of weight force due slope
  dm(1) = dm(1)+ng*an*real(sf, lrk)
  dm(2) = dm(2)+an
!     ' concent. mass (kg) = '
  txt = txtmsgf(50, 90)
  ii = len_trim(txt)
!     sum of concent. masses
  an = conmasf()
  write(iu,20,err=200) txt(1:ii),an
!     z component of weight force due slope
  dm(1) = dm(1)+ng*an*real(sf, lrk)
  dm(2) = dm(2)+an
!     '   sum' !144
  write(iu,5,err=200) txtmsgf(50, 144),dm(2)
!
  write(iu,10,err=200)
!
!     forces
!     '  z' !63
  txt = txtmsgf(50, 63)
  ii = len_trim(txt)
!     '  force(N)' !56
!     '  x' !62
  write(iu,135,err=200)txtmsgf(50, 56),txtmsgf(50, 62),txt(1:ii)
!     '  weight' !141
  write(iu,145,err=200) txtmsgf(50, 141),-dm(2)*sf
!     z distributed force sum -> an
  an = disforf(.false.)
!     x distributed force sum -> df
  df = disforf(.true.)
!     '   dist.' !139
  write(iu,115,err=200) txtmsgf(50, 139),df,an
!
!     force on z direction, no slope
  dm(1) = dm(1)+an
!     sum concentrated forces x->an and z->dm2, see predad.f
  call confor(an,dm(2))
!     '   conc.' !140
  write(iu,115,err=200) txtmsgf(50, 140),an,dm(2)
!     '   sum' !144
  write(iu,115,err=200) txtmsgf(50, 144),an+df,dm(1)+dm(2)
!
  write(iu,10,err=200)
!
!     coordinates
  write(iu,15,err=200)txtmsgf(50, 20),cori(1),txtmsgf(50, 62),cori(2),txtmsgf(50, 63),cori(3),txtmsgf(50, 50)
!
  write(iu,10,err=200)
!
!     extremity
  call fltout(iu,txtmsgf(50, 19))
  write(iu,25,err=200) txtmsgf(50, 42),txtmsgf(50, 24),txtmsgf(50, 20)
  jj = 1
  write(iu,50,err=200) y(1),dl(jj),1
  write(iu,50,err=200) y(1),dl(jj+1),2
  write(iu,50,err=200) y(1),cxzf(dl,1,mtg),0
  jj = ifidxof(nt,1)
  write(iu,50,err=200) y(nt),dl(jj),1
  write(iu,50,err=200) y(nt),dl(jj+1),2
  write(iu,50,err=200) y(nt),cxzf(dl,nt,mtg),0
!
  write(iu,10,err=200)
!
!     maximum values
  call fltout(iu,txtmsgf(50, 151))
  write(iu,25,err=200) txtmsgf(50, 42),txtmsgf(50, 24),txtmsgf(50, 20)
!     max loop
  do ii = 1,nl
    write(iu,50,err=200) pl(ii),vl(ii),cl(ii)
  end do
!
  write(iu,10,err=200)
!
!     sections displacements and rotations
  call fltout(iu,txtmsgf(50, 58))
  write(iu,35,err=200) txtmsgf(50, 42),txtmsgf(50, 46),txtmsgf(50, 47),txtmsgf(50, 59),txtmsgf(50, 60)
!     sections loop
  do ii = 1,nt
    jj = ifidxof(ii,1)
    write(iu,45,err=200) y(ii),dl(jj),dl(jj+1),dl(jj+2),dl(jj+3)
  end do
!
  write(iu,10,err=200)
!
!     section displacements on plane
  txt = txtmsgf(50, 50)
  ii = len_trim(txt)
  write(iu,95,err=200) txt(1:ii)
  write(iu,75,err=200) txtmsgf(50, 42),txtmsgf(50, 67),txtmsgf(50, 66)
!     sections loop
  do ii = 1,nt
    jj = ifidxof(ii,1)
!       angle z and x -> an
    an = real(atan2(dl(jj+1),dl(jj)), lrk)
!       modulo -> v1
    v1 = cxzf(dl,ii,mtg)
    write(iu,85,err=200) y(ii),v1,an
  end do
!
  write(iu,10,err=200)
!
!     bending moment at section coordinates
  call fltout(iu,txtmsgf(50, 145))
!     y(m) x z
  write(iu,75,err=200) txtmsgf(50, 42),txtmsgf(50, 62),txtmsgf(50, 63)
!     max/min
  dm(1) = -1e15_lrk
  dm(2) = 1e15_lrk
  dm(3) = -1e15_lrk
  dm(4) = 1e15_lrk
  do ii = 1,4
    id(ii) = 0
  end do
!     sections loop
  do ii = 1,nt
    write(iu,85,err=200) y(ii),mi(ii,1),mi(ii,2)
!       max x
    if(mi(ii,1) .gt. dm(1)) then
      dm(1) = mi(ii,1)
      id(1) = ii
    end if
!       min x
    if(mi(ii,1) .lt. dm(2)) then
      dm(2) = mi(ii,1)
      id(2) = ii
    end if
!       max z
    if(mi(ii,2) .gt. dm(3)) then
      dm(3) = mi(ii,2)
      id(3) = ii
    end if
!       min z
    if(mi(ii,2) .lt. dm(4)) then
      dm(4) = mi(ii,2)
      id(4) = ii
    end if
  end do
!
  write(iu,10,err=200)
!     min/max
  call fltout(iu,txtmsgf(50, 149))
!     y,min/max,coordinate
  write(iu,25,err=200)txtmsgf(50, 42),txtmsgf(50, 149),txtmsgf(50, 20)
  if (id(2) .gt. 0) write(iu,50,err=200) y(id(2)),dm(2),1
  if (id(1) .gt. 0) write(iu,50,err=200) y(id(1)),dm(1),1
  if (id(4) .gt. 0) write(iu,50,err=200) y(id(4)),dm(4),2
  if (id(3) .gt. 0) write(iu,50,err=200) y(id(3)),dm(3),2
!
  write(iu,10,err=200)
!
!     bending stress
  call fltout(iu,txtmsgf(50, 174))
!     y (m)  stress (MPa)
  write(iu,195,err=200) txtmsgf(50, 42),txtmsgf(50, 159)
!     max/min
  dm(1) = -1e15_lrk
  dm(2) = 1e15_lrk
  id(1) = 0
  id(2) = 0
!     sections loop
  do ii = 1,nt
    write(iu,185,err=200) y(ii),bs(ii)
!       max x
    if(bs(ii) .gt. dm(1)) then
      dm(1) = bs(ii)
      id(1) = ii
    end if
!       min x
    if(bs(ii) .lt. dm(2)) then
      dm(2) = bs(ii)
      id(2) = ii
    end if
  end do
  write(iu,10,err=200)
!     min/max
  call fltout(iu,txtmsgf(50, 149))
!     y,min/max,coordinate
  write(iu,195,err=200) txtmsgf(50, 42),txtmsgf(50, 149)
  if (id(2) .gt. 0) write(iu,185,err=200) y(id(2)),dm(2)
  if (id(1) .gt. 0) write(iu,185,err=200) y(id(1)),dm(1)
!
  write(iu,10,err=200)
!
!     shear force constant at shear section coordinates ys
  call fltout(iu,txtmsgf(50, 147))
!     x
  txt = txtmsgf(50, 62)
  ii = len_trim(txt)
!     z
  tx1 = txtmsgf(50, 63)
  jj = len_trim(tx1)
!     x,z =
  txt = txt(1:ii)//cori(4)//tx1(1:jj+1)//cori(5)
  jj = len_trim(txt)
!     median filter width
  tx1 = txtmsgf(50, 148)
  ii = len_trim(tx1)
  write(iu,175,err=200) tx1(1:ii),txt(1:jj),isf(1),cori(4),isf(2)
!
!     y(m) x z
  write(iu,165,err=200)txtmsgf(50, 42),txtmsgf(50, 42),txtmsgf(50, 62),txtmsgf(50, 63)
!
!     max/min
  dm(1) = -1e15_lrk
  dm(2) = 1e15_lrk
  dm(3) = -1e15_lrk
  dm(4) = 1e15_lrk
  do ii = 1,4
    id(ii) = 0
  end do
!     sections loop
  do ii = 1,nt-1
    jj = (ii-1)*2+1
    write(iu,155,err=200) ys(jj),ys(jj+1),si(jj,1),si(jj,2)
    if(si(jj,1) .gt. dm(1)) then
      dm(1) = si(jj,1)
      id(1) = jj
    end if
    if(si(jj,1) .lt. dm(2)) then
      dm(2) = si(jj,1)
      id(2) = jj
    end if
    if(si(jj,2) .gt. dm(3)) then
      dm(3) = si(jj,2)
      id(3) = jj
    end if
    if(si(jj,2) .lt. dm(4)) then
      dm(4) = si(jj,2)
      id(4) = jj
    end if
  end do
!
  write(iu,10,err=200)
!     coordinate
  call fltout(iu,txtmsgf(50, 149))
!     y,min/max,coordinate
  write(iu,25,err=200) txtmsgf(50, 42),txtmsgf(50, 149),txtmsgf(50, 20)
  if (id(2) .gt. 0) write(iu,50,err=200) y(id(2)),dm(2),1
  if (id(1) .gt. 0) write(iu,50,err=200) y(id(1)),dm(1),1
  if (id(4) .gt. 0) write(iu,50,err=200) y(id(4)),dm(4),2
  if (id(3) .gt. 0) write(iu,50,err=200) y(id(3)),dm(3),2
!
  write(iu,10,err=200)
!
!     not std i/o
  if (.not. std) then
!       close output
    close(iu)
  else
    call marksec(sec,1)
  end if
!
!     plot output
  if (plt) then
!       check for standard i/o
    if (.not. std) then
!         regular plot output file -> ofn
      call mntfnm(-8,m,ofn)
    end if
!       shear size
    jj = 2*(nt-1)
    lst = rks .ne. 0
    call elin(nc,nt,jj,y,dl,bs,&
    &mi,ys,si,isf,rm,bst,mts,mtg,mxr,mxb,&
    &lst,std,ofn)
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
5 format(a10,11x,E18.10)
10 format()
15 format(a15,3(a,a10))
20 format(a,E18.10)
25 format(3a20)
35 format(5a18)
45 format(6E18.10)
55 format(a10,3a18,a)
50 format(2E18.10,i10)
65 format(i5,5x,4E18.10)
75 format(3a18)
85 format(3E18.10)
95 format(a)
105 format(a,E18.10,1x,a)
115 format(a10,2E18.10)
125 format(i5,5x,2E18.10)
135 format(a10,a18,a)
145 format(a10,18x,E18.10)
155 format(4E18.10)
165 format(4a18)
175 format(2a,i2,a,i2)
185 format(2E18.10)
195 format(a18,1x,a18)

!
end subroutine s_lin_el
!
!     ==================================================================
!>    @brief calculation summary output
!
!>    @param[in] telm torsion element type or zero if lateral
!>    @param[out] mdm basic matrix size, x2 for adjoint
!>    @param[out] tlen total sections length (m)
!>    @param[out] tsm total sections mass (kg)
!>    @param[out] tdm total disk mass (kg)
!>    @param[out] tcm total concentrated mass (kg)
!
subroutine s_summ(telm,mdm,tlen,tsm,tdm,tcm)
  use com_sec, only: n, y, nt, nn
  use rd_kinds, only: lrk
  implicit none
!
  integer :: telm, mdm
  real(lrk) :: tlen, tsm, tdm, tcm
!
!     locals
  integer :: nsp, smn
!     functions
  integer :: nsuppf, flexdisk_ndof_f
  real(lrk) :: sectmasf, dskmasf, conmasf, sumv_r
!
!     total number of sections
  integer :: mts
  parameter (mts = 999)
!     all sections (divisions)
!
  intrinsic :: int
!
!     total sections mass, see predad.f
  tsm = sectmasf(tlen)
!     total disk mass, see predad.f
  tdm = dskmasf()
!     total concentrated mass, see predad.f
  tcm = conmasf()
!
!     global matrices dimension
!
!     sum all divisions
  smn = int(sumv_r(n,mts,nt))
!
  mdm = 0
!     check lateral / torsion
  if (telm .eq. 0) then
!       global lateral matrices dimensions
    mdm = 4*(smn+1)+flexdisk_ndof_f()
  else
!       torsion matrix dimension
    if (telm .eq. 1) then
!         one node
      mdm = smn+1
    else if (telm .eq. 2) then
!         two node
      mdm = 2*smn+1
    end if
  end if
!
!     effective number of bearings with support
!     0 if no consider supports
  nsp = nsuppf()
!     augment pp * support matrix [S] 2x2
  mdm = mdm+(2*nsp)
!
  return
!
end subroutine s_summ
!
!     ==================================================================
!>    @brief bearing parameters output.
!>    table or equation coefficients.
!
!>    @param[in] eu output file unit number
!>    @param[in] kd return kind of options file
!>    @param[out] errmsg return error message
!>    @param[out] ok return flag. unsuccessful if <0.
!
subroutine beafile(eu,kd,errmsg,ok)
  use rd_textfun, only: femsgf, fomsgf
  use com_cfm, only: c11, c12, c21, c22, d11, d12, d21, d22, nl
  use com_cft, only: eph, eth
  use com_nbc, only: cp, scl, bc
  use com_pmk, only: nt, kmc, cmc, rmc
  use com_pmt, only: tph, tth
  use rd_kinds, only: lrk
  implicit none
!
!     arguments
  integer :: eu, ok
  character(len=1) :: kd
  character(len=99) :: errmsg
!
!     local
  integer :: i, j, k
  character(len=1) :: c1
  character(len=3) :: c3
  character(len=6) :: c6
  character(len=7) :: nmm
  character(len=13) :: c13
  character(len=24) :: c24
  character(len=33) :: c33
  character(len=255) :: ofn
  dimension c1(3),c3(10)
  parameter (c1 = (/'1','2',' '/),&
  &c3 = (/'kxx','kxz','kzx','kzz','cxx','cxz','czx','czz',&
  &'eph','eth'/),&
  &c6 = 'BEANBR',&
  &nmm = 'beafile', c13 ='BEARING FILES',&
  &c24 = 'TABLE:rpm 4xk 4xc [2xkt]',&
  &c33 = 'COEFF:y=a/x,a=sum(c(i)*x^i),i=0,2')
!
!     max number of parameters lines
  integer :: mpm, mxm
  parameter (mpm = 99,mxm = 9)
!
!     bearing parameters coefficients
!
!     additional rotational parameters - francisco - oct-15
!
!
  intrinsic :: len_trim
!
!description
!8
!c11 67.51E06 73.65E01 14.35E-02
!c12 -29.59E06 32.12E03 -39.54E-01
!c21 -22.42E07 55.79E03 -10.48E00
!c22 45.33E07 -23.28E04 38.31E00
!d11 10.57E05 -50.39E01 80.03E-03
!d12 -22.24E05 13.78E02 -23.27E-02
!d21 -22.53E05 13.96E02 -23.57E-02
!d22 10.14E06 -67.28E02 11.68E-01
!EOF
!
!description
!number of lines (-> 3)
!500.00 67.98E06 -15.50E06 -20.59E07 37.15E07 90.10E04 -18.28E05 -18.53E
!912.50 68.23E06 -38.40E05 -17.82E07 25.97E07 59.10E04 -94.30E04 -95.50E
!1325.00 69.03E06 83.96E05 -16.16E07 18.33E07 48.90E04 -65.30E04 -66.10E
!EOF
!
!     return init
  ok = -1
!
  call marksep(eu,0,c13)
!     equation
  if (kd .eq. c1(1)) then
    write(eu,5,err=200) c33
    do i = 1,cp
      write(eu,10,err=200) c6, bc(i)
      write(eu,20,err=200) nl(i),scl(i)
      write(eu,30,err=200) c3(1),(c11(i,j),j=1,3)
      write(eu,30,err=200) c3(2),(c12(i,j),j=1,3)
      write(eu,30,err=200) c3(3),(c21(i,j),j=1,3)
      write(eu,30,err=200) c3(4),(c22(i,j),j=1,3)
      write(eu,30,err=200) c3(5),(d11(i,j),j=1,3)
      write(eu,30,err=200) c3(6),(d12(i,j),j=1,3)
      write(eu,30,err=200) c3(7),(d21(i,j),j=1,3)
      write(eu,30,err=200) c3(8),(d22(i,j),j=1,3)
      if (nl(i) .gt. 8) then
        write(eu,30,err=200) c3(9),(eph(i,j),j=1,3)
        write(eu,30,err=200) c3(10),(eth(i,j),j=1,3)
      end if
    end do
    write(eu,15,err=200)
  else if (kd .eq. c1(2)) then
!       table
    write(eu,5,err=200) c24
    do i = 1,cp
      write(eu,10,err=200) c6,bc(i)
      write(eu,20,err=200)nt(i),scl(i)
      do j=1,nt(i)
        write(eu,25,err=200)&
        &rmc(i,j),(kmc(i,j,k),k=1,4),(cmc(i,j,k),k=1,4),&
        &tph(i,j),tth(i,j)
      end do
    end do
    write(eu,15,err=200)
  else
    errmsg = femsgf(99, nmm,6,9,0)
    return
  end if
!
  call marksep(eu,1,c1(3))
!
!     return ok
  ok = 0
!
  return
!
200 close(eu)
  inquire(unit=eu,name=ofn)
  errmsg = fomsgf(99, nmm,13,ofn,1)
  return
!
5 format(a)
10 format(a,f10.0)
15 format()
20 format(i9,1x,E9.3)
25 format(11(E14.6))
30 format(a4,3E14.6)
!
end subroutine beafile
!
!     ==================================================================
!>    @brief options file output.
!
!>    @param[in] eu output file unit number
!>    @param[in] kd return kind of options file
!>    @param[in] nomopf options file name
!>    @param[out] errmsg return error message
!>    @param[out] ok return flag. unsuccessful if <0.
!
subroutine parfile(eu,nomopf,kd,errmsg,ok)
  use rd_textfun, only: fomsgf, sjoinif, sjoinrf
  use com_bef, only: opf
  use com_cpbd, only: dmp, nit
  use com_knd, only: cknd, supr
  use com_lio, only: nro, lno
  use com_mdd2, only: imd, nsm
  use com_mfa, only: ma, rm, au
  use com_nbc, only: cp, scl, bc
  use rd_kinds, only: lrk
  implicit none
!
!     arguments
  integer :: eu, ok
  character(len=1) :: kd
  character(len=255) :: nomopf
  character(len=99) :: errmsg
!
!
!     locals
  integer :: i, j
  character(len=1) :: blank
  character(len=4) :: c4
  character(len=6) :: c6
  character(len=7) :: nmm
  character(len=11) :: dsc
  character(len=99) :: cbf
  character(len=255) :: ofn
  parameter (blank = ' ',c4 = '(i2)',c6 = '(f4.1)',&
  &nmm = 'parfile',dsc='OPTION FILE')
!
!     max bearings,max number of modes,search for commons
  integer :: mxm, xmd
  parameter (mxm = 9,xmd = 19)
!
!     Campbell crossing lines 1,2,0.5
!
!     kind of bearing parameters
!     table or coefficients
!
!     min amplification factor fator and modal rpm block
!     angle unit r -> radian, default degree
!
!     modes to show
!
!     bearings with varible parameters
!
!     bearing files
!
!     mask
!
!     intrinsic functions
  intrinsic :: len_trim, real
!
!     init return
  ok = -1
!     kind of options file
  kd = cknd
!
  call marksep(eu,0,dsc)
!
!     check if there is an options file name
  if (nomopf(1:1) .le. blank) then
    call marksep(eu,1,blank)
    ok = 0
    return
  end if
!
  i = len_trim(lno(1))
  write(eu,300,err=200) lno(1)(1:i)
!     join modes to show, see tmatfun
  cbf = sjoinif(99, imd,c4,nsm,xmd)
  i = len_trim(cbf)
  write(eu,35,err=200) cknd,cbf(1:i)
  i = len_trim(lno(2))
  write(eu,300,err=200) lno(2)(1:i)
!     join Campbell crossings, see tmatfun
  cbf = sjoinrf(99, dmp,c6,nit,mxm)
  i = len_trim(cbf)
  write(eu,5,err=200) cp,ma,rm,cbf(1:i)
  i = len_trim(lno(3))
  write(eu,300,err=200) lno(3)(1:i)
  do i = 1,cp
    j = len_trim(opf(i))
    write(eu,15,err=200) bc(i),opf(i)(1:j)
  end do
  write(eu,25,err=200)
  i = len_trim(lno(4))
  write(eu,350,err=200) lno(4)(1:i)
!
  call marksep(eu,1,blank)
!
!     return ok
  ok = 0
!
  return
!
200 inquire(unit=eu,name=ofn)
  errmsg = fomsgf(99, nmm,13,ofn,1)
  return
!
5 format(i2,8x,2f10.2,a,/)
15 format(f2.0,8x,a)
25 format(/)
35 format(1x,a,8x,a,/)
300 format(a)
350 format(a,/)
!
end subroutine parfile
!
!     ==================================================================
!>    @brief bearing support parameters output.
!
!>    @param[in] eu output file unit number
!>    @param[out] errmsg return an error message
!>    @param[out] ok return flag. unsuccessful if <0.
!
subroutine outsup(eu,errmsg,ok)
  use rd_textfun, only: fomsgf
  use com_sp1, only: ns, bn
  use com_sp2, only: sup, sps
  use com_sp3, only: kos
  use rd_kinds, only: lrk
  implicit none
!
!     arguments
  integer :: eu, ok
  character(len=99) :: errmsg
!
!     locals
  integer :: i
  character(len=1) :: blank
  character(len=6) :: nmm
  character(len=12) :: c12
  character(len=15) :: c15
  character(len=54) :: c54
  character(len=255) :: ofn
  dimension c54(2)
  parameter (blank = ' ',nmm = 'outsup',c12 = 'SUPPORT FILE',&
  &c15 = 'SUPPORT   SCALE', c54 = (/&
  &'BEANBR    KXX       KXZ       KZZ       KZX           '&
  &,'CXX       CXZ       CZZ       CZX       weight    TYPE'/))
!
!     max number of bearings
  integer :: mxm
  parameter (mxm = 9)
!
!     bearing support
!     number of parameter lines
!     number of barings
!     desc of support
!     added scale sps - francisco - nov-15
!
!     intrinsic functions
  intrinsic :: len_trim
!
!     init return
  ok = -1
!
  call marksep(eu,0,c12)
  write(eu,5,err=200) c15
  write(eu,15,err=200) ns,sps
  write(eu,25,err=200)
  write(eu,35,err=200) (c54(i),i=1,2)
  do i = 1,ns
    write(eu,45,err=200)&
    &bn(i),sup(i,1),sup(i,2),sup(i,4),sup(i,3),&
    &sup(i,5),sup(i,6),sup(i,8),sup(i,7),&
    &sup(i,9),kos(i)
  end do
  call marksep(eu,1,blank)
!
!     return ok
  ok = 0
!
  return
!
5 format(a)
15 format(i9,1x,e9.3)
25 format()
35 format(a50,a54)
45 format(i4,6x,9(e9.3,1x),a)
200 inquire(unit=eu,name=ofn)
  errmsg = fomsgf(99, nmm,13,ofn,1)
!
end subroutine outsup
!
!     ==================================================================
!>    @brief dump message files.
!
!>    @param[in] std standard input output
!>    @param[out] ok return flag. unsuccessful if <0.

subroutine s_dump(std,ok)
  implicit none
!
  integer :: ok
  logical :: std
!
!     locals
  integer :: j, iu
  character(len=5) :: cst
  character(len=10) :: sec
  parameter (cst = 'stdio',sec = 'msgdump')
  character(len=255) :: ofn
!
  ok = -1
!
!     file i/o
  if (.not. std) then
!       unit number
    iu = 12
!       get output file name -> ofn, size -> j
    call mntfnm(22,j,ofn)
!       open output file
    open(iu,file=ofn(1:j),err=100)
  else
!       standard i/o
    ofn = cst
    iu = 6
    call marksec(sec,0)
  end if
!
!     will set ok
  call dmpmsg(iu,ok)
!
!     not std i/o
  if (.not. std) then
!       close output
    close(iu)
  else
    call marksec(sec,1)
  end if
!
  return
!
100 return
!
end subroutine s_dump
!
!     ==================================================================
!>    @brief get a list of index and gyroscopic factor for sections.
!>     format: section index-gyroscopic factor, comma separated.
!>    @return list of index and gyroscopic factor for sections.
!>    @see gycscid, entrada
!
!
!     ==================================================================
!>    @brief general data output.
!
!>    @param[in] cm center of mass (m)
!>    @param[in] nomopf options file name
!>    @param[in] tors torsional calculation flag
!>    @param[in] sok true if support was read in line.
!>    @param[in] fto optional flexural-torsion data is present
!>    @param[in] iex export kind flag = 0 HB, 1=mm,2=plain
!>    @param[in] telm torsion element type
!>    @param[out] errmsg return an error message
!>    @param[out] ok return flag. unsuccessful if <0.
!
!     added tors  - francisco - mar-20.
subroutine saidas(cm,nomopf,tors,fto,sok,iex,telm,errmsg,ok)
  use rd_textfun, only: cadjf, ciff, fomsgf, pgycscidf, pinfo, sinfo, txtmsgf
  use com_cab, only: name, descript
  use com_conc, only: nmic, psic, vlmc, ixic, iyic, izic
  use com_copcm, only: ops
  use com_cpb, only: nini, nfin, dw, npi, ncc, imt
  use com_cpba, only: nrdc, rgini, rgrpm
  use com_cpbd, only: dmp, nit
  use com_cpbn, only: spdn
  use com_dflexi, only: dmodel, dfdofx, dfdofz, nfdisk, nshaftfd
  use com_dflexr, only: dkr, df1nd
  use com_dis, only: pd, d_d, h_d, rho_d, r2, nd
  use com_disa, only: d_i, i_x, i_y, m_d
  use com_doff, only: off_d
  use com_eix, only: l, d, di, ps, e, nu, rho_e, g_e, r1, cs
  use com_eix2, only: ys
  use com_eixa, only: divs, umps
  use com_fixstif, only: fxstf
  use com_hang, only: hangle, accg, isf, ha
  use com_inf, only: vrs, dta, rgh, sid
  use com_lid, only: nre, lne
  use com_lif, only: usr, dtu, tmu
  use com_man, only: kxx, kxz, kzz, kzx, cxx, cxz, czz, czx, mm, scl => sc
  use com_mdd, only: qtd_modos, porb, rorb, rang
  use com_mdd1, only: ru
  use com_mfa, only: ma, rm, au
  use com_ppr, only: opt, ifn
  use com_pse, only: v_maior, v_menor
  use com_psea, only: ld_r
  use com_sp1, only: ns, bn
  use com_sp2, only: sup, sps
  use com_sp3, only: kos
  use com_sta, only: stm
  use com_thmfr, only: thfr
  use com_tman, only: ntbr, tpc, tkk, tcc, tjj, sct
  use com_tor, only: kph, kth
  use com_unb, only: nini_r, nfin_r, dw_r
  use com_unb0, only: pr, desp, ori, np, nm
  use com_unb1, only: ndd, mu, ed, tpf, nb
  use com_ymc, only: nbrg, rks, pc
  use rd_kinds, only: lrk
  implicit none
!
!     arguments
  real(lrk) :: cm
  integer :: iex, telm, ok
  character(len=99) :: errmsg
  character(len=255) :: nomopf
  logical :: tors, fto, sok
!
!     locals
  real(lrk) :: sc
  integer :: i, j, k, iu, iiff
  character(len=1) :: kd, crd, blank, cdg
  character(len=3) :: cau, cdr
  character(len=5) :: cst
  character(len=6) :: nmm, cio
  character(len=8) :: can
  character(len=10) :: sec
  character(len=50) :: txt
  character(len=255) :: ofn
  dimension cdg(10),cio(2),cdr(2),cau(2),can(2)
  parameter(nmm = 'saidas',sec = 'output',blank = ' ',&
  &cst = 'stdio',cio = (/'INPUT ','OUTPUT'/),&
  &can = (/' lateral',' torsion'/),&
  &cdg = (/'r','R','d','D','<','>','1','2','x',','/),&
  &cdr = (/'rad','deg'/))
!
!     summary
  integer :: mdm
  real(lrk) :: tlen, tsm, tdm, tcm
!
!     parameters
  integer :: mxs, mxd, mxm
  parameter (mxs = 99,mxd = 99,mxm = 9)
!
!     unbalance
  integer :: mxb, mxp
  parameter (mxb = 99,mxp = 9)
!
!     shaft data
!     umbalanced magnetic pull
!     yield strength
!
!     section parameters
!     added francisco - feb-19
!     horizontal angle and gravity
!     angle unit r -> radian, default degree
!     bending stress median filter width
!
!     bearings
!     scale, sc added francisco - oct-15
!     added 13/04/2007
!     fixed stiffnes for no displacement
!     addeed - francisco - oct-15
!     torsion restriction bearings, added mar-21
!
!     disks
!     added - franciso - feb-19
!     flexible disk reduced model
!
!     campbell
!     nominal speed (rpm), added francisco oct-20
!     Campbell crossing lines 1,2,0.5
!
!     unbalance, ori -> rad
!     force kind
!      -1:transient torque,0:unbalance (default),1:concentrated,
!      2:harmonic torque,3:static torque,4:freq. response,5:dist. force
!     min amplification factor fator and modal rpm block
!     angle unit r -> radian, default degree
!     torsion harmonic excitation frequency  (rad/s)
!     or force offset for kind = 1 (m)
!
!     modes
!     time orbit position
!     response angle unit 'd' or 'D' for degree, default radian
!
!     concentrated masses ant inertias
  integer :: mxic
  parameter (mxic = 15)
!
!     bearing suport
!     number of parameters lines
!     bearing number
!     description of support
!     added scale sps - francisco - nov-15
!
!     header
!
!     user id
!
!     system id
!
!     parameters
  integer :: mop
  parameter (mop = 20)
!
!     input mask (see init.f)
  integer :: ll(18)
!
!     stamp
!
!     calculation options
!
!     intrinsic functions
  intrinsic :: len_trim
!
!     return init
  ok = -1

!     file i/o
  if (.not. opt(11)) then
!       unit number
    iu = 13
!       get output file name -> ofn, size -> j
    call mntfnm(4,j,ofn)
!       open output file
    open(iu,file=ofn(1:j),err = 100)
  else
!       standard i/o
    ofn = cst
    iu = 6
    call marksec(sec,0)
  end if
!
!     program headers
  write(iu,300,err=200) stm
  call outdsc(iu)
!     added revision - francisco - feb-19
  txt = sinfo(26, .false.,0)
  i = len_trim(txt)
  write(iu,300,err=200) txt(1:i)
  txt = pinfo(42, .false.)
  i = len_trim(txt)
  write(iu,300,err=200) txt(1:i)
  write(iu,250,err=200)
!
!     user
  i = len_trim(usr)
  txt = txtmsgf(50, 68)
  k = len_trim(txt)
  write(iu,350,err=200) txt(1:k),cdg(5),usr(1:i),cdg(6)
!
!     input file name
  i = len_trim(ifn)
  txt = txtmsgf(50, 69)
  k = len_trim(txt)
  write(iu,350,err=200) txt(1:k),cdg(5),ifn(1:i),cdg(6)
!
!     calculation options
  txt = txtmsgf(50, 70)
  k = len_trim(txt)
  write(iu,250,err=200)
  write(iu,300,err=200) txt(1:k)
!
!     see init.f, rotordin.f entrada.f messages.f
!
!     opt(1) cpb campbell
!     opt(2) mds modes
!     opt(3) fqr frequency resp.
!     opt(4) tmr resp. time
!     opt(5) fng input file set
!     opt(6) exp geometry file
!     opt(8) static elastic line
!     opt(9) undamped critical speed map
!     opt(10) bearing support
!     opt(11) standard i/o
!     opt(12) none
!     opt(13) none
!     opt(14) output HPGL plot -p
!     opt(15) undamped angular critical speed map
!     opt(16) variable speed bearing parameters plot
!     opt(17) override messages -o
!     opt(18) torsion option
!     opt(19) torsion time transient
!     opt(20) flexural-torsion
!     iex export kind flag = 0 HB, 1=mm,2=plain
!
!1    71 -> ' Campbell diagram'
!2    72 -> ' mode/speed shapes'
!3    73 -> ' unbalance response'
!4    74 -> ' time response'
!5     ' '
!6    75 -> ' geometry export'
!7    76 -> ' with speed dep. bearing params'
!8    77 -> ' static elastic line'
!9    78 -> ' undamped critical speed map'
!10   79 -> ' with bearing support params'
!11    ' '
!12   80 -> ' export matrix HB'
!13   81 -> ' export matrix MM'
!14   82 -> ' HPGL plot output'
!15   83 -> ' undamped angular critical speed map'
!16   84 -> ' speed dep.bearing params plot'
!19   120-> ' Torsion Time Transient Response'
!20   154-> ' Flexural - Torsion Analysis'
!
  do i = 1,mop
!       i = option index
    if(opt(i) .and. i .ne. 5 .and. i .ne. 11&
    &.and. i .ne. 12 .and. i .ne. 13 .and. i .ne. 18) then
      if (i .lt. 5) then
        k = i
      else if (i .lt. 11) then
        k = i-1
      else if (i .eq. 19) then
        k = 50
      else if (i .eq. 20) then
        k = 84
      else
        k = i-2
      end if
!         see text messages
      txt = txtmsgf(50, 70+k)
      k = len_trim(txt)
      write(iu,300,err=200) txt(1:k)
    end if
  end do
!     export kind
  if (.not. iex .lt. 0) then
!       80 -> ' export matrix HB'
!       81 -> ' export matrix MM'
!       176 -> ' export matrix kind'
    if (iex .eq. 0) txt = txtmsgf(50, 80)
    if (iex .eq. 1) txt = txtmsgf(50, 81)
    if (iex .eq. 2) txt = txtmsgf(50, 176)
    k = len_trim(txt)
    write(iu,300,err=200) txt(1:k)
  end if
!
!     main input file data
  call marksep(iu,0,cio(1))
!
!     calculates length mask lines
  do i = 1,nre
    ll(i) = len_trim(lne(i))
  end do
!
!     user name
  write(iu,300,err=200) lne(1)(1:ll(1))
  i = len_trim(name)
  write(iu,300,err=200) name(1:i)
  write(iu,250,err=200)
!
!     description
  write(iu,300,err=200) lne(2)(1:ll(2))
  i = len_trim(descript)
  write(iu,300,err=200) descript(1:i)
  write(iu,250,err=200)
!
!     sections
!     angle unit, default degree
  crd = cadjf(1, ha,1)
  cau(1) = ciff(3, crd .eq. cdg(1) .or. crd .eq. cdg(2),cdr(1),cdr(2))
!     added angle and g - francisco - feb-19
  write(iu,300,err=200) lne(3)(1:ll(3))
  write(iu,5,err=200)&
  &cs,l,v_menor,v_maior,ld_r,hangle,accg,cau(1),&
  &isf(1),cdg(10),isf(2)
  write(iu,250,err=200)
!
!     shaft
  write(iu,300,err=200) lne(4)(1:ll(4))
  do i = 1,cs
    write(iu,15,err=200)&
    &ps(i),d(i),di(i),e(i),nu(i),rho_e(i),divs(i),umps(i),ys(i)
  end do
  write(iu,250,err=200)
!
!     disk
  txt = pgycscidf(50)
  k = len_trim(txt)
  write(iu,300,err=200) lne(5)(1:ll(5))
  write(iu,25,err=200) nd,spdn,txt(1:k)
  write(iu,250,err=200)
  write(iu,300,err=200) lne(6)(1:ll(6))
  do i = 1,nd
!       Preserve legacy rigid disk output exactly. Flexible disks append
!       a compact OFFSET FLEX KR form accepted by entrada.f.
    if (dmodel(i) .eq. 0) then
      write(iu,35,err=200)&
      &pd(i),d_d(i),h_d(i),rho_d(i),&
      &d_i(i),i_x(i),i_y(i),m_d(i),off_d(i)
    else
      write(iu,36,err=200)&
      &pd(i),d_d(i),h_d(i),rho_d(i),&
      &d_i(i),i_x(i),i_y(i),m_d(i),off_d(i),'FLEX',dkr(i)
    endif
  end do
  write(iu,250,err=200)
!
!     bearings
!     scale same for both
  if (nbrg .gt. 0) then
!       lateral bearing
    sc = scl
  else
!       torsion restriction
    sc = sct
  end if
  write(iu,300,err=200) lne(7)(1:ll(7))
  write(iu,50,err=200) nbrg,ntbr,sc,rks,fxstf
  write(iu,250,err=200)
  write(iu,300,err=200) lne(8)(1:ll(8))
!     lateral bearing
  do i = 1,nbrg
    write(iu,55,err=200)&
    &pc(i),kxx(i),kxz(i),kzz(i),kzx(i),&
    &cxx(i),cxz(i),czz(i),czx(i),mm(i),&
    &kph(i),kth(i)
  end do
!     torsion restriction
  do i = 1,ntbr
    write(iu,60,err=200) tpc(i),tkk(i),tcc(i),-tjj(i)
  end do
  write(iu,250,err=200)
!
!     Campbell
  write(iu,300,err=200) lne(9)(1:ll(9))
  write(iu,65,err=200)&
  &nini,nfin,dw,npi,ncc,rgini,nrdc,rgrpm,imt
  write(iu,250,err=200)
!
!     unbalance
!     angle unit, default degree
  crd = cadjf(1, au,1)
  cau(1) = ciff(3, crd .eq. cdg(1) .or. crd .eq. cdg(2),cdr(1),cdr(2))
  write(iu,300,err=200) lne(10)(1:ll(10))
!     on entrada, number of modes x 2
  write(iu,75,err=200)&
  &nb,nini_r,nfin_r,dw_r,nm/2,ma,rm,cau(1)
  write(iu,250,err=200)
  write(iu,300,err=200) lne(11)(1:ll(11))
!     added torsion harmonic exc.freq. (rad/s) - francisco sep-20
  do i = 1,nb
    write(iu,85,err=200)&
    &ndd(i),mu(i),ed(i),tpf(i),thfr(i)
  end do
  write(iu,250,err=200)
!
!     response
!     angle unit, default rads.
  crd = cadjf(1, ru(1),1)
  cau(1) = ciff(3, crd .eq. cdg(3) .or. crd .eq. cdg(4),cdr(2),cdr(1))
  crd = cadjf(1, ru(2),1)
  cau(2) = ciff(3, crd .eq. cdg(3) .or. crd .eq. cdg(4),cdr(2),cdr(1))
  write(iu,300,err=200)lne(12)(1:ll(12))
  write(iu,95,err=200)&
  &np,qtd_modos,porb,rorb,rang,cau(1),cau(2)
  write(iu,250,err=200)
  write(iu,300,err=200) lne(13)(1:ll(13))
  do i = 1,np
    write(iu,105,err=200)pr(i),desp(i),ori(i)
  end do
  write(iu,250,err=200)
!
!     options
  write(iu,300,err=200) lne(14)(1:ll(14))
!     options file name
  if(nomopf(1:1) .gt. blank) then
    i = len_trim(nomopf)
    write(iu,300,err=200) nomopf(1:i)
  else
    write(iu,300,err=200) ops
  end if
  write(iu,250,err=200)
!
!     concentrated
  write(iu,300,err=200) lne(15)(1:ll(15))
  write(iu,125,err=200) nmic
  write(iu,250,err=200)
  write(iu,300,err=200) lne(16)(1:ll(16))
  do i = 1,nmic
    write(iu,135,err=200)&
    &psic(i),vlmc(i),ixic(i),iyic(i),izic(i)
  end do
!
!     support in line
  if (sok) then
    write(iu,250,err=200)
    write(iu,300,err=200) lne(17)(1:ll(17))
    write(iu,45,err=200) ns,sps
    write(iu,250,err=200)
    write(iu,300,err=200) lne(18)(1:ll(18))
    do i = 1,ns
!         #nbr,kxx,kxz,kzz,kzx,cxx,cxz,czz,czx,mass,desc
      write(iu,115,err=200)&
      &bn(i),sup(i,1),sup(i,2),sup(i,4),sup(i,3),&
      &sup(i,5),sup(i,6),sup(i,8),sup(i,7),&
      &sup(i,9),kos(i)
    end do
  end if
!
  call marksep(iu,1,blank)
!
!     added torsion flag - francisco - mar-20
  if (.not. tors) then
!       lateral option files
    call parfile(iu,nomopf,kd,errmsg,ok)
    if (ok .ne. 0) then
      close(iu)
      return
    end if
!       bearing parameters
    if (kd .eq. cdg(7) .or. kd .eq. cdg(8)) then
      call beafile(iu,kd,errmsg,ok)
      if (ok .ne. 0) then
        close(iu)
        return
      end if
    end if
!
  else
!       torsion option file, see tsaidas.f
    call tparfile(iu,nomopf,kd,fto,errmsg,ok)
    if (ok .ne. 0) then
      close(iu)
      return
    end if
  end if
!
!     bearing support data, if not in line
  if (opt(10) .and. .not. sok) then
    call outsup(iu,errmsg,ok)
    if (ok .ne. 0) then
      close(iu)
      return
    end if
  end if
!
  call marksep(iu,0,cio(2))
!
!     k = kind of element if torsion, 0 instead
  k = iiff(tors,telm,0)
!     calculation summary - francisco - feb-19
!     added kind of torsion element, zero if lateral - francisco - nov-2
  call s_summ(k,mdm,tlen,tsm,tdm,tcm)
!
!     ' calculation summary ' !85
  txt = txtmsgf(50, 85)
  k = len_trim(txt)
  if (tors) then
    write(iu,325,err=200) can(2),txt(1:k)
  else
    write(iu,325,err=200) can(1),txt(1:k)
  end if
  txt = txtmsgf(50, 87)
  k = len_trim(txt)
  write(iu,145,err=200) txt(1:k),2*mdm,cdg(9),2*mdm
  txt = txtmsgf(50, 86)
  k = len_trim(txt)
  write(iu,155,err=200) txt(1:k),cs
  txt = txtmsgf(50, 88)
  k = len_trim(txt)
  write(iu,400,err=200) txt(1:k),tsm
  txt = txtmsgf(50, 89)
  k = len_trim(txt)
  write(iu,400,err=200) txt(1:k),tdm
  txt = txtmsgf(50, 90)
  k = len_trim(txt)
  write(iu,400,err=200) txt(1:k),tcm
  txt = txtmsgf(50, 91)
  k = len_trim(txt)
  write(iu,400,err=200) txt(1:k),tsm+tdm+tcm
  txt = txtmsgf(50, 92)
  k = len_trim(txt)
  write(iu,400,err=200)txt(1:k),tlen
  txt = txtmsgf(50, 93)
  k = len_trim(txt)
  write(iu,400,err=200) txt(1:k),cm
  call marksep(iu,1,blank)
!
!     not standard i/o
  if (.not. opt(11)) then
!       close output file
    close(iu)
  else
    call marksec(sec,1)
  end if
!
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
5 format(1(i9,1x),(e9.3,1x),2(i9,1x),3(f9.4,1x),a9,1x,i2,a,i2)
15 format(6(e9.3,1x),1(i9,1x),2(e9.3,1x))
25 format(1(i9,1x),f9.1,1x,a)
35 format(9(e9.3,1x))
36 format(9(e9.3,1x),a4,1x,e10.3)
45 format(1(i9,1x,e9.3,1x,f9.2))
50 format(1(i2,' +',i2,4x,e9.3,1x,f9.2,e9.3))
55 format(12(e9.3,1x))
60 format(2(e9.3,1x),30x,e9.3,31x,e9.3)
65 format(3(e9.3,1x),2(i9,1x),1(e9.3,1x),1(i9,1x),&
  &1(e9.3,1x),1(i9,1x))
75 format(1(i9,1x),3(e9.3,1x),i9,1x,2(e9.3,1x),a)
85 format(3(e9.3,1x),i9,1x,e9.3)
95 format(2(i9,1x),3(e9.3,1x),2(1x,a9))
105 format(3(e9.3,1x))
115 format(1(i9,1x),9(e9.3,1x),a)
125 format(1(i9,1x))
135 format(5(e9.3,1x))
145 format(/,a,i4,a,i4)
155 format(a,i4)
250 format()
300 format(a)
325 format(2a)
350 format(4a)
400 format(a,e12.5)
!
end subroutine saidas
!
!     ==================================================================
!>    @brief export modal parameters matrices PHI, PSI and LAMBDA,
!>    the matrices must be ready. The complex matrices will be exported
!>    in real and imaginary parts.
!
!>    @param[in] job zero if HB, matrix market if 1, kind if 2.
!>    @param[in] rpm speed to calculate the modal space
!>    @param[in] std standard input output
!>    @param[out] errmsg error message
!>    @param[out] ok error flag
!
subroutine export(job,rpm,std,errmsg,ok)
  use rd_textfun, only: fmmsgf, fomsgf
  use com_epm, only: fi, fn, avl, ddm
  use com_epm1, only: psi
  use com_nmi, only: inm, sbv
  use rd_kinds, only: lrk, wp
  implicit none
!
  real(lrk) :: rpm
  integer :: job, ok
  character(len=99) :: errmsg
  logical :: std
!
!     dimensions
  integer :: mtg
  parameter (mtg = 500)
!     modal space
  integer :: mte
  parameter (mte = 2*mtg)
!
!
!     internal name
!
!     locals
  integer :: i, iu, m, ierr
  real(wp) :: a
  complex(wp) :: lam
  character(len=1) :: blank
  character(len=2) :: nm, nms
  character(len=3) :: csl
  character(len=5) :: cfn, crp
  character(len=6) :: nmm, secs
  character(len=8) :: key, keys
  character(len=10) :: sec
  character(len=72) :: title
  character(len=255) :: ofn
  dimension nms(3),csl(2),secs(6),keys(6),&
  &a(mte,mte),lam(mte,mte)
  parameter (blank = ' ',nms = (/'HB','mm','pl'/),&
  &cfn = 'stdio',csl=(/' - ',' @ '/),&
  &crp = '(rpm)',secs = (/'phi-r_','phi-i_','psi-r_',&
  &'psi-i_','lam-r_','lam-i_'/),&
  &keys = (/'PHI-REAL','PHI-IMAG','PSI-REAL',&
  &'PSI-IMAG','LAM-REAL','LAM-IMAG'/),nmm = 'export')
!
  intrinsic :: len_trim
!
!     return
  ok = -1
  ierr = 0
!
!     check job, Harwell-Boeing, Matrix Market or kind
  if (job .lt. 0 .or. job .gt. 2) then
!       'export:invalid job id' !14
    errmsg = fmmsgf(99, nmm,14,blank,0)
    return
  end if
!
!     Complete usable modal basis, not a raw diagnostic dump.
!     Validate all columns before opening any of the six output files.
!     Preserve dimensions and indices; never omit invalid columns.
  call modal_require_range(ddm,avl,mte,errmsg,ierr)
  if (ierr .ne. 0) return
!
!     internal application name
  i = len_trim(inm)
!
  write(title,5) inm(1:i),csl(2),rpm,crp
!     name suffix
  if (job .eq. 0) then
    nm = nms(1)
  else if (job .eq. 1) then
    nm = nms(2)
  else if (job .eq. 2) then
    nm = nms(3)
  end if
!
!     matrix export PHIr,PSIr,LAMr,PHIi,PSIi,LAMi
!     output
  if (.not. std) then
    iu = 12
  else
    iu = 6
    ofn = cfn
  end if
!
!     1) PHI real -> a
!
  call cplxmat_d(a,fi,0,ddm,ddm,mte,mte)
  if (.not. std) then
!       file name -> ofn
    call mntfnm(11,m,ofn)
    open(iu,file=ofn(1:m),err=100)
  else
    write(sec,150) secs(1),nm
    call marksec(sec,0)
  end if
  if (job .eq. 0) then
!       Harwell/Boeing
    key = keys(1)
    call dwritehb(title,key,iu,ddm,ddm,a,mte,ierr)
  else
    write(title,200) inm(1:i),csl(1),keys(1)
    if (job .eq. 1) then
!         MatrixMarket
      call dwritemm(title,iu,ddm,ddm,a,mte,ierr)
    else
!         plain
      call dwritepl(title,iu,ddm,a,mte,ierr)
    end if
  end if
!     close PHI real
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
!     2) PHI imag -> a
!
  call cplxmat_d(a,fi,1,ddm,ddm,mte,mte)
  if (.not. std) then
    call mntfnm(12,m,ofn)
    open(iu,file=ofn(1:m),err=100)
  else
    write(sec,150) secs(2),nm
    call marksec(sec,0)
  end if
  if (job .eq. 0) then
    key = keys(2)
    call dwritehb(title,key,iu,ddm,ddm,a,mte,ierr)
  else
    write(title,200) inm(1:i),csl(1),keys(2)
    if (job .eq. 1) then
      call dwritemm(title,iu,ddm,ddm,a,mte,ierr)
    else
      call dwritepl(title,iu,ddm,a,mte,ierr)
    end if
  end if
!     close PHI imag
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
!     3) PSI real -> a
!
  call cplxmat_d(a,psi,0,ddm,ddm,mte,mte)
  if (.not. std) then
    call mntfnm(13,m,ofn)
    open(iu,file=ofn(1:m),err=100)
  else
    write(sec,150) secs(3),nm
    call marksec(sec,0)
  end if
  if (job .eq. 0) then
    key = keys(3)
    call dwritehb(title,key,iu,ddm,ddm,a,mte,ierr)
  else
    write(title,200) inm(1:i),csl(1),keys(3)
    if (job .eq. 1) then
      call dwritemm(title,iu,ddm,ddm,a,mte,ierr)
    else
      call dwritepl(title,iu,ddm,a,mte,ierr)
    end if
  end if
!     close PSI real
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
!     4) PSI imag -> a
!
  call cplxmat_d(a,psi,1,ddm,ddm,mte,mte)
  if (.not. std) then
    call mntfnm(14,m,ofn)
    open(iu,file=ofn(1:m),err=100)
  else
    write(sec,150) secs(4),nm
    call marksec(sec,0)
  end if
  if (job .eq. 0) then
    key = keys(4)
    call dwritehb(title,key,iu,ddm,ddm,a,mte,ierr)
  else
    write(title,200) inm(1:i),csl(1),keys(4)
    if (job .eq. 1) then
      call dwritemm(title,iu,ddm,ddm,a,mte,ierr)
    else
      call dwritepl(title,iu,ddm,a,mte,ierr)
    end if
  end if
!     close PSI imag
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
!     get diagonal lambda eigenvalues matrix
  call matdia_c(avl,lam,ddm,mte,mte)
!
!     5) LAM real -> a
!
  call cplxmat_d(a,lam,0,ddm,ddm,mte,mte)
  if (.not. std) then
    call mntfnm(15,m,ofn)
    open(iu,file=ofn(1:m),err=100)
  else
    write(sec,150) secs(5),nm
    call marksec(sec,0)
  end if
  if (job .eq. 0) then
    key = keys(5)
    call dwritehb(title,key,iu,ddm,ddm,a,mte,ierr)
  else
    write(title,200) inm(1:i),csl(1),keys(5)
    if (job .eq. 1) then
      call dwritemm(title,iu,ddm,ddm,a,mte,ierr)
    else
      call dwritepl(title,iu,ddm,a,mte,ierr)
    end if
  end if
!     close LAM real
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
!     6) LAM imag -> a
!
  call cplxmat_d(a,lam,1,ddm,ddm,mte,mte)
  if (.not. std) then
    call mntfnm(16,m,ofn)
    open(iu,file=ofn(1:m),err = 100)
  else
    write(sec,150) secs(6),nm
    call marksec(sec,0)
  end if
  if (job .eq. 0) then
    key = keys(6)
    call dwritehb(title,key,iu,ddm,ddm,a,mte,ierr)
  else
    write(title,200) inm(1:i),csl(1),keys(6)
    if (job .eq. 1) then
      call dwritemm(title,iu,ddm,ddm,a,mte,ierr)
    else
      call dwritepl(title,iu,ddm,a,mte,ierr)
    end if
  end if
!     close LAM imag
  if (.not. std) then
    close(iu)
  else
    call marksec(sec,1)
  end if
!
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
5 format(2a,f6.0,1x,a)
150 format(2a)
200 format(3a)
!
end subroutine export
!
!     ==================================================================
!>    @brief append the spent time.
!
!>    @param[in] rt exec time
!>    @param[in] std standard input output
!>    @param[out] errmsg return an error message
!>    @param[out] ok return flag. unsuccessful if <0.
!
subroutine petim(rt,std,errmsg,ok)
  use rd_textfun, only: fomsgf, txtmsgf
  implicit none
!
!     locals
  integer :: iu, i, j, k
  character(len=5) :: nmm, cfn
  character(len=10) :: sec
  parameter(nmm = 'petim',cfn = 'stdio',sec = 'exectime')
  character(len=50) :: txt
  character(len=255) :: ofn
  logical :: std
!
!     arguments
  character(len=6) :: rt
  integer :: ok
  character(len=99) :: errmsg
!
  intrinsic :: len_trim
!
!     init return
  ok = -1

  if (.not. std) then
!       file output
    iu = 12
!       file name -> ofn
    call mntfnm(4,j,ofn)
!       opens the output file
    open(iu,file = ofn(1:j),err=100)
!       count number of lines
    k = 0
    i = 0
    do while (i .eq. 0)
      k = k+1
      read(iu,5,iostat=i) txt
    end do
!       back to start
    rewind(iu)
!       read up to last line
    do i =1, k-1
      read(iu,5) txt
    end do
  else
!       standard output
    iu = 6
    ofn = cfn
    call marksec(sec,0)
  end if
!
  txt = txtmsgf(50, 94)
  i = len_trim(txt)
  write(iu,15,err=200) txt(1:i),rt
!
  if (.not. std) then
    close(iu)
  else
    call marksec(sec,1)
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
15 format(2a)
!
end subroutine petim
!
!     ==================================================================
!>    @brief should get the elapsed time, using <b>etime</b>.
!
!>    @return the elapsed time in seconds since started
!
!
!     ==================================================================
!>    @brief variable speed bearing parameters.
!
!>    @param[in] pv bearing speed dependent param flag
!>    @param[in] std standard i/o flag
!>    @param[in] plt HPGL plot output
!>    @param[out] errmsg return an error message
!>    @param[out] ok return flag. unsuccessful if <0.

subroutine sbeapar(pv,std,plt,errmsg,ok)
  use rd_textfun, only: fomsgf, txtmsgf
  use com_cpb, only: nini, nfin, dw, npi, ncc, imt
  use com_cpbn, only: spdn
  use com_knd, only: knd => cknd, supr
  use com_nbc, only: cp, scl, bc
  use com_sta, only: stm
  use com_unb, only: nini_r, nfin_r, dw_r
  use com_ymc, only: nbrg, rks, pc
  use rd_kinds, only: lrk
  implicit none
!
  logical :: pv, plt, std
  character(len=99) :: errmsg
  integer :: ok
!
  integer :: i, iu, j, k, m, l, mx1, mx2, nbr, np, oki
  real(lrk) :: dr, ni, nf, pa, par, rad, rpm, rpmi, rpmf, sp, rpm2radf
  character(len=1) :: blank, c1
  character(len=3) :: c3
  character(len=4) :: c4
  character(len=5) :: c5, ckd
  character(len=7) :: c7, nmm
  character(len=10) :: sec
  character(len=50) :: txt
  character(len=255) :: ofn
  dimension c1(3),c3(8),c5(4)
  parameter (np = 200,&
  &blank = ' ',nmm = 'sbeapar',sec = 'beapars',&
  &c1 = (/'1','2',':'/),&
!     see beap on hplots.f
  &c3 = (/'Khh','Khv','Kvh','Kvv','Chh','Chv','Cvh','Cvv'/),&
  &c4 = 'kind',&
  &c5 = (/'stdio','coeff','table','unknw'/),&
  &c7 = 'bea.nbr')
!
!     bearings
  integer :: mxm
  parameter (mxm = 9)
!     variable speed bearing parameters
!     kind of bearing parameters
!     table or coefficients
!
!     campbell
!     nominal speed (rpm), added francisco oct-20
!
!     unbalance response
!
!     stamp
!
  parameter (mx1 = (np+1)*mxm*8,mx2 = np+1)
!     if spdn > 0 last point (np+1) is rated
  dimension par(mxm,10),pa(np+1,mxm,8),sp(np+1)
!
  data pa/mx1*0/,sp/mx2*0/
!
  intrinsic :: nint, min, max, len_trim, real
!
!     initialize return
  ok = -1
!
!     check number of variable bearings.
  if (cp .le. 0) then
!       'no suitable bearing' !34
    errmsg = fomsgf(99, nmm,34,blank,1)
    return
  end if
  call progress_begin('BEARING_PARAMETERS',cp*np,&
  &'evaluate speed-dependent bearing coefficients')
!
  if (.not. std) then
    iu = 12
!       output file name -> ofn
    call mntfnm(18,m,ofn)
!       open output file
    open(iu,file=ofn(1:m),err=100)
  else
    iu = 6
    ofn = c5(1)
    call marksec(sec,0)
  end if
!
!     stamp
  write(iu,15,err=200) stm
  call outdsc(iu)
!     bearing parameters
  call fltout(iu,txtmsgf(50, 153))
!     new line
  write(iu,5,err=200)
!     kind of paramters
  if (knd .eq. c1(1)) then
!       coefficients
    ckd = c5(2)
  else if (knd .eq. c1(2)) then
!       table
    ckd = c5(3)
  else
!       unknown
    ckd = c5(4)
  end if
  write(iu,35,err=200) c4,c1(3),ckd
  write(iu,5,err=200)
!
!     initial speed
  ni = min(nini,nini_r)
  nf = max(nfin,nfin_r)
!
!     speed limits on variable parameters, see parmanv.f -> rpmi,rpmf
  call lmvpar(pv,ni,nf,rpmi,rpmf)
!
  l = 0
!     speed increment
  dr = (rpmf-rpmi)/real(np-1, lrk)
!     bearing number loop
  do k = 1,nbrg
!       loop param bearings
    do m =1,cp
!         check bearing number
      if (nint(bc(m)) .eq.k) then
        l = l+1
!           speed loop
        rpm = rpmi
        do i = 1,np
!             rpm -> rad/s
          rad = rpm2radf(rpm)
!             variable speed bearing parameter -> par
          call parman(rad,.true.,mxm,nbr,par,errmsg,oki)
          if (oki .lt. 0) then
            return
          end if
!             stiffness and damping
!             k11 k12 k21 k22 c11 c12 c21 c22
          do j = 1,8
            pa(i,l,j) = par(k,j)*scl(l)
          end do
!             speed vector
          if (l .eq. 1) sp(i) = rpm
          call progress_update('BEARING_PARAMETERS',(l-1)*np+i,&
          &cp*np,rpm,'RPM')
          rpm = rpm+dr
        end do
!           rated speed
        if (spdn .gt. 0) then
!             nominal speed rad/s -> rad
          rad = rpm2radf(spdn)
!             variable speed bearing parameter -> par
          call parman(rad,.true.,mxm,nbr,par,errmsg,oki)
          if (oki .lt. 0) then
            return
          end if
!             stiffness and damping
!             k11 k12 k21 k22 c11 c12 c21 c22
!             get stiffness at rated speed -> dm
          do j = 1,8
            pa(np+1,l,j) = par(k,j)*scl(l)
          end do
          if (l .eq. 1) sp(np+1) = spdn
        end if
!
!           bearing number
        write(iu,25,err=200) c7,c1(3),k
        write(iu,5,err=200)
!           rated speed
        if (spdn .gt. 0) then
          txt = txtmsgf(50, 152)
          i = len_trim(txt)
          write(iu,45,err=200) txt(1:i),(c3(i),i=1,8)
          write(iu,55,err=200) sp(np+1),(pa(np+1,l,j),j=1,8)
          write(iu,5,err=200)
        end if
!           rpm
        txt = txtmsgf(50, 23)
        i = len_trim(txt)
        write(iu,65,err=200) txt(1:i),(c3(i),i=1,8)
!           parameter data speed,4xstiffness,4xdampings
        do i = 1,np
          write(iu,55,err=200) sp(i),(pa(i,l,j),j=1,8)
        end do
        write(iu,5,err=200)
!
!           bearing number if
      end if
!
!         bearing parameters do
    end do
!
!       bearings do
  end do
!
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
!         output file, 0=>-99 -> ofn
      call mntfnm(-18,m,ofn)
    end if
!
    call beap(nbrg,np,mxm,sp,pa,spdn,std,&
    &knd,ckd,ofn)
  end if
!     set return
  ok = 0
  call progress_end('BEARING_PARAMETERS','OK')
!
  return
!
5 format()
15 format(a)
25 format(2x,2(a,1x),i2)
35 format(2x,3(a,1x))
45 format(a,1x,8(a,15x))
55 format(9(e18.10))
65 format(a,15x,8(a,15x))
!
100 errmsg = fomsgf(99, nmm,1,ofn,1)
  return
!
!     close even std i/o
200 close(iu)
  errmsg = fomsgf(99, nmm,13,ofn,1)
  return
!
end subroutine sbeapar
!
!     ==================================================================
!>    @brief header output.
!
!>    @param[in] iu output unit handle
!
subroutine outdsc(iu)
  use com_cab, only: name, descript
  implicit none
!
  integer :: iu
!
!     header
!
  integer :: i, j
  character(len=1) :: cds
  character(len=5) :: clb
  character(len=76) :: tout
  dimension clb(2)
  parameter (cds = '-',clb = (/'user:','desc:'/))
!
  intrinsic :: len_trim
!
  i = len_trim(name)
  j = len_trim(descript)
!
  write(tout,5)clb(1),name(1:i),cds,clb(2),descript(1:j)
  i = len_trim(tout)
!
  call fltout(iu,tout(1:i))
!
  return
!
5 format(2a,1x,a,1x,2a)
!
end subroutine outdsc
!

