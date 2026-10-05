!     $Id$
!     ==================================================================
!
!>    @file messages.f
!>    @author francisco
!>    @date 13-mar-19
!>    @brief central messages and text, last changes:<br>
!>    new module - francisco - mar-19<br>
!>    added messages dump dmpmsg - francisco - may-20<br>
!>    added femsgf and fomsgf, formated error messages - francisco - jan
!>    message functions moved from saidas.f - francisco - jan-21<br>
!>    updated emsg control dollar sign - francisco jul-21<br>
!>    updated warnmsgf, fix warning if index less than zero - franciso -
!
!     ==================================================================
!>    @brief default message texts.
!>    Texts for error, warning, help, output, plot and screen messages.
!
!
!     ==================================================================
!>    @brief get user messages by index.
!
!>    @param[in] imsg message index
!>    @param[in] ikd kind of user message:
!>    0=matrix export error messages,1=error messages,
!>    2=warning messages,3=plot messages,4=help messages
!>    5=output text messages,6=screen messages
!>    @return user message by index and kind.
!
!
!     ==================================================================
!>    @brief get matrix export error messages by index.
!
!>    @param[in] imsg matrix export error message index
!>    @return matrix export error message by index
!
!
!     ==================================================================
!>    @brief get error messages by index.
!
!>    @param[in] imsg error message index
!>    @return error message by index
!
!
!     ==================================================================
!>    @brief get warning messages by index.
!
!>    @param[in] imsg warning message index, if less than zero
!>     add fixed warning text before.
!>    @return warning message by index
!
!
!     ==================================================================
!>    @brief get plot messages by index.
!
!>    @param[in] imsg plot message index
!>    @return plot message by index
!
!
!     ==================================================================
!>    @brief get help messages by index.
!
!>    @param[in] imsg help message index
!>    @return help message by index
!
!
!     ==================================================================
!>    @brief get output text messages by index.
!
!>    @param[in] imsg output text message index
!>    @return output text message by index
!
!
!     ==================================================================
!>    @brief get screen output text messages by index.
!
!>    @param[in] imsg output text message index
!>    @return output screen text message by index
!
!
!     ==================================================================
!>    @brief load user messages.
!>    one message per line<br>
!>    first kind of message 1-7, next message index, last message<br>
!>    message between single quotes. kind:
!>    1=matrix export error messages,2=error messages,
!>    3=warning messages,4=plot messages,5=help messages
!>    6=output text messages,7=screen messages.
!
!>    @param[in] ifh input handle
!>    @param[in] std standard input output
!
subroutine loadmsg(ifh,std)
  use com_emsgs, only: emsg
  use com_hmsgs, only: hmsg
  use com_mmmsg, only: exmsg
  use com_pmsgs, only: pmsg
  use com_scmsg, only: lmc
  use com_tmsgs, only: tmsg
  use com_wmsgs, only: wmsg
  implicit none
!
  integer :: ifh
  logical :: std
!
  integer :: i, j, k, l, ikn, ims, ios, nme
  character(len=7) :: c7
  character(len=50) :: msg
  parameter (c7 = 'loadmsg')
!
  integer :: mxg0, mxg1, mxg2, mxg3, mxg4, mxg5, nsmsg
  parameter (mxg0 = 15,mxg1 = 35,mxg2 = 45,&
  &mxg3 = 70,mxg4 = 30,mxg5 = 180,nsmsg = 30)
!
!     matrix export error messages
!     general error messages
!     general warning messages
!     general plot messages
!     general help messages
!     general text messages
!     screen messages
!
!
  intrinsic :: len, len_trim, min
!
!     maximum number of messages
  nme = mxg0+mxg1+mxg2+mxg3+mxg4+mxg5+nsmsg
!     standard
  if (std) then
!       when in standard mode, read number of messages to read
    read(ifh,*,err=200,end=200) i
    if (.not. i .gt. nme) nme = i
  end if
!
  i = 0
  do while (i .lt. nme)
!       msg with spaces between single quotes
    read(ifh,*,iostat=ios,end=200) ikn,ims,msg
    j = len_trim(msg)
    if (.not. ios .eq. 0) cycle
!       kind of message
    if (ikn .eq. 1) then
!         matrix export error messages
      k = len(exmsg(1))
      l = min(j,k)
      if (ims .gt. 0 .and. ims .le. mxg0) exmsg(ims) = msg(1:l)
    else if (ikn .eq. 2) then
!         general error messages
      k = len(emsg(1))
      l = min(j,k)
      if (ims .gt. 0 .and. ims .le. mxg1) emsg(ims) = msg(1:l)
    else if (ikn .eq. 3) then
!         general warning messages
      k = len(wmsg(1))
      l = min(j,k)
      if (ims .gt. 0 .and. ims .le. mxg2) wmsg(ims) = msg(1:l)
    else if (ikn .eq. 4) then
!         general plot messages
      k = len(pmsg(1))
      l = min(j,k)
      if (ims .gt. 0 .and. ims .le. mxg3) pmsg(ims) = msg(1:l)
    else if (ikn .eq. 5) then
!         general help messages
      k = len(hmsg(1))
      l = min(j,k)
      if (ims .gt. 0 .and. ims .le. mxg4) hmsg(ims) = msg(1:l)
    else if (ikn .eq. 6) then
!         general text messages
      k = len(tmsg(1))
      l = min(j,k)
      if (ims .gt. 0 .and. ims .le. mxg5) tmsg(ims) = msg(1:l)
    else if (ikn .eq. 7) then
!         screen messages
      k = len(lmc(1))
      l = min(j,k)
      if (ims .gt. 0 .and. ims .le. mxg5) lmc(ims) = msg(1:l)
    end if
    i = i+1
  end do
!
200 return
!
end subroutine loadmsg
!
!     ==================================================================
!>    @brief dump user messages.
!
!>    @param[in] iu output file handle.
!>    @param[out] ok return flag. unsuccessful if <0.
!
subroutine dmpmsg(iu,ok)
  use com_emsgs, only: emsg
  use com_hmsgs, only: hmsg
  use com_mmmsg, only: exmsg
  use com_pmsgs, only: pmsg
  use com_scmsg, only: lmc
  use com_tmsgs, only: tmsg
  use com_wmsgs, only: wmsg
  implicit none
!
  integer :: iu, ok
!
  integer :: mxg0, mxg1, mxg2, mxg3, mxg4, mxg5, nsmsg
  parameter (mxg0 = 15,mxg1 = 35,mxg2 = 45,&
  &mxg3 = 70,mxg4 = 30,mxg5 = 180,nsmsg = 30)
!
!     matrix export error messages
!     general error messages
!     general warning messages
!     general plot messages
!     general help messages
!     general text messages
!     screen messages
!
  integer :: i, j, k, l, nmsg, nvmsg
  parameter (nmsg = 7)
  character(len=1) :: blank, c1
  character(len=3) :: c3
  character(len=7) :: purp
  character(len=9) :: c9
  character(len=50) :: buf
  dimension nvmsg(nmsg),purp(nmsg)
  parameter (blank = ' ',c1 = '=',c3 = 'id:',&
  &purp = (/'export ','error  ','warning',&
  &'plot   ','help   ','text   ','screen '/),&
  &c9 = '**blank**',&
  &nvmsg = (/mxg0,mxg1,mxg2,mxg3,mxg4,mxg5,nsmsg/))
!
  intrinsic :: len_trim
!
  ok = -1
!
  do i = 1,nmsg
!       begin separator
    write(buf,150) c3,i,c1,purp(i)
    call marksep(iu,0,buf)
    k = nvmsg(i)
    do l = 1,k
!         export
      if (i .eq. 1) buf = exmsg(l)
!         error
      if (i .eq. 2) buf = emsg(l)
!         warning
      if (i .eq. 3) buf = wmsg(l)
!         plot
      if (i .eq. 4) buf = pmsg(l)
!         help
      if (i .eq. 5) buf = hmsg(l)
!         text
      if (i .eq. 6) buf = tmsg(l)
!         screen
      if (i .eq. 7) buf = lmc(l)
      j = len_trim(buf)
      if (j .eq. 0) then
        write(iu,200,err=100) l,c1,c9
      else
        write(iu,200,err=100) l,c1,buf(1:j)
      end if
    end do
!       end separator
    call marksep(iu,1,blank)
  end do
!
  ok = 0
  return
!
100 return
!
150 format(a,i1,2a)
200 format(i3,2a)
!
end subroutine dmpmsg
!
!     ==================================================================
!>    @brief prepare message text.
!>    Format user standard messages.
!
!>    @param[in] mname module name, can be left empty
!>    @param[in] msgtxt main message text
!>    @param[in] opttxt optional additional text
!>    @param[in] usesep use defined separators around optional text,
!>     0 will not use
!
!>    @return prepared standard user message text
!
!
!     ==================================================================
!>    @brief prepare message text using error and warning messages.
!>    Format user standard messages using error and warning messages
!>    indices.
!
!>    @param[in] mname module name, can be left empty
!>    @param[in] nemsg valid error message index greater than zero.
!>    @param[in] nwmsg optional warning message index, 0 will not use.
!>    @param[in] usesep use defined separators around optional text,
!>     0 willnot use
!>    @return prepared standard user message text.
!>    @see fmtmsgf,warnmsgf
!
!
!     ==================================================================
!>    @brief prepare message text using error, warning or help and
!>     optional messages.
!>    Format user standard messages using error, warning or help
!>    message index and an optional message.
!
!>    @param[in] mname module name, can be left empty
!>    @param[in] nmsg valid message index greater than zero.
!>    @param[in] opttxt optional additional text
!>    @param[in] usesep use defined separators around optional text,
!>     0 will not use
!>    @param[in] ikd kind of message, 0=error,1=warning and 2=help,
!>     3=matrix export.
!>    @return prepared standard user message text.
!>    @see fmtmsgf,errmsgf,,warnmsgf,hlpmsgf,mexmsgf
!
!
!     ==================================================================
!>    @brief prepare message text using error and optional messages.
!>    Format user standard messages using error message index and
!>    an optional message.
!
!>    @param[in] mname module name, can be left empty
!>    @param[in] nemsg valid error message index greater than zero.
!>    @param[in] opttxt optional additional text
!>    @param[in] usesep use defined separators around optional text
!>    @return prepared standard user message text.
!>    @see famsgf
!
!
!     ==================================================================
!>    @brief prepare message text using warning and optional messages.
!>    Format user standard messages using warning message index and
!>    an optional message.
!
!>    @param[in] mname module name, can be left empty
!>    @param[in] nwmsg valid warning message index greater than zero.
!>    @param[in] opttxt optional additional text
!>    @param[in] usesep use defined separators around optional text
!>    @return prepared standard user message text.
!>    @see famsgf
!
!
!     ==================================================================
!>    @brief prepare message text using help and optional messages.
!>    Format user standard messages using help message index and
!>    an optional message. Output message will have a dollar sign prefix
!
!>    @param[in] mname module name, can be left empty
!>    @param[in] nhmsg valid help message index greater than zero.
!>    @param[in] opttxt optional additional text
!>    @param[in] usesep use defined separators around optional text,
!>     0 will not use
!>    @return prepared standard user message text.
!>    @see famsgf
!
!
!     ==================================================================
!>    @brief prepare message text using matrix export error and
!>     optional messages.
!>     Format user standard messages using matrix export error message
!>     index and an optional message.
!
!>    @param[in] mname module name, can be left empty
!>    @param[in] nmmsg valid matrix export message index greater than ze
!>    @param[in] opttxt optional additional text
!>    @param[in] usesep use defined separators around optional text,
!>     0 will not use
!>    @return prepared standard user message text.
!>    @see famsgf
!
!
!     ==================================================================
!>    @brief prepare message text for input data reading errors.
!>    Format user standard messages using error message index,
!>    input file and reading section names.
!
!>    @param[in] mname module name, can be left empty.
!>    @param[in] nemsg valid error message index greater than zero.
!>    @param[in] pname reading section name.
!>    @param[in] fname file name.
!>    @return prepared standard user message text.
!>    @see fmtmsgf,errmsgf
!
!
!     ==================================================================
!>    @brief screen messages and log file handling.
!
!>    @param[in] ilog flag, 0 = without file log
!>    @param[in] wtd <code>1</code> should application be stopped
!>    @param[in] msg message text
!
subroutine emsg(ilog,wtd,msg)
  use rd_textfun, only: fomsgf
  use com_msb, only: lid, msl
  implicit none
!
!     arguments
  integer :: ilog, wtd
  character(len=*) :: msg
!
!     log
  integer :: mxlg
  parameter(mxlg = 20)
!
!     locals
  logical :: dlr, sco, lds
  integer :: iwtd, i, k, cols
  character(len=1) :: cs
  character(len=3) :: ds
  character(len=4) :: nmm
  character(len=10) :: dm, tm
  character(len=12) :: ts
  character(len=117) :: tmp
  dimension cs(6),ds(3)
  parameter (nmm = 'emsg',cs = (/'$','%',':','(',')',' '/),&
  &ds = (/'MSG','ERR','lid'/),cols = 80)
!     error messages
!
  data lds/.false./
  save lds
!
!     added - francisco - feb-19
  intrinsic :: index, len_trim
!
!     current date
  call sdate(dm,tm)
!
  iwtd = wtd
!     time format
  write(ts,50) cs(4),tm,cs(5)
!
  k = len_trim(msg)
!     check for dollar sign
  dlr = msg(1:1) .eq. cs(1)
!
!     check for percent sign, no stopping message
  sco = msg(1:1) .eq. cs(2)
!
!     should write to log file
  if (ilog .ne. 0) then
    if (dlr .or. sco) then
      i = 2
      if (msg(i:i) .le. cs(6)) i = i+1
      write(tmp,100) ds(1),ts,cs(3),msg(i:k)
      msl(lid) = tmp(1:cols)
    else
      write(tmp,100) ds(2),ts,cs(3),msg(1:k)
      msl(lid) = tmp(1:cols)
    end if
    lid = lid+1
!       check log array limit
    if (lid .gt. mxlg) then
      iwtd = 1
      msg = fomsgf(99, nmm,4,ds(3),0)
    end if
  end if
!
!     stop screen message - stderr (0)
  if (iwtd .ne. 0 .or. sco) then
    if (dlr .or. sco) then
!         check for previous dollar, line in hold
      if (lds) then
        write(0,200) ds(1),ts,cs(3),msg(2:k)
      else
        write(0,100) ds(1),ts,cs(3),msg(2:k)
      end if
    else
!         error
      if (lds) then
        write(0,200) ds(2),ts,cs(3),msg(1:k)
      else
        write(0,100) ds(2),ts,cs(3),msg(1:k)
      end if
    end if
  end if
!
!     wanted stop
  if (iwtd .eq. 1) then
    stop 1
  end if
!
!     save dollar mark flag
  lds = dlr
!
  return
!
50 format(3a)
100 format(4a)
200 format(/,4a)
!
end subroutine emsg
!
!     ==================================================================
!>    @brief screen messages and log file handling, convenience method.
!
!>    @param[in] wtd <code>1</code> should application be stopped
!>    @param[in] ms message text
!>    @see emsg
!
subroutine lmsg(wtd,ms)
  use com_lgf, only: ilg
!     arguments
  integer :: wtd
  character(len=*) :: ms
!
  integer :: i, mx
  character(len=99) :: msg
  parameter (mx = 99)
!
!     log flag
!
  intrinsic :: len_trim
!
  i = len_trim(ms)
  if (i .gt. 0) then
    if (i .gt. mx) i = mx
    msg = ms(1:i)
    call emsg(ilg,wtd,msg)
  end if
!
  return
!
end subroutine lmsg
!
!     ==================================================================
!>    @brief screen warning message and log file handling.
!>    Convenience method for just show warning message, no stop.
!
!>    @param[in] ms message text
!>    @see lmsg
!
subroutine wlmsg(ms)
  implicit none
  character(len=*) :: ms
!
  character(len=1) :: c1
  parameter (c1 = '%')
!
  call lmsg(0,c1//ms)
!
  return
!
end subroutine wlmsg
!
!     ==================================================================
!>    @brief screen error and warning messages and log file handling,
!>    another convenience method.
!
!>    @param[in] wtd <code>1</code> should application be stopped
!>    @param[in] midx warning or error  message index
!>    @param[in] nmm originating module name
!>    @param[in] opt additional text output after index message
!>    @param[in] warn set if warning message error else
!>    @see lmsg
!
subroutine elmsga(wtd,midx,nmm,opt,warn)
  use rd_textfun, only: ciff, errmsgf, fmtmsgf, warnmsgf
  implicit none
!
  logical :: warn
  integer :: wtd, midx
  character(len=*) :: nmm, opt
!
!     error messages functions
!
  integer :: i, mx
  character(len=1) :: c1
  character(len=99) :: ms, msw, msg
  parameter (c1 = '%',mx = 99)
!
  intrinsic :: len_trim
!
!     get error or warning message
  msg = ciff(99, warn,warnmsgf(99, midx),errmsgf(99, midx))
!     call format function error ierr
  ms = fmtmsgf(99, nmm,msg,opt,0)
!     check for warning message
  if (warn) then
!       if warning, add dollar sign to mark it as a warning
    i = len_trim(ms)
    if (i .lt. mx) then
      write(msw,5) c1,ms(1:i)
    else
      write(msw,5) c1,ms(1:mx-1)
    end if
!       output error message
    call lmsg(wtd,msw)
  else
    call lmsg(wtd,ms)
  end if

  return
!
5 format(2a)
!
end subroutine elmsga
!
!     ==================================================================
!>    @brief screen error messages and log file handling,
!>    another convenience method.
!
!>    @param[in] wtd <code>1</code> should application be stopped
!>    @param[in] midx error message index
!>    @param[in] nmm originating module name
!>    @see elmsga
!
subroutine elmsge(wtd,midx,nmm)
  implicit none
!
  integer :: wtd, midx
  character(len=*) :: nmm
!
  character(len=1) :: blank
  parameter(blank = ' ')
!
  call elmsga(wtd,midx,nmm,blank,.false.)
  return
!
end subroutine elmsge
!
!     ==================================================================
!>    @brief screen warning messages and log file handling,
!>    another convenience method.
!
!>    @param[in] wtd <code>1</code> should application be stopped
!>    @param[in] midx warning message index
!>    @param[in] nmm originating module name
!>    @see elmsga
!
subroutine elmsgw(wtd,midx,nmm)
  implicit none
!
  integer :: wtd, midx
  character(len=*) :: nmm
!
  character(len=1) :: blank
  parameter(blank = ' ')
!
  call elmsga(wtd,midx,nmm,blank,.true.)
!
  return
!
end subroutine elmsgw
!
!     ==================================================================
!>    @brief screen warning messages and log file handling, additional
!>     message. another convenience method.
!
!>    @param[in] wtd <code>1</code> should application be stopped
!>    @param[in] midx message index
!>    @param[in] nmm originating module name
!>    @param[in] opt additional message
!>    @see elmsga
!
subroutine elmsgow(wtd,midx,nmm,opt)
  implicit none
!
  integer :: wtd, midx
  character(len=*) :: nmm, opt
!
  call elmsga(wtd,midx,nmm,opt,.true.)
  return
!
end subroutine elmsgow
!
!>    @brief text with size of the matrices
!
!>    @param[in] pdm size of matrices
!>    @param[in] lop optional identification text,
!>     false blank, "b" support (base) consideration.
!
!
