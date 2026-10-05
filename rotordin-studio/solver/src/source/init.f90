!     $Id$
!     ==================================================================
!
!>    @file init.f
!>    @author francisco
!>    @date 03-dec-08
!>    @brief initialization procedures, last changes:<br>
!>    new module - francisco - 03/12/2008<br>
!>    added option stiffness map opt(9) - francisco - 08/06/2009<br>
!>    changed help (), string vector added  - francisco - 14/09/2009<br>
!>    changed help (), support parameters added - francisco - 17/11/2009
!>    rtinfo changed, added bsf - francisco - 17/11/2009<br>
!>    cmdarg changed, changed opt size  - francisco - 17/11/2009<br>
!>    cmdarg changed, added opt(10) - Francis - 17/11/2009<br>
!>    changed version - francisco - 05/11/2010<br>
!>    added column orient to rtinfo - francisco - 29/06/2011<br>
!>    comments translated to english - francisco - 06/02/2012<br>
!>    emsg moved to saidas.f - francisco - 20/02/2012<br>
!>    oplg call moved from init.f to rotordin.f - 20/02/2012<br>
!>    added kind on unbalance output template - francisco - 14/02/2014<b
!>    updated base name routine - francisco - 14/02/2014<br>
!>    added TANGLE, changed version to 1.06 - francisco - 01/10/2014<br>
!>    added options export 12 HB and 13 matrix market - francisco - 19/1
!>    removed file extension, moved to saidas.f - francisco 28-may-15<br
!>    changed cmdarg, changed end by else if - francisco 29-may-15<br>
!>    added -p, HPGL plot option - francisco 29-may-15<br>
!>    added MINAMP and MDRPM data in the mskinf data block - francisco -
!>    minor version changed to 4 - francisco - jul-15<br>
!>    added support entry on input data block, updated mask - francisco
!>    added support parameter scale sps - francisco - nov-15<br>
!>    modified mskinf added support and bearing sale text - francisco -
!>    added undamped angular critical speed map option - francisco nov-1
!>    added variable speed parameters plot option "j" - francisco feb-19
!>    changed to central messages - francisco - apr-19<br>
!>    changed valid options are at common block, see blockd.f - francisc
!>    added sinfo single line application info for log purpose - francis
!>    added torsion time transient option, updtaed messages from cmdarg
!>    added sub options for input mask generation - francisco apr-20<br>
!>    changed strings to constants where possible - francisco - sep-20<b
!>    changed sinfo, added pinfo, updated code to use it - francisco - s
!>    added isf on init, bend stress median filter window length - fraci
!>    added flexural-torsion option - francisco - jul-21<br>
!>    added "GYCOF" index-gyroscopic factor, comma separated input -  fr
!>    added cpinfof and rvinfof, encapsulate compiler and revision info
!>    updated pinit, check for var. param. bearing output wihtout al lea
!>    added iex (kind export flag) on getarg, init and pinit - francisco
!>    added fxstf fixed stiffnes for no displacement on mskinf - francis
!>    moved block data to blockd.f - francisco dec-21
!
!     ==================================================================
!>    @brief default block data.
!>    @see blockd.f, saidas.f
!
!     ==================================================================
!>    @brief search for standard input file (rotordin.in)
!
!>    @param lcl first command line argument (0), i. e. the applications
!>    @param ifn complete input file name
!>    @param ein input file name (block data)
!>    @param wrl input file folder (found)
!>    @param fng file exit flag
!
subroutine findif(lcl,ifn,ein,wrl,fng)
  implicit none
!
!     sub arguments
  character(len=255) :: lcl, ifn, wrl
  character(len=128) :: ein
  logical :: fng
!
!     locals
  integer :: i, j, k, l
  character(len=1) :: slash, bslash, blank
  character(len=5) :: tmp
  parameter (slash = '/',bslash = char(92),blank = ' ',&
  &tmp= '/tmp/')
  character(len=255) :: nar
!
!     intrinsic functions
  intrinsic :: len_trim
!
!     length of the application executable
!     string space striped
  j = len_trim(lcl)
!
!     search for one path separator in lcl
!     the work folder is empty wrl = ' '
  if (wrl(1:1) .le. blank) then
!
!       always changes back "/" by slash "/"
    do i = j, 1, -1
      if (lcl(i:i) .eq. bslash) lcl(i:i) = slash
    end do
!       set the work foder
    wrl = lcl
!
!       search for last "/"
    if (wrl(j:j) .ne. slash) then
      write(wrl,10) lcl(1:j),slash
    end if
!
  end if
!
!     work folder string length space striped
  k = len_trim(wrl)
!
!     the input file name is not set
  if (ifn(1:1) .le. blank) then
!
!       input file name string length space striped
    j = len_trim(ein)
!
!       search for the input file on the work folder
    write(nar,10) wrl(1:k),ein(1:j)
    l = len_trim(nar)
    inquire(file = nar(1:l),exist = fng)
    if (fng) then
!         found
      ifn = nar
!         return ok
      return
!
    end if
!
!     only input file name
  else
!       input file name string length space striped
    j = len_trim(ifn)
!
!       search for the input file on the work folder
    write(nar,10) wrl(1:k),ifn(1:j)
    l = len_trim(nar)
    inquire(file = nar(1:l),exist = fng)
    if (fng) then
!         found
      ifn = nar
!
!         return ok
      return
!
    end if
!
  end if
!
!     search in "/tmp/" (old default folder)
  wrl = tmp
  k = len_trim(wrl)
!
!     input file name empty
  if (ifn(1:1) .le. blank) then
    j = len_trim(ein)
    write(nar,10) wrl(1:k),ein(1:j)
    l = len_trim(nar)
    inquire(file = nar(1:l),exist = fng)
    if (fng) ifn = nar
!       should check if return is ok, ifn = ' '
    return
!
!     only input file name
  else
!       input file name string length space striped
    j = len_trim(ifn)
!       mount complete floder
    write(nar,10) wrl(1:k),ifn(1:j)
    inquire(file = nar(1:l),exist = fng)
    if (fng) ifn = nar
!       should check if return is ok, ifn = ' '
    return
!
  end if
!
  return
!
10 format(2a)
!
end subroutine findif
!
!     ==================================================================
!>    @brief get local input output folder, work folder
!
!>    @param ifn complete input file name
!>    @param wrl return the input file work folder
!
subroutine wrklcl(ifn,wrl)
  implicit none
!
!     sub arguments
  character(len=255) :: ifn, wrl
!
!     local
  integer :: i, j
  character(len=1) :: slash, ddot, bslash
  parameter (slash = '/',bslash = char(92),ddot = ':')
!
!     intrinsic functions
  intrinsic :: char, len_trim
!
!       input file name string length space striped
  j = len_trim(ifn)
!
!     changes back "/" by slash "/"
  do i = j,1,-1
    if (ifn(i:i) .eq. bslash) ifn(i:i) = slash
  end do
!
!     search last slash "/" (or ":" windows)
  do i = j,1,-1
    if ((ifn(i:i) .eq. slash) .or. (ifn(i:i) .eq. ddot)) then
      wrl = ifn(1:i)
!         exit do loop
      exit
    end if
  end do
!
  return
!
end subroutine wrklcl
!
!     ==================================================================
!>    @brief check if the work folder is usable.
!>    Should not do this when in standard mode.
!
!>    @param wrl input file work folder
!>    @param[out] ok return length for work folder writeable or
!>                <code>-1</code> for work folder not writeable
!
subroutine chkfld(wrl,ok)
  implicit none
!
!     sub argument
  character(len=255) :: wrl
  integer :: ok
!
!     locals
  character(len=12) :: tfn
  character(len=6) :: cdl
  character(len=1) :: slash, bslash
  parameter (slash = '/',bslash = char(92),cdl = 'delete')
  parameter (tfn = 'rotordin.tmp')
  integer :: i, j
!
!     intrinsic functions
  intrinsic :: len_trim
!
!     return init
  ok = -1
!
!     work folder name string length space striped
  j = len_trim(wrl)
!
!     changes back "/" by slash "/"
  do i = j,1,-1
    if (wrl(i:i) .eq. bslash) wrl(i:i) = slash
  end do
!
!     check if last char is the file separator "/"
  if (j .gt. 0) then
    if (wrl(j:j) .ne. slash) then
      write(wrl,5) wrl(1:j),slash
    end if
!       tries to open a file in the work folder
    open(1,file=wrl(1:j)//tfn,err=100)
  else
    open(1,file=tfn,err=100)
  end if
!
!     should delete it
  close(1,status=cdl)
!
!     ok return if there is some wrl (work folder)
  ok = len_trim(wrl)
!
100 return
!
5 format(2a)
!
end subroutine chkfld
!
!     ==================================================================
!>    @brief generates a blank input file mask.

!>    @param[in] ifn input file name
!>    @param[in] std standard i/o
!>    @param[in] gso mask sub option:(b)earing,(s)upport,(t)orsion
!>    @param[out] errmsg return error message
!>    @param[out] ok return flag. unsuccessful if <0.
!
subroutine mkmsk(ifn,gso,std,errmsg,ok)
  use rd_textfun, only: fomsgf
  use com_lid, only: nre, lne
  use com_lio, only: nro, lno
  use com_lis, only: nrs, lns
  use com_lit, only: nrt, lnt
  implicit none
!
!     arguments
  integer :: ok
!     sub option,
  character(len=1) :: gso
  character(len=99) :: errmsg
  character(len=255) :: ifn
  logical :: std
!
!     locals
  integer :: i, j, k, iu
!     mask generation sub-options, see chkmsk
  character(len=1) :: gsb
  character(len=5) :: nmm, cst
  dimension gsb(4)
!     (b)earing,(s)upport,(t)orsion
  parameter (gsb = (/'b','s','t',' '/),&
  &nmm = 'mkmsk',cst = 'stdio')
!
!     block mask (see saidas.f)

!     options file mask block
!     nro number of non blank input description tags line.
!     lno string vector with the tags lines, each line.
!
!     support file mask block
!
!     torsion options file mask block
!
!     intrinsic functions
  intrinsic :: len_trim
!
!     init return
  ok = -1
!
  if (.not. std) then
!       mask file name string length space striped
    i = len_trim(ifn)
    iu = 12
    open(iu,file=ifn(1:i),err=50)
  else
    iu = 6
    ifn = cst
  end if
!
  if (gso .le. gsb(4)) then
!       default input mask
    do j = 1,nre
      k = len_trim(lne(j))
      write(iu,5,err=60) lne(j)(1:k)
      write(iu,15,err=60)
    end do
  else if (gso .eq. gsb(1)) then
!       (b)earing mask
    do j = 1, nro
      k = len_trim(lno(j))
      write(iu,5,err=60) lno(j)(1:k)
      write(iu,15,err=60)
    end do
  else if (gso .eq. gsb(2)) then
!       (s)upport mask
    do j = 1, nrs
      k = len_trim(lns(j))
      write(iu,5,err=60) lns(j)(1:k)
      write(iu,15,err=60)
    end do
  else if (gso .eq. gsb(3)) then
!       (t)torsion mask
    do j = 1, nrt
      k = len_trim(lnt(j))
      write(iu,5,err=60) lnt(j)(1:k)
      write(iu,15,err=60)
    end do
  end if
!
  if (.not. std) then
    close(iu)
  end if
!
!     return ok
  ok = 0
  return
!
50 errmsg = fomsgf(99, nmm,1,ifn,1)
  return
!
60 errmsg = fomsgf(99, nmm,13,ifn,1)
  return
!
5 format(a)
15 format()
!
end subroutine mkmsk
!
!     ==================================================================
!>    @brief prepares help command line options.
!>    @see messages.f, blockd.f
!
!>    @param[out] ops command line options help
!>    @param[in] nop options dimension
!
subroutine prepops(ops,nop)
  use com_otho, only: oopt
  use com_vldo, only: vopt
  implicit none
!
  character(len=7) :: ops
  integer :: nop
  dimension ops(nop)
!
  character(len=7) :: cst
  parameter (cst = ' [-std]')
!
!     valid options, see blockd.f
  integer :: jvop
  parameter (jvop = 20)
!
!     other valid command line options, see blockd.f
  integer :: joop
  parameter (joop = 7)
!
  write(ops(1),20) vopt(15)
  write(ops(2),20) vopt(10)
  write(ops(3),20) vopt(1)
  write(ops(4),20) vopt(6)
  write(ops(5),20) vopt(3)
  write(ops(6),20) oopt(1)
  write(ops(7),20) vopt(16)
  write(ops(8),20) vopt(9)
  write(ops(9),20) oopt(2)
  write(ops(10),20) vopt(2)
  write(ops(11),20) vopt(19)
  write(ops(12),20) oopt(7)
  write(ops(13),20) vopt(14)
  write(ops(14),20) vopt(20)
  write(ops(15),20) vopt(8)
  write(ops(16),20) vopt(4)
  write(ops(17),20) oopt(3)
  write(ops(18),20) vopt(12)
  write(ops(19),20) oopt(4)
  write(ops(20),20) oopt(5)
  write(ops(21),20) oopt(6)
  write(ops(22),10) cst
!
  return
!
10 format(a)
20 format(' [-',a,']  ')
!
end subroutine prepops
!
!     ==================================================================
!>    @brief show help text on screen.
!>    @see messages.f
!
subroutine help()
  use rd_textfun, only: hlpmsgf
  implicit none
!
  character(len=3) :: cfm
  parameter (cfm = '(a)')
  character(len=80) :: txt
  integer :: i
!     see messages.f
  integer :: nop
  parameter (nop = 22)
  character(len=7) :: ops(nop)
!
  intrinsic :: trim
!
  call prepops(ops,nop)
!
  txt = trim(hlpmsgf(50, 25))
  call sprint(.false.,txt,cfm)
  do i = 1,nop
    write(txt,5) ops(i),trim(hlpmsgf(50, i))
    call sprint(.false.,txt,cfm)
  end do
  txt = trim(hlpmsgf(50, 26))
  call sprint(.false.,txt,cfm)
!
  return
!
5 format(2a)
!
end subroutine help
!
!     ==================================================================
!>    @brief input file base name, before point "."
!
!>    @param ifn input file name
!>    @param wrl work folder name
!>    @param bsn return input file base name
!
subroutine basnam(ifn,wrl,bsn)
  implicit none
!
!     sub arguments
  character(len=255) :: ifn, wrl
  character(len=128) :: bsn
!
!     locals
  character(len=1) :: dot
  parameter (dot = '.')
  integer :: i, j, k, l, m
!
!     intrinsic functions
  intrinsic :: len_trim
!
!     complete input file name string length space striped
  i = len_trim(ifn)
!
!     work folder name string length space striped
  j = len_trim(wrl)
!
!     search for the dot "." in the input file name string
  k = i
10 k = k-1
  if (ifn(k:k) .ne. dot .and. k .gt. 1)  goto 10
!
  l = j+1
  m = k-1
!     dot "." found and wrl is in ifn
  if (k .gt. 1 .and. ifn(1:j) .eq. wrl(1:j)) then
    bsn = ifn(l:m)
!     dot "." found wrl not in inf
  else if(k .gt. 1) then
    bsn = ifn(1:m)
!     dot "." not found wrl in ifn
  else if (k .eq. 1 .and. ifn(1:j) .eq. wrl(1:j)) then
    bsn = ifn(l:)
!       not found wrl not in ifn
  else
    bsn = ifn(1:i)
  end if
!
  return
!
end subroutine basnam
!
!     ==================================================================
!>    @brief get compilert info.
!>     check preprocessor compiler capability (cpp,fpp).
!
!>    @param[out] csiz size of revision info
!
!>    @return compilert info.
!>    @see compiler.inc rtinfo
!
!
!     ==================================================================
!>    @brief get revision info.
!>     check preprocessor compiler capability (cpp,fpp).
!
!>    @param[out] csiz size of revision info
!>    @return revision info.
!>    @see revision.inc rtinfo
!
!
!     ==================================================================
!>    @brief print version info.
!>    @see rtinfo,cpinfof,sprint
!
subroutine printvr()
  use rd_textfun, only: cpinfof, rvinfof
  use com_inf, only: vrs, dta, rgh, sid
  use com_lif, only: usr, dtu, tmu
  use com_nmi, only: inm, sbv
  implicit none
!
  integer :: i
  character(len=1) :: blank
  character(len=2) :: cno
  character(len=3) :: fmt, opt, cyes
!     revision info
  character(len=11) :: ctx
!     compiler info
  character(len=256) :: buf
  dimension ctx(7)
  parameter (blank = ' ',cno = 'no',cyes = 'yes',fmt ='(a)',&
  &ctx = (/' appName : ',' version : ',' ver.year: '&
  &,' revision: ',' build   : ',' opt lpk : ',' OS user : '/))
!
!     internal name
!
!     system identification
!
!     user info
!
!     build time stamp
!     $Id$
!
!>    @file build.inc
!
!> \verbatim
!>     build date and time info.
!>     should be generated on makefile.
!>     only declarations here.
!> \endverbatim
!
  character(len=24) :: bdti
!     parameter (bdti = '2021-SE-24 11:02')
  parameter (bdti = '2025-JA-24 10:30')
!
!
!     optimization
!
!     preprocessor flag
!
#ifdef _OPT
  opt = cyes
#else
  opt = cno
#endif
!
!     version info
  call sprint(.false.,blank,fmt)
  call sprint(.false.,rgh,fmt)
  call sprint(.false.,sid,fmt)
  buf = cpinfof(256, i)
  call sprint(.false.,blank//buf(1:i),fmt)
  call sprint(.false.,blank,fmt)
  call sprint(.false.,ctx(1)//inm,fmt)
  call sprint(.false.,ctx(2)//vrs//sbv,fmt)
  call sprint(.false.,ctx(3)//dta,fmt)
  buf = rvinfof(5, i)
  call sprint(.false.,ctx(4)//buf(1:i),fmt)
  call sprint(.false.,ctx(5)//bdti,fmt)
!
!     addeed optimized lapak support - francisco - feb-19
  call sprint(.false.,ctx(6)//opt,fmt)
  call sprint(.false.,ctx(7)//usr,fmt)
  call sprint(.false.,blank,fmt)
!
  return
!
end subroutine printvr
!
!     ==================================================================
!>    @brief print program header. checkout include file.
!>    @see rtinfo
!
subroutine printin()
  use rd_textfun, only: cpinfof, pinfo, sinfo
  use com_sta, only: stm
  implicit none
!
!     locals
  integer :: i
  logical :: fls
  character(len=1) :: blank
  character(len=3) :: fmt
!     compiler info, see compiler.inc
  character(len=256) :: buf
  parameter (fls = .false.,blank = ' ',fmt ='(a)')
!
!     stamp
!
!     application info
  call sprint(fls,blank,fmt)
  call sprint(fls,pinfo(42, .false.),fmt)
  call sprint(fls,sinfo(50, .false.,1),fmt)
  buf = cpinfof(256, i)
  call sprint(fls,blank//buf(1:i),fmt)
  call sprint(fls,blank,fmt)
  call sprint(fls,stm,fmt)
  call sprint(fls,blank,fmt)
!
  return
!
end subroutine printin
!
!     ==================================================================
!>    @brief get single line production info.
!
!>    @param[in] lgn true for log info.
!>    @return single line production info.
!>    @see rtinfo
!
!
!     ==================================================================
!>    @brief get single line application info
!
!>    @param[in] lgn true for log info
!>    @param[in] ifl non zero for full aaplication name short name else
!>    @return single line application info
!>    @see rtinfo,rvinfof
!
!
!     ==================================================================
!>    @brief handles input mask data file writing.
!
!>    @param[in] msk mask output file name
!>    @param[in] ifn input file name
!>    @param[in] wrl work folder name
!>    @param[in] lcl local folder name (exec)
!>    @param[in] wfl user work folder set flag
!>               <code>true</code> user folder is set
!>               <code>false</code> user folder is NOT set
!>    @param[in] gso mask sub option:(b)earing,(s)upport,(t)orsion
!>    @param[in] std standard i/o
!>    @param[out] errmsg return error message
!>    @param[out] ok return flag. unsuccessful if <0.
!
subroutine chkmsk(msk,ifn,wrl,lcl,wfl,gso,std,errmsg,ok)
  use rd_textfun, only: femsgf, fhmsgf, fomsgf
  implicit none
!
!     parameters
  integer :: ok
!     sub option,
  character(len=1) :: gso
  character(len=99) :: errmsg
  character(len=255) :: msk, ifn, wrl, lcl
  logical :: wfl, std
!
!     locals
  integer :: i, j
  character(len=6) :: nmm
  parameter(nmm ='chkmsk')
  character(len=255) :: nar

!     intrinsic functions
  intrinsic :: len_trim
!
!     not standadrd i/o
  if (.not. std) then
!
!       user folder set -d
    if (wfl) then
!         check the folder
      call chkfld(wrl,j)
!         folder ok
      if (j .gt. 0) then
!           msk file name
        i = len_trim(msk)
!           complete file name
        write(nar,10) wrl(1:j),msk(1:i)
!           ok generates the input mask file
        call mkmsk(nar,gso,std,errmsg,ok)
!           return due mkmsk internal error
        if (ok .ne. 0) return
!           file generated' !29
        errmsg = fhmsgf(99, nmm,29,nar,1)
!         folder unusable, giving up
      else
!           'invalid directory' !26
        errmsg = fomsgf(99, nmm,26,wrl,1)
      end if
!       try msk folder
    else
!         get folder name from input file name
      call wrklcl(msk,ifn)
!         check the folder
      call chkfld(ifn,j)
!         folder ok
      if (j .gt. 0) then
!           ok generates the input mask file
        call mkmsk(msk,gso,std,errmsg,ok)
!           return due mkmsk internal error
        if (ok .ne. 0) return
        errmsg = fhmsgf(99, nmm,29,msk,1)
!         msk folder unusable/given, try local
      else
!           check the local folder
        call chkfld(lcl,j)
!           lcl folder ok
        if (j .gt. 0) then
          !           msk file name
          i = len_trim(msk)
          write(nar,10)lcl(1:j),msk(1:i)
!             ok generates the input mask file
          call mkmsk(nar,gso,std,errmsg,ok)
!             return due mkmsk internal error
          if (ok .ne. 0) return
          errmsg = fhmsgf(99, nmm,29,nar,1)
        else
!             'invalid directory' !26
!             'folder name' !22
          errmsg = femsgf(99, nmm,26,22,0)
!           if lcl folder ok
        end if
!         msk folder
      end if
!       user folder set if
    end if
!
  else
!       ok generates the input mask file on std
    call mkmsk(nar,gso,std,errmsg,ok)
    errmsg = fhmsgf(99, nmm,29,nar,1)
  end if

!     ever returns error code
  ok = -1

  return
!
10 format(2a)
!
end subroutine chkmsk
!
!     ==================================================================
!>    @brief command line arguments.
!>    @see also coptf function in entrada.f and blockd.f
!
!>    @param[out] llg log recording flag
!>    @param[out] ver request version info flag
!>    @param[out] hlp request help flag
!>    @param[out] dmp request dump messages flag
!>    @param[out] std standard input output
!>    @param[out] iex export kind flag = 0 HB, 1=mm,2=plain
!>    @param[out] errmsg error message
!>    @param[out] ok flag. unsuccessful if <0.
!
subroutine cmdarg(llg,ver,hlp,dmp,std,iex,errmsg,ok)
  use rd_sys, only: rd_getcwd
  use rd_textfun, only: femsgf, fhmsgf, fomsgf
  use com_fbn, only: bsn
  use com_inm, only: ein
  use com_otho, only: oopt
  use com_ppr, only: opt, ifn
  use com_pst, only: wrl
  use com_vldo, only: vopt
  implicit none
!
!     arguments
  integer :: iex, ok
  character(len=99) :: errmsg
  logical :: llg, ver, dmp, hlp, std
!
!     locals
  integer :: i, j, k
  integer :: ncm
!     function
!     mask generation sub-options
  character(len=1) :: gsb, gso, dash, blank
!     (b)earing,(s)upport,(t)orsion
!     matrix export options
  character(len=2) :: cex
!     standard io option
  character(len=4) :: cstd
  character(len=6) :: nmm
  character(len=255) :: buf, lcl, msk
  logical :: vld, ler, gmf, wfl, bpl, lok, msi, transient_cli_requested_f
  dimension gsb(4),cex(3)
  parameter (gsb = (/'b','s','t','d'/),dash = '-',blank = ' ',&
  &cex = (/'mm','HB','pl'/),cstd = '-std',nmm = 'cmdarg')
!
  data&
!     local error
  & ler /.false./&
!     empty input mask -g
  &,gmf /.false./&
!     defines the output(/input) <fold>er name -d
  &,wfl /.false./&
!     valid argument flag
  &,vld /.false./
!
!     parameters
  integer :: mop
  parameter (mop = 20)
!
!     please checkout the init loop
!
!     valid options, see blockd.f
  integer :: jvop
  parameter (jvop = 20)
!
!     other valid command line options, see blockd.f
!     valid parameters:help,version,info,log,directory,mask
  integer :: joop
  parameter (joop = 7)
!
!     base name
!
!     input file name
!
!     work folder
!
!     intrinsic functions
  intrinsic :: len_trim

!     init
!
!     return
  ok = -1
!
!     export kind
  iex = -1
!     work folder name
  wrl = blank
!     file log -l
  llg = .false.
!     version -v
  ver = .false.
!     help -h
  hlp = .false.
!     message dump
  dmp = .false.
!     standard io
  std = .false.
!
!     opt(1)  cpb campbell -c
!     opt(2)  mds mode shapes -m
!     opt(3)  fqr frequencia response -f
!     opt(4)  tmr time response -t
!     opt(5)  fng input file set -i
!     opt(6)  exp geom file export -e
!     opt(7)  use speed dep. bearing params
!     opt(8)  static elastic line calc. -s
!     opt(9)  undamped critical speed map -k
!     opt(10) bearing support -b
!     opt(11) standard i/o -std
!     opt(12) none
!     opt(13) none
!     opt(14) output HPGL plot -p
!     opt(15) undamped angular critical speed map -a
!     opt(16) variable speed bearing parameters -j
!     opt(17) override messages -o
!     opt(18) torsion option
!     opt(19) torsion time transient -n
!     opt(20) flexural-torsion -r
!     iex export kind flag = 0 HB, 1=mm,2=plain
!
!     check for some calculation option set
!      *** opt check ***
!     see also on rotordin.f and saidas.f entrada.f
!
!     opt array init on blockd.f
!
!     number of command line arguments
!     check compiler support
  ncm = command_argument_count()
!
!     executable folder
  call rd_getcwd(lcl)
!
!     check stdio
  do i = 1,ncm
!       get i th cmd line argument
    call get_command_argument(i,buf)
!       argument length space striped
    j = len_trim(buf)
!       standard i/o
    std = buf(1:j) .eq. cstd
    if (std) exit
  end do
!
!     standard i/o
  opt(11) = std
!
!     check for arguments
  if (ncm .gt. 0) then
!
!       get each argument
    i = 0
10  i = i+1
!
!         valid option flag
    vld = .false.
!
!         get i th cmd line argument
    call get_command_argument(i,buf)
!
!         argument length space striped
    j = len_trim(buf)
!         signal check
    msi = buf(1:1) .eq. dash
!
!         export HB, default export action with -x
    if (buf(1:j) .eq. '--transient') then
      call transient_set_cli(.true.)
      vld=.true.
!         export HB, default export action with -x
    else if (msi .and. buf(2:j) .eq. vopt(12)) then
!           next argument could be HB, mm or pl
      call get_command_argument(i+1,msk)
      k = len_trim(msk)
!           matrix market format
      bpl = msk(1:k) .eq. cex(1)
!           Harwell-Boeing format
      lok = msk(1:k) .eq. cex(2)
!           plain format
      vld = msk(1:k) .eq. cex(3)
!           if valid, increment argument index
!           matrix market or Harwell-Boeing or plain format
      if (bpl .or. lok .or. vld) i = i+1
!           plain
      if (.not. (lok .or. bpl)) iex = 2
!           Harwell-Boeing format
      if (lok) iex = 0
!           export matrix market
      if (bpl) iex = 1
!           this is a valid argument
      vld = .not. iex .lt. 0
!         override messages
    else if (msi .and. buf(2:j) .eq. oopt(7)) then
!           next argument message file or 'd' for dumping
      call get_command_argument(i+1,msk)
      k = len_trim(msk)
      if (.not. std) then
!             file i/o, check if given file name exists
        inquire(file=msk(1:k),exist=bpl)
        if (bpl) then
!               message override file exists, open
          open(10,file=msk(1:k),err=100)
          call loadmsg(10,.false.)
          close(10)
!               valid, increment argument index
          i = i+1
          opt(17) = .true.
!               this is a valid argument
          vld = .true.
        end if
!             not dumping messages -o d option
      else
!             standard i/o
        opt(17) = .true.
!             this is a valid argument
        vld = .true.
      end if
!           check for dumping messages
      if (k .eq. 1 .and. msk(1:1) .eq. gsb(4)) then
!             valid, increment argument index
        i = i+1
!             this is a valid argument
        vld = .true.
!             dump messages
        dmp = .true.
      end if
!         use bearing support data
    else if (msi .and. buf(2:j) .eq. vopt(10)) then
      opt(10) = .true.
      vld = .true.
!         variable speed bearing parameters plot
    else if (msi .and. buf(2:j) .eq. vopt(16)) then
      opt(16) = .true.
      vld = .true.
!         campbell
    else if (msi .and. buf(2:j) .eq. vopt(1)) then
      opt(1) = .true.
      vld = .true.
!         mode shape
    else if (msi .and. buf(2:j) .eq. vopt(2)) then
      opt(2) = .true.
      vld = .true.
!         torsion time transient
    else if (msi .and. buf(2:j) .eq. vopt(19)) then
      opt(19) = .true.
      vld = .true.
!         flexural-torsion
    else if (msi .and. buf(2:j) .eq. vopt(20)) then
      opt(20) = .true.
      vld = .true.
!         HPGL plot
    else if (msi .and. buf(2:j) .eq. vopt(14)) then
      opt(14) = .true.
      vld = .true.
!         frequency resp
    else if (msi .and. buf(2:j) .eq. vopt(3)) then
      opt(3) = .true.
      vld = .true.
!         time resp
    else if (msi .and. buf(2:j) .eq. vopt(4)) then
      opt(4) = .true.
      vld = .true.
!         undamped critical speed map
    else if (msi .and. buf(2:j) .eq. vopt(9)) then
      opt(9) = .true.
      vld = .true.
!         undamped angular critical speed map
    else if (msi .and. buf(2:j) .eq. vopt(15)) then
      opt(15) = .true.
      vld = .true.
!         static elastic line
    else if (msi .and. buf(2:j) .eq. vopt(8)) then
      opt(8) = .true.
      vld = .true.
!         use log file
    else if (msi .and. buf(2:j) .eq. oopt(2)) then
      llg = .true.
      vld = .true.
!         export geometry
    else if (msi .and. buf(2:j) .eq. vopt(6)) then
      opt(6) = .true.
      vld = .true.
!         standard i/o
    else if (buf(1:j) .eq. cstd) then
      vld = .true.
      ifn = 'stdio'
!         help request
    else if (msi .and. buf(2:j) .eq. oopt(1)) then
      hlp = .true.
      vld = .true.
!         version request
    else if (msi .and. buf(2:j) .eq. oopt(3)) then
      ver = .true.
      vld = .true.
!         user wants to inform the complete input file name
    else if (msi .and. buf(2:j) .eq. oopt(4)) then
!           not stdio
      if (.not. std) then
        i = i+1
!             next argument should be a file name
        call get_command_argument(i,buf)
!             check the file name (not empty argument)
        if (buf(1:1) .gt. blank) then
!               set the input file name
          ifn = buf
!               check if file name argument exists
          j = len_trim(buf)
!               opt(5) = input file set
          inquire(file = buf(1:j), exist = opt(5))
        else
!               local error set
          ler = .true.
          errmsg = femsgf(99, nmm,6,23,0)
        end if
      else
!             stdio
        ler = .true.
        errmsg = femsgf(99, nmm,6,21,0)
      end if
!           this is a valid argument
      vld = .true.
!           -i if
!         check for i/o folder name (work folder)
!         defines the output(/input) <fold>er name
    else if (msi .and. buf(2:j) .eq. oopt(5)) then
!           not stdio
      if (.not. std) then
        i = i+1
!             next argument should be the work folder name
        call get_command_argument(i,buf)
!             check if name argument exists
        if (buf(1:1) .gt. blank) then
!                check if folder is writeable
          call chkfld(buf,j)
          !              ok
          if (j .gt. 0) then
!                 flag for user folder
            wfl = .true.
!                 set  working folder
            wrl = buf
          else
!                 problem with folder's name
            ler = .true.
            errmsg = fomsgf(99, nmm,26,buf,1)
          end if
        else
!               folder name is empty
          ler = .true.
          errmsg = femsgf(99, nmm,6,22,0)
        end if
      else
!             stdio
        ler = .true.
        errmsg = femsgf(99, nmm,6,21,0)
      end if
!           this is a valid argument
      vld = .true.
!           -d if
!         empty input file mask
    else if (msi .and. buf(2:j) .eq. oopt(6)) then
!           not std io redirection
      if (.not. std) then
!             init sub option
        gso = blank
        i = i+1
!             next argument should be the input file name
!             or specific mask files sub option
        call get_command_argument(i,buf)
!             there was one more argument? get length
        j = len_trim(buf)
        if (j .gt. 0) then
!               check the size
          if (j .eq. 1 .and.&
          &(buf(1:1) .eq. gsb(1) .or.&
          &buf(1:1) .eq. gsb(2) .or.&
          &buf(1:1) .eq. gsb(3))) then
!                 keep sub option
            gso = buf(1:1)
            i = i+1
!                 next argument should be the input file name
            call get_command_argument(i,buf)
!                 check if file name argument exists, get length
            j = len_trim(buf)
            if (j .gt. 1) then
!                   ok, input mask file name
              msk = buf
              gmf = .true.
            else
!                   ko, too short to consider file name > 1
              ler = .true.
              errmsg = femsgf(99, nmm,6,24,0)
            end if
          else
!                 ok, input mask file name
            msk = buf
            gmf = .true.
          end if
!               input mask file name
          msk = buf
          gmf = .true.
        else
!               no second argument
          ler = .true.
          errmsg = femsgf(99, nmm,6,24,0)
        end if
!             std if
      else
!             -g std option
!             specific mask files sub option
        call get_command_argument(i+1,buf)
!             there was one more argument? get length
        j = len_trim(buf)
!             valid sub option?
        lok = (j .eq. 1 .and.&
        &(buf(1:1) .eq. gsb(1) .or.&
        &buf(1:1) .eq. gsb(2) .or.&
        &buf(1:1) .eq. gsb(3)))
!             check valid sub option
        if (lok) then
!               valid
!               keep sub option
          gso = buf(1:1)
!               increment argument count
          i = i+1
        end if
!             want to generate input masks
        gmf = lok .or. j .eq. 0 .or. msi
!             std if
      end if
      vld = .true.
!           -g if
    end if
!
!         check if the argument is an input file name
    if ( .not. (vld .or. std)) then
      j = len_trim(buf)
      inquire(file = buf(1:j),exist = opt(5))
      vld = opt(5)
      ifn = buf
    end if
!
!         loop on arguments (ugly goto, sorry)
    if (i .lt. ncm .and. vld) goto 10
!
!       ncm arguments if
  end if
!
!      *** opt check ***
!
!     bearing variable speed parameters plot
!     should have also variable parameters plot option -p
  bpl = opt(16) .and. opt(7) .and. opt(14)
!     check for some calculation option set
!     see also on rotordin.f
  if (hlp .or. ver .or. dmp .or.&
  &(.not.&
  &(opt(1) .or. opt(2) .or. opt(3) .or.&
  &opt(4) .or. opt(6) .or. opt(8) .or.&
  &opt(9) .or. opt(12) .or. opt(13) .or.&
  &opt(15) .or. bpl .or. gmf .or.&
  &transient_cli_requested_f())&
  &)) then
    if (ver) then
!         version request
      errmsg = fhmsgf(99, nmm,27,blank,0)
      return
    else if (hlp) then
!         help request
      errmsg = fhmsgf(99, nmm,28,blank,0)
      return
    else if (dmp) then
!         dump messages
      errmsg = fhmsgf(99, nmm,30,blank,0)
      return
    else
!         check for valid option
      if (.not. vld .and. ncm .gt. 0) then
!           screen help
        hlp =  .true.
        errmsg = fomsgf(99, nmm,6,buf,1)
        return
      else
!           it is a local error?
        if ( .not. ler ) then
!             ok goes below
          ok = 0
        else
!             get out in case of local error
          return
        end if
      end if
!
!         valid option if
    end if
    !
  else
    if (ler) then
!         local error
      return
    else if (.not. vld .and. ncm .gt. 0) then
      hlp = .true.
      errmsg = fomsgf(99, nmm,6,buf,1)
      return
    end if
  end if
!
!     check mask file generation
  if (gmf) then
!       mask file generation
    call chkmsk(msk,ifn,wrl,lcl,wfl,gso,std,errmsg,ok)
    return
  end if
!
!     user not using standard i/o
  if (.not. std) then
!       if user has not defined the input file name
    if (ifn(1:1) .le. blank .or. .not. opt(5)) then
      ok = -1
      call findif(lcl,ifn,ein,wrl,opt(5))
      if (ifn(1:1) .le. blank) then
        errmsg = fomsgf(99, nmm,2,ein,1)
        return
      else
        if (.not. opt(5)) then
          errmsg = fomsgf(99, nmm,2,ifn,1)
          return
        end if
      end if
    end if
!
!       user work folder set
    if (wrl(1:1) .le. blank) then
!         if input file name is ok
!         set work folder to input's folder
      if (ifn(1:1) .gt. blank) call wrklcl(ifn,wrl)
    end if
!
!       check input file and work folder names
    if (ifn(1:1) .le. blank .or. wrl(1:1) .le. blank) then
      if (ifn(1:1) .le. blank) then
!           file name
        errmsg = femsgf(99, nmm,6,23,0)
      end if
      if (wrl(1:1) .le. blank) then
!           folder name
        errmsg = femsgf(99, nmm,6,23,0)
      end if
      return
    end if
!
!       base name for output files
    call basnam(ifn,wrl,bsn)
!       std if
  end if
!
!     return ok
  ok = 0
  return
!
!     unable to open message file
100 errmsg = fomsgf(99, nmm,1,msk,1)
  return
  !
end subroutine cmdarg
!
!     ==================================================================
!>    @brief get system's date and time.
!>
!>    @param[out] ds returns formatted date "dd/mm/yyyy"
!>    @param[out] ts returns formatted time "hh:mm:ss.d"
!
subroutine sdate(ds,ts)
  implicit none
!
!     arguments
  character(len=10) :: ds, ts
!
!     locals
  integer :: ii
  character(len=1) :: ddot, slash, dash
  character(len=2) :: mt, mn
  character(len=10) :: dt, tm
!
!     JA  January  Unlike "JN," "JA" can't be confused with any possible
!     FE  February
!     MR  March   Using just "MA" could be confused with May
!     AP  April
!     MY  May     Using just "MA" could be confused with March
!     JN  June    "JU" can't be used because it's ambiguous with the pos
!     JL  July    "JU" can't be used because it's ambiguous with the pos
!     AU  August
!     SE  September
!     OC  October
!     NV  November    "NO" was rejected to prevent any confusion with th
!     DE  December
  dimension mn(12)
  parameter (ddot = ':',slash = '/',dash= '-',&
  &mn = (/'JA','FE','MR','AP','MY','JN',&
  &'JL','AU','SE','OC','NV','DE'/))
  data mt/dash/
!
  intrinsic :: date_and_time
!
!     check compiler
  call date_and_time(dt,tm)
!     format
  write(ts,5) tm(1:2),ddot,tm(3:4),ddot,tm(5:8)
!     read month number
  read(dt(5:6),15,err=10) ii
  if (ii .gt. 0 .and. ii .le. 12) mt = mn(ii)
!     format
  write(ds,5) dt(7:8),dash,mt,dash,dt(1:4)
!
  return
!
10 write(ds,5) dt(7:8),slash,dt(5:6),slash,dt(1:4)
!
  return
!
5 format(5a)
15 format(i2)
!
end subroutine sdate
!
!     ==================================================================
!>    @brief generates header stamp

!>    @param[out] sta return date and id stamp
!
subroutine stamp(sta)
  use com_lif, only: usr, dtu, tmu
  use rd_kinds, only: lrk
  implicit none
!
!     arguments
  character(len=*) :: sta
!
!     locals
  real(lrk) :: s, p, w
  integer :: i, j, l
  character(len=1) :: clb
  character(len=3) :: sky
  character(len=6) :: nky, skey, bkey
  character(len=8) :: stm
  character(len=50) :: cst
  dimension clb(5)
  parameter (p = 1.011_lrk, w = 9e5_lrk, l = 500 ,clb = (/' ','@','-','[',']'/),sky = 'key',nky = 'no-key')
!
!     user info
!
  intrinsic :: int, log10, len, len_trim, min
!
!     init
  s = 3.14159_lrk
  cst = clb(1)
!
!     current time
  write(stm,35,err=10)tmu(8:),tmu(4:5),tmu(1:2)
!
!     number based on current time
  read(stm,5,err=10) s
10 i = 0
  do while (s .lt. w .and. i .lt. l)
    i = i+1
    s = s**p
    if (s .lt. 1) s = w**(1/p)
  end do
  if (s .gt. w) s = s**(1/p)
!
!     key text generation
  write(bkey,15,err=20) int(s)
  write(skey,35,err=30) bkey(5:6),bkey(1:2),bkey(3:4)
!
!     number length
20 if (s .gt. 0) then
    i = 6-int(log10(s))
    if (i.gt. 0 .and. i .lt. 7)&
    &write(cst,25,err=30) dtu,clb(2),tmu,&
    &clb(3),sky,clb(4),skey(i:),clb(5)
  end if
!
!     on error will be blank
30 if (cst(1:1) .le. clb(1))&
  &write(cst,35,err=40) dtu,clb(2),tmu,&
  &clb(3),sky,clb(4),nky,clb(5)
!     check size
40 i = len(sta)
  j = len_trim(cst)
  i = min(i,j)
  if (i .gt. 0) sta = cst(1:i)
!
  return
!
5 format(f8.0)
15 format(i6)
25 format(3a,1x,a,1x,a,1x,3a)
35 format(3a)
!
end subroutine stamp
!
!     ==================================================================
!>    @brief user info
!
!>    @param usr return the current user name
!
subroutine getusr(usr)
  implicit none
!
!     arguments
  character(len=*) :: usr
!
!     locals
  integer :: i, j
  character(len=1) :: blank
  character(len=4) :: cusr
  character(len=8) :: cunv
  dimension cusr(2),cunv(2)
  parameter (blank = ' ',&
  &cusr = (/'USER','user'/),&
  &cunv = (/'USERNAME','username'/))
  character(len=255) :: buf
!
  intrinsic :: len
!
  j = len(usr)
  if (j .gt. 0 .and. j .lt. 256) then
    usr = blank
!       user name from environment -> buf
!       changed to upper - francisco - feb-19
!       try upper and lower case - franciso - feb-21
    do i = 1,2
!         try "username"
      call get_environment_variable(cunv(i),buf)
      if(buf(1:1) .gt. blank) exit
!         try "user"
      call get_environment_variable(cusr(i),buf)
      if(buf(1:1) .gt. blank) exit
    end do
!       limit length
    usr = buf(1:j)
  end if
!
  return
!
end subroutine getusr
!
!     ==================================================================
!>    @brief main init.
!
!>    @param[in] std standard input output
!>    @param[out] lgn return flag for log writing
!>           <code>positive number</code> unit to write log
!>           <code>negative</code> does not write log file
!>    @param[out] iex export kind flag = 0 HB, 1=mm,2=plain

!>    @param[out] errmsg return an error message
!>    @param[out] ok return flag. unsuccessful if <0.
!>    @see saidas.f
!
subroutine init(std,lgn,iex,errmsg,ok)
  use rd_textfun, only: fomsgf
  use com_lgf, only: ilg
  use com_lif, only: usr, dtu, tmu
  use com_msb, only: lid, msl
  use com_sta, only: stm
  implicit none
!
!     arguments
  integer :: lgn, iex, ok
  logical :: std
  character(len=99) :: errmsg
!
!     locals
  integer :: i, oki
  character(len=1) :: blank
  character(len=4) :: nmm
  character(len=14) :: cmsg
  parameter(blank = ' ',nmm = 'init',cmsg = 'unable to dump')
!     changed var name log to llg
  logical :: llg, ver, hlp, dmp
!
!     user info
!
!     stamp
!
!     log
  integer :: mxlg
  parameter(mxlg = 20)
!
!     log flag
!
!     return init
  ok = -1
  oki = 0
  lgn = 0
!
!     try get the username
  call getusr(usr)
!
!     date and time
  call sdate(dtu,tmu)
!
!     stamp with random id
  call stamp(stm)
!
!     process command line arguments, will set ok
  call cmdarg(llg,ver,hlp,dmp,std,iex,errmsg,ok)
!
!     check for version ask
  if (.not. ver) then
!       not stdio print application info
    if (.not. std) call printin()
    if (hlp) then
!         help request
      call help()
    else if (dmp) then
!         dump messages, see saidas.f
      call s_dump(std,oki)
      if (oki .ne. 0) then
!           most likely i/o error
! TODO: sartori - erro de tipo, deve ser arrumado, alterado provisoriame
!           errmsg = fomsgf(nmm,cmsg,blank,0)
        errmsg = fomsgf(99, nmm,0   ,cmsg,0)
      end if
    end if
  else
!       -v, print version info
!        screen program info
    call printvr()
  end if
!
!     log file
  if (llg) then
!       init log buffer
!       log index line (see saidas.f)
    lid = 1
!       clear log buffer
    do i = 1,mxlg
      msl(i) = blank
    end do
!       log flag set
    lgn = 1
    ilg = 1
  end if
!
  return
!
end subroutine init
!
!     ==================================================================
!>    @brief valid options known after data input.
!
!>    @param[in] lgn return flag for log writing
!>           <code>positive number</code> unit to write log
!>           <code>negative</code> does not write log file
!>    @param[in] mop dimension of option leters vector
!>    @param[in] iex export kind flag = 0 HB, 1=mm,2=plain
!>    @param[in] opt option leters vector
!>    @param[in] inm application internal name
!>    @see rotordin.f,entrada.f,init
!
subroutine pinit(lgn,mop,iex,opt,inm)
  use rd_textfun, only: fomsgf, warnmsgf
  use com_vldo, only: vopt
  implicit none
!
  integer :: lgn, mop, iex
  logical :: opt
  character(len=*) :: inm
  dimension opt(mop)
!
!     locals
  logical :: tors, sok, isbrtbf, transient_requested_f
  integer :: cp
  character(len=1) :: blank, cm
  character(len=2) :: co
  character(len=99) :: msg
  parameter(blank = ' ',cm = '-')
  data cp/0/
!
!     valid options, see blockd.f,init.f
  integer :: jvop
  parameter (jvop = 20)
!
!     torsional option
  tors = opt(18)
!
!     torsion only option on lateral problem
  if ((opt(19) .or. opt(20)) .and. .not. tors) then
!       screen help message
    call help()
    if (opt(19)) then
!         torsion time transient
      write(co,5) cm,vopt(19)
    else if (opt(20)) then
!         flexural-torsion
      write(co,5) cm,vopt(20)
    end if
!       ':torsion only' !42
    write(msg,5) co,warnmsgf(50, 42)
    msg = fomsgf(50, inm,6,msg,0)
!       invalid option, 1 -> exit
    call emsg(lgn,1,msg)
  end if
!
!     opt(1) campbell
!     opt(2) modes
!     opt(3) frequency response
!     opt(4) time response
!     opt(5) input file name set
!     opt(6) geometry export
!     opt(7) speed dependent bearing parameters
!     opt(8) static elastic line
!     opt(9) undamped critical speed map
!     opt(10) bearing support
!     opt(11) standard i/o
!     opt(12) none
!     opt(13) none
!     opt(14) HPGL plot output
!     opt(15) undamped angular critical speed map
!     opt(16) variable speed bearing parameters plot
!     opt(17) override messages -o
!     opt(18) torsion option
!     opt(19) torsion time transient
!     opt(20) flexural-torsion
!     iex export kind flag = 0 HB, 1=mm,2=plain
!
!     check for calculation options, see also init.f
  sok = opt(1) .or. opt(2) .or. opt(3) .or. opt(4) .or.&
  &opt(6) .or. opt(8) .or. opt(9) .or.&
!     export
  &(.not. iex .lt. 0) .or.&
  &opt(15) .or. opt(16) .or.&
  &(tors .and. opt(19)) .or.&
  &(tors .and. opt(20)) .or. transient_requested_f()
!
  if (.not. sok) then
!       screen help message
    call help()
!       nothing to do
    msg = fomsgf(50, inm,25,blank,0)
!       1 -> exit
    call emsg(lgn,1,msg)
  end if
!
!     bearing variable speed parameters plot
!     get number of variable bearings -> cp
  sok = isbrtbf(cp)
!     check if user request output variable bearing parameter
!     has variable parameters flag set and there are defined
!     or not request it
  sok = (opt(16) .and. opt(7) .and. cp .gt. 0) .or. .not. opt(16)
  if (.not. sok) then
!       'no suitable bearing' !34
    msg = fomsgf(50, inm,34,blank,0)
!       1 -> exit
    call emsg(lgn,1,msg)
  end if
!
  return
!
5 format(2a)
!
end subroutine pinit
!
