!     $Id$
!     ==================================================================
!
!>    @file hpgl.f
!>    @author various, last francisco.
!>    @date 27-may-15
!>    @brief hpgl plot support, last changes:<br>
!>    new module - francisco - 27-may-15<br>
!>    added legend plot routine<br>
!>    added ltype - francisco - apr-19<br>
!>    added axis grid control - francisco - jan-20<br>
!>    added negative number control on log axis - francisco - feb-20<br>
!>    added force zero axis line drawing control, fzax block,
!>    see malhapx - francisco - oct-20<br>
!>    added getaxbf axis bound limits, changed axis routines to hold - f
!
!     ==================================================================
!>    @brief initilize plot common blocks.
!
!
!     ==================================================================
!>    @brief set plot grid control variables.
!>    true at initilization. Change is permanent. See blockhp, getgrid.
!
!>    @param[in] vert plot vertical grid state
!>    @param[in] horiz plot horizontal grid state
!
subroutine setgrid(vert,horiz)
  use com_grids, only: hgrid, vgrid
  implicit none
!
  logical :: vert, horiz
!
!
  hgrid = horiz
  vgrid = vert
!
  return
!
end subroutine setgrid
!
!     ==================================================================
!>    @brief get plot grid control variables.
!>    See setgrid.
!
!>    @param[out] vert plot vertical grid state
!>    @param[out] horiz plot horizontal grid state
!
subroutine getgrid(vert,horiz)
  use com_grids, only: hgrid, vgrid
  implicit none
!
  logical :: vert, horiz
!
!
  horiz = hgrid
  vert = vgrid
!
  return
!
end subroutine getgrid
!
!     ==================================================================
!>    @brief set plot grids to on
subroutine gridson()
  call setgrid(.true.,.true.)
end subroutine gridson
!
!     ==================================================================
!>    @brief set plot grids to off
subroutine gridsoff()
  call setgrid(.false.,.false.)
end subroutine gridsoff
!
!     ==================================================================
!>    @brief set force zero axis to pbe plot.
!>    false at initilization. Change is permanent. See blockhp.
!>    @param[in] iax axis index, 1=1,2=y.
!>    @param[in] lst axis force status value
subroutine forzax(iax,lst)
  use com_frax, only: fzax
  implicit none
!
  integer :: iax
  logical :: lst
!
!     force axis
!
  if(iax .gt. 0 .and. iax .lt. 3) fzax(iax) = lst
!
  return
!
end subroutine forzax
!
!     ==================================================================
!>    @brief open output plot file, or console if std, search for
!>    first non used file handle number less than 100, <br>
!>    if console output (std) use standard number six (6).
!
!>    @param[in] idum 0 to open other closes
!>    @param[in] jdum dummy integer
!>    @param[in] dname output file name
!
subroutine plots(idum, jdum, dname)
  use com_blocp, only: xursp, yursp, xsymb, ysymb, fac, xwhere, ywhere, xfac, yfac, xver, yver, ldev
  use com_device, only: idev
  use com_opt, only: std
  use rd_kinds, only: lrk
  implicit none
!
!     Last change:  M.A   9 Mar 98    2:13 am
!
!     ES FOLGEN ALLE FUER MALHAP BENOETIGTEN UNTERPROGRAMME AUS PLTHPGL2
!
  integer :: idum, jdum
  character(len=*) :: dname
!
  logical :: offen
  character(len=5) :: nmm
  parameter(nmm = 'plots')
!
!     idev = ldev
!
!
!     warning workaround
  offen = jdum .eq. 0
!     reset scaling
  if (idum .eq. 0) call sreset(3)
!     plot file handle
  if (.not.std) then
    if (idum .eq. 0) then
      ldev = 99
!         loop to try find an available file handle channel
!         check if ldev is already open
10    inquire (ldev,opened=offen)
!         ldev is already open
      if (offen) then
!           decrease the file handle channel number
        ldev = ldev-1
!           come down to standard io channel
        if (ldev .lt. 7) then
          call elmsge(1,1,nmm)
        end if
        go to 10
      end if
!         file handle OK
!         common to output buffer
      idev = ldev
      open (ldev,file=dname)
      rewind (ldev)
      call initimark
    else
!         check for standard io
      call bflush
      if (ldev .ne. 6) close (ldev)
    end if
!     plot to standard io
  else
    if (idum .eq. 0) then
      ldev = 6
!         common to output buffer
      idev = ldev
      call initimark
    else
      call bflush
    end if
  end if
!
  return
!
end subroutine plots
!
!     ==================================================================
!>    @brief direct text buffer, will be send to the buffered output.
!
!>    @param[in] sbuf input text, limitied length
!
subroutine swrite(sbuf)
  implicit none
!
  character(len=*) :: sbuf
!
  character(len=6) :: nmm
  character(len=128) :: buf
  integer :: i
  parameter(nmm = 'swrite')
!
  intrinsic :: len_trim
!
  i = len_trim(sbuf)
!     maximaum size check
  if (i .gt. 128) then
    call elmsgw(0,2,nmm)
    i = 128
  end if
!     copy
  buf = sbuf(1:i)
!     call buffered write
  call bwrite(buf,0)
!
  return
!
end subroutine swrite
!
!     ==================================================================
!>    @brief to write buffer to output device and flush.
!
subroutine bflush()
  implicit none
!     local
  character(len=1) :: blank
  character(len=128) :: buf
  parameter (blank = ' ')
!
  buf = blank
  call bwrite(buf,1)
!
  return
!
end subroutine bflush
!
!     ==================================================================
!>    @brief output buffer,when buffered content is greater than a size
!>           then buffered output is write to output device and flushed,
!>           one can flush anytime using iflush.
!
!>    @param[in] buf input text, limitied length.
!>    @param[in] iflush flush buffer writing it to output device
!
subroutine bwrite(buf,iflush)
  use com_device, only: idev
  implicit none
!     arguments
  integer :: iflush
  character(len=128) :: buf
!
  integer :: i, j
!     check if below
  character(len=128) :: lbuf
  save lbuf
  data lbuf/' '/
!
!     idev = ldev
!
  intrinsic :: len_trim
!
  i = len_trim(buf)
  j = len_trim(lbuf)
!
  if (i+j .gt. 128 .or. iflush .eq. 1) then
    write(idev,5) lbuf(1:j)
    lbuf = buf(1:i)
  else
!       append
    write(lbuf,15) lbuf(1:j),buf(1:i)
  end if
!
  return
!
5 format(a)
15 format(2a)
!
end subroutine bwrite
!
!     ==================================================================
!>    @brief initilialize plot page mark.
!
subroutine initimark()
  use com_pmrk, only: imerk
  implicit none
!
!
  imerk = 0
!
end subroutine initimark
!
!     ==================================================================
!>    @brief initilialize plot page mark
!
!>    @param[in] iunit output file unit
!>    @param[in] jj dummy
!>    @param[in] x dummy
!>    @param[in] y dummy
!>    @param[in] ianz dummy
!
subroutine newplot(iunit, jj, x, y, ianz)
  use com_blocp, only: xursp, yursp, xsymb, ysymb, fac, xwhere, ywhere, xfac, yfac, xver, yver, ldev
  use com_opt, only: std
  use com_pmrk, only: imerk
  use rd_kinds, only: lrk
  implicit none
!
!     NEWPLOT OEFFNET NEUEN RAHMEN
!
  integer :: iunit, jj, ianz
  real(lrk) :: x, y
!
  character(len=128) :: sdev
!
!
!
!
  logical :: wk
  character(len=3) :: c3
  character(len=10) :: c10
  parameter (c3 = 'PG;',c10 = 'IN;RO 270;')
!
!
!      warning workaround
  wk = x .eq. y .or. iunit .eq. jj .or. ianz .eq. 0
  if (imerk .eq. 0) then
!       not needed anymore
  else
!       Seitenvorschub ...
!       direct string buffer
    call swrite(c3)
  end if
!
  imerk = imerk+1
!
!     call buffered write
  write (sdev,10) imerk
  call bwrite(sdev,0)
!
  xfac = 400._lrk
  yfac = 400._lrk
  xursp = 0._lrk
  yursp = 0._lrk
  xsymb = 0._lrk
  ysymb = 0._lrk
  xwhere = 0._lrk
  ywhere = 0._lrk
!
!     ... Seite initialisieren
!     direct string buffer
  call swrite(c10)
!
  xver = 0._lrk
  yver = 0._lrk
  fac = 1._lrk
!
!     Nullpunkt einstellen fuer MALHAP-Plots ...
  call plot (0._lrk, -1._lrk, -3)
  call sbreite (0.22_lrk)
!
  return
!
10 format ('CO"',10x,'--> Anfang von Rahmen Nr. ',i2,' <--',10x,'"')
!
end subroutine newplot
!
!     ==================================================================
!>    @brief HPGL plot command.
!
!>    @param[in] xpag x position in cm
!>    @param[in] ypag y position in cm
!>    @param[in] ipen pen position, 2 down other up, 999 page feed
!>     and close output device
!
subroutine plot(xpag, ypag, ipen)
  use com_blocp, only: xursp, yursp, xsymb, ysymb, fac, xwhere, ywhere, xfac, yfac, xver, yver, ldev
  use rd_kinds, only: lrk
  implicit none
!
!     PLOT FUEHRT EINE STIFTBEWEGUNG DURCH ODER SCHLIESST DIE PLOTDATEI
!
  real(lrk) :: xpag, ypag
  integer :: ipen
  character(len=128) :: sdev
!
  real(lrk) :: x, xpage, y, ypage
  character(len=3) :: c3
  parameter (c3 = 'PG;')
!
!
  intrinsic :: abs
!
  xpage=xpag*fac*xfac+xursp
  ypage=ypag*fac*yfac+yursp
!
  xwhere=xpage
  ywhere=ypage
!
  x=xpage-xver
  y=ypage-yver
!
  if (abs(ipen).eq.2) then
!       call buffered write
    write (sdev,10) x,y
    call bwrite(sdev,0)
  else
    write (sdev,20) x,y
    call bwrite(sdev,0)
  end if
!
  if (ipen.lt.0) then
    xursp=xpage
    yursp=ypage
  end if
!
  if (ipen.eq.999) then
!
!     GRAFIK-MODUS AUS UND SEITENVORSCHUB
!
!       direct string buffer
    call swrite(c3)
!       check for standard io
    if (ldev .ne. 6) close (ldev)

  end if
!
  return
!
10 format ('PD ',f7.1,',',f7.1,';')
20 format ('PU ',f7.1,',',f7.1,';')
!
end subroutine plot
!
!     ==================================================================
!>    @brief HPGL plot command.
!
!>    @param[in] number HPGL pen number, controls also the color,
!>     default Pen colors: 1:Black, 2:Red, 3:Green, 4:Yellow, 5:Blue,
!>     6:Magenta
!
subroutine pen(number)
  use com_blocp, only: xursp, yursp, xsymb, ysymb, fac, xwhere, ywhere, xfac, yfac, xver, yver, ldev
  use rd_kinds, only: lrk
  implicit none
!
!     PEN SELECTION
!
  integer :: number
!
  character(len=128) :: sdev
!
!
!     call buffered write
  write (sdev,10) number
  call bwrite(sdev,0)
!
  return
!
10 format ('SP',i2,';')
!
end subroutine pen
!
!     ==================================================================
!>    @brief set a global scaling factor
!
!>    @param[in] ffac scaling factor
!
subroutine factor(ffac)
  use com_blocp, only: xursp, yursp, xsymb, ysymb, fac, xwhere, ywhere, xfac, yfac, xver, yver, ldev
  use rd_kinds, only: lrk
  implicit none
!
!     FACTOR STELLT DEN VERGROESSERUNGSFAKTOR EIN
!
  real(lrk) :: ffac
!
!
  fac=ffac
!
  return
!
end subroutine factor
!
!     ==================================================================
!>    @brief set the line width, please note that not all viewers
!>     supports this option.
!
!>    @param[in] strbr line width
!
subroutine sbreite(strbr)
  use com_blocp, only: xursp, yursp, xsymb, ysymb, fac, xwhere, ywhere, xfac, yfac, xver, yver, ldev
  use rd_kinds, only: lrk
  implicit none
!
!     SBREITE STELLT DIE STRICHBREITE EIN
!
  real(lrk) :: strbr
!
  character(len=128) :: sdev
!
!
!     WAHL DER STIFTBREITE FUER LASER-PRINTER IN mm
!
!     call buffered write
  write (sdev,10) strbr
  call bwrite(sdev,0)
!
  return
!
10 format ('PW',f6.2,';')
!
end subroutine sbreite
!
!     ==================================================================
!>    @brief draw symbols.
!
!>    @param[in] xpag x position in cm
!>    @param[in] ypag y position in cm
!>    @param[in] height symbol height
!>    @param[in] ibcd symbol characters, ascii 3 is the terminator.
!>    @param[in] inteq
!>    @param[in] angle rotation angle
!>    @param[in] nchar number of characters in symbol
!
subroutine symbol(xpag, ypag, height, ibcd, inteq, angle, nchar)
  use com_blocp, only: xursp, yursp, xsymb, ysymb, fac, xwhere, ywhere, xfac, yfac, xver, yver, ldev
  use rd_kinds, only: lrk
  implicit none
!
!     SYMBOL SCHREIBT EIN ODER MEHR ZEICHENEN
!
  real(lrk) :: xpag, ypag, height, angle
  integer :: inteq, nchar
  character(len=*) :: ibcd
  character(len=80) :: jchar
!
  character(len=1) :: term, smc
  character(len=2) :: c2
  parameter (smc = ';',term = char(3),c2 = 'LB')
  character(len=128) :: sdev
!
  real(lrk) :: toradf
  real(lrk) :: h, h1, h2, h3, h4, h5, hei, hei1, r, x, x1, xpage, y, y1, ypage
  integer :: nn
!
!
  intrinsic :: cos, nint, sin
!
  r = 0.0_lrk
  if (nchar .le. 0) then
    h = height*fac*xfac
    h1 = h*(-1._lrk)
    h2 = h/2._lrk
    h3 = h2*(-1._lrk)
    h4 = h/4._lrk
    h5 = h4*(-1._lrk)
!
    if (nint(xpag) .eq. 999) then
      xpage = xsymb
    else
      xpage = xpag*fac*xfac+xursp
    end if
!
    if (nint(ypag) .eq. 999) then
      ypage = ysymb
    else
      ypage = ypag*fac*yfac+yursp
    end if
!
    x = xpage-xver
    y = ypage-yver
!
!       call buffered write
    write (sdev,10) x,y
    call bwrite(sdev,0)
!
    if (inteq .eq. 0) then
      write (sdev,20) r,h3,h3,r,r,h,h,r,r,h1,h3,r
    else if (inteq .eq. 1) then
      write (sdev,30) r,h3,h5,r,h5,h4,r,h2,h4,h4,h2,r,h4,h5,r,h3,h5,&
      &h5,h5,r
    else if (inteq .eq. 2) then
      write (sdev,40) r,h3,h3,h*3._lrk/4._lrk,h,r,h3,h1*3._lrk/4._lrk
    else if (inteq .eq. 3) then
      write (sdev,50) r,h3,r,h,h3,h3,h,r
    else if (inteq .eq. 4) then
      write (sdev,50) h3,h3,h,h,h1,r,h,h1
    else if (inteq .eq. 5) then
      write (sdev,60) r,h3,h3,h2,h2,h2,h2,h3,h3,h3
    else if (inteq .eq. 7) then
      write (sdev,100) h3,h2,h,h1,h1,r,h,h
    else if (inteq .eq. 8) then
      write (sdev,100) h3,h3,h,r,h1,h,h,r
    else if (inteq .eq. 9) then
      write (sdev,80) h3,h3,h2,h2,r,h2,h2,h1,h3,h2
    else if (inteq .eq. 11) then
      write (sdev,50) r,h3,r,h,h3,h3,h,r
      call bwrite(sdev,0)
      write (sdev,50) r,h3,h1,h,h,r,h1,h1
    else if (inteq .eq. 12) then
      write (sdev,90) h3,h3,h,r,h1,h,h,r,h1,h1
    else
      write (sdev,50) r,h3,r,h,h3,h3,h,r
    end if
    call bwrite(sdev,0)
!
!       call buffered write
    write (sdev,70) x,y
    call bwrite(sdev,0)
!
    xwhere = xpage
    ywhere = ypage
    xsymb = xpage+height*1.5_lrk*fac*xfac*cos(toradf(angle))
    ysymb = ypage+height*1.5_lrk*fac*yfac*sin(toradf(angle))
  else
    hei = height*fac
    hei1 = height*fac/1.2_lrk
    if (nint(xpag) .eq. 999) then
      xpage = xsymb
    else
      xpage = xpag*fac*xfac+xursp
    end if
!
    if (nint(ypag) .eq. 999) then
      ypage = ysymb
    else
      ypage = ypag*fac*yfac+yursp
    end if
    x = cos(toradf(angle))
    y = sin(toradf(angle))
    x1 = xpage-xver
    y1 = ypage-yver
!
!       call buffered write
    write (sdev,110) x1,y1,hei1,hei,x,y
    call bwrite(sdev,0)
!
    nn = nchar
    if (nchar .gt. 71) nn = 71
    write(jchar,115) c2,ibcd(1:nn),term,smc
    nn = nn+4
!       direct string buffer
    call swrite(jchar(1:nn))
!
    xwhere = xsymb
    ywhere = ysymb
    xsymb = xpage+height*nchar*fac*xfac*cos(toradf(angle))
    ysymb = ypage+height*nchar*fac*yfac*sin(toradf(angle))
  end if
!
  return
!
10 format ('PU',f7.1,',',f7.1,';')
20 format ('PR;',2(3('PD',f7.1,',',f7.1,';') ))
30 format ('PR;',2(4('PD',f7.1,',',f7.1,';') ),2('PD',f7.1,',',f7.1,'&
  &;'))
40 format ('PR;',4('PD',f7.1,',',f7.1,';'))
50 format ('PR;PU',f7.1,',',f7.1,';PD',f7.1,',',f7.1,';PU',f7.1,',',&
  &f7.1,';PD',f7.1,',',f7.1,';')
60 format ('PR;',3('PD',f7.1,',',f7.1,';') ,2('PD',f7.1,',',f7.1,';')&
  &)
70 format ('PA;PU',f7.1,',',f7.1,';PD;')
80 format ('PR;PU',f7.1,',',f7.1,';',2('PD',f7.1,',',f7.1,';'),'PU',&
  &f7.1,',',f7.1,';PD',f7.1,',',f7.1,';')
90 format ('PR;PU',f7.1,',',f7.1,';',2('PD',f7.1,',',f7.1,';') ,2('PD&
  &',f7.1,',',f7.1,';'))
100 format ('PR;PU',f7.1,',',f7.1,';',3('PD',f7.1,',',f7.1,';'))
110 format ('PU',f7.1,',',f7.1,';SI',f8.4,',',f8.4,';DI',f7.3,',',&
  &f7.3,';')
115 format(4a)
!
end subroutine symbol
!
!     ==================================================================
!>    @brief calculates the position in cm for an input
!>     in graphics value, must first stablish scaling.
!
!>    @param[in] in input graphic value
!>    @param[in] k variable index, 1 for x and 2 for y
!>    @return the position in cm relative to the graphic input value
!
real(lrk) function gval(in,k)
  use com_lset, only: vm, dl, vx
  use rd_kinds, only: lrk
  implicit none
!
  real(lrk) :: in
  integer :: k
!
!
  gval=(in-vm(k))/dl(k)
!
  return
!
end function gval
!
!     ==================================================================
!>    @brief circle, call skalierung first.
!
!>    @param[in] x x position, data coordinate
!>    @param[in] y y position, data coordinate
!>    @param[in] r circle radius, >0 graphic coordinates, plot else
!
subroutine circle(x, y, r)
  use com_blocp, only: xursp, yursp, xsymb, ysymb, fac, xwhere, ywhere, xfac, yfac, xver, yver, ldev
  use rd_kinds, only: lrk
  implicit none
!
  real(lrk) :: x, y, r
!
  character(len=128) :: sdev
!
!
  real(lrk) :: gval, xpag, ypag, xx, yy, ar
!
  intrinsic :: abs
!
  if (r .gt. 0._lrk) then
!       graphic coordinates
    xpag=gval(x,1)
    ypag=gval(y,2)
  else
!       plot coordinates
    xpag=x
    ypag=y
  end if
!     radius
  ar=abs(r)
!
  xwhere=xpag*fac*xfac+xursp
  ywhere=ypag*fac*yfac+yursp
!
  xx=xwhere-xver
  yy=ywhere-yver
!
!     call buffered write
  write (sdev,10) xx,yy
  call bwrite(sdev,0)
  write (sdev,20) ar
  call bwrite(sdev,0)
!
  return
!
10 format ('PU',f7.1,',',f7.1,';')
20 format ('CI',f7.1,';')
!
end subroutine circle
!
!     ==================================================================
!>    @brief get maximum number of mark kinds.
!>    @see mxmarks, blockhp
!
!>    @return maximum number of mark kinds.
!
integer function mxmarkf()
  implicit none
!
!     maximum number of mark kinds
  integer :: mxmark
!
  call mxmarks(mxmark)
  mxmarkf = mxmark
!
  return
!
end function mxmarkf
!
!     ==================================================================
!>    @brief get maximum number of mark kinds.
!>    @see mxmarkf, blockhp
!
!>    @param[out] mxmarkd maximum number of mark kinds
!
subroutine mxmarks(mxmarkd)
  use com_markkd, only: mxmark
  implicit none
!
  integer :: mxmarkd
!
!     maximum number of mark kinds
!
  mxmarkd = mxmark
!
  return
!
end subroutine mxmarks
!
!     ==================================================================
!>    @brief draw mark, call skalierung first.
!
!>    @param[in] x x position, data coordinate
!>    @param[in] y y position, data coordinate
!>    @param[in] sz size, >0 graphic coordinates, plot else
!>    @param[in] k kind, 0=x,1=+,2=square,3=triangle,4=circle,
!>     5=upside down triangle,6=diamond.
!
subroutine mark(x, y, sz, k)
  use com_blocp, only: xursp, yursp, xsymb, ysymb, fac, xwhere, ywhere, xfac, yfac, xver, yver, ldev
  use com_markkd, only: mxmark
  use rd_kinds, only: lrk
  implicit none
!
  real(lrk) :: x, y, sz
  integer :: k
!
  real(lrk) :: ds, dm, s
  character(len=4) :: nmm
  character(len=128) :: sdev
  parameter(nmm = 'mark')
!
!
  real(lrk) :: gval, xpag, ypag, xx, yy
!
!     maximum number of mark kinds
!
  intrinsic :: abs
!
  if (k .lt. 0 .and. k .gt. mxmark) then
    call elmsge(1,6,nmm)
  end if
!
  if (sz .gt. 0._lrk) then
!       graphic coordinates
    xpag=gval(x,1)
    ypag=gval(y,2)
  else
!       plot coordinates
    xpag=x
    ypag=y
  end if
!     size
  s=abs(sz)
  xwhere=xpag*fac*xfac+xursp
  ywhere=ypag*fac*yfac+yursp
!
  xx=xwhere-xver
  yy=ywhere-yver
!
  if (k .eq. 0) then
!       x
    ds = s/2._lrk*0.707_lrk
!       call buffered write
    write (sdev,10) xx-ds,yy-ds
    call bwrite(sdev,0)
    write (sdev,20) xx+ds,yy+ds
    call bwrite(sdev,0)
    write (sdev,10) xx-ds,yy+ds
    call bwrite(sdev,0)
    write (sdev,20) xx+ds,yy-ds
    call bwrite(sdev,0)
  else if (k .eq. 1) then
!       +
    ds = s/2._lrk
!       call buffered write
    write (sdev,10) xx-ds,yy
    call bwrite(sdev,0)
    write (sdev,20) xx+ds,yy
    call bwrite(sdev,0)
    write (sdev,10) xx,yy+ds
    call bwrite(sdev,0)
    write (sdev,20) xx,yy-ds
    call bwrite(sdev,0)
  else if (k .eq. 2) then
!       square
    ds = s/2._lrk
!       call buffered write
    write (sdev,10) xx-ds,yy+ds
    call bwrite(sdev,0)
    write (sdev,20) xx-ds,yy-ds
    call bwrite(sdev,0)
    write (sdev,20) xx+ds,yy-ds
    call bwrite(sdev,0)
    write (sdev,20) xx+ds,yy+ds
    call bwrite(sdev,0)
    write (sdev,20) xx-ds,yy+ds
    call bwrite(sdev,0)
  else if (k .eq. 3) then
!       triangle
    ds = s/1.8_lrk
    dm = s*0.866_lrk/1.8_lrk
!       call buffered write
    write (sdev,10) xx,yy+dm
    call bwrite(sdev,0)
    write (sdev,20) xx-ds,yy-dm
    call bwrite(sdev,0)
    write (sdev,20) xx+ds,yy-dm
    call bwrite(sdev,0)
    write (sdev,20) xx,yy+dm
    call bwrite(sdev,0)
  else if (k .eq. 4) then
!       circle
    ds = sz/2._lrk
    call circle(x, y, ds)
  else if (k .eq. 5) then
!       upside down triangle
    ds = s/1.8_lrk
    dm = s*0.866_lrk/1.8_lrk
!       call buffered write
    write (sdev,10) xx,yy-dm
    call bwrite(sdev,0)
    write (sdev,20) xx-ds,yy+dm
    call bwrite(sdev,0)
    write (sdev,20) xx+ds,yy+dm
    call bwrite(sdev,0)
    write (sdev,20) xx,yy-dm
    call bwrite(sdev,0)
  else if (k .eq. 6) then
!       diamond
    ds = s/1.8_lrk
!       call buffered write
    write (sdev,10) xx-ds,yy
    call bwrite(sdev,0)
    write (sdev,20) xx,yy-ds
    call bwrite(sdev,0)
    write (sdev,20) xx+ds,yy
    call bwrite(sdev,0)
    write (sdev,20) xx,yy+ds
    call bwrite(sdev,0)
    write (sdev,20) xx-ds,yy
    call bwrite(sdev,0)
  end if
!
  return
!
10 format ('PU',f7.1,',',f7.1,';')
20 format ('PD',f7.1,',',f7.1,';')
!
end subroutine mark
!
!>    @brief Evaluates the piecewise linear interpolant.
!>    The piecewise linear interpolant L(ND,XD,YD)(X) is the piecewise
!>     linear function which interpolates the data (XD(I),YD(I))
!>     for I = 1 to ND.
!>     Licensing:
!>     This code is distributed under the GNU LGPL license.
!>     Modified: 22 September 2012
!>     Author: John Burkardt
!
!>    @param[in] nd number of data points. Must be at least 1.
!>    @param[in] dm dimension of data points.
!>    @param[in] xd x data points.
!>    @param[in] yd y data values.
!>    @param[in] xi the interpolation point.
!>    @param[out] yi interpolated value.
!
subroutine interp_1d(nd,dm,xd,yd,xi,yi)
  use rd_kinds, only: lrk, wp
  implicit none
!
  integer :: nd, dm
  real(lrk) :: xd(dm), yd(dm)
  real(lrk) :: xi, yi
!
  real(lrk) :: dif, prec
  parameter(prec = 1e-12_lrk)
  integer :: k
  real(wp) :: t
!
  intrinsic :: real, abs
!
  yi = 0.0_lrk
  if (nd .eq. 1) then
    yi = yd(1)
    return
  end if
!
  if (xi .le. xd(1)) then
!       check precision
    dif = abs((xd(1)-xi)/xd(1))
    if (dif .gt. prec) then
      t = (xi-xd(1))/(xd(2)-xd(1))
      yi = real(1.0_lrk-t, lrk)*yd(1)+real(t*yd(2), lrk)
    else
      yi = yd(1)
    end if
  else if (xd(nd) .le. xi ) then
!       check precision
    dif = abs((xd(nd)-xi)/xd(nd))
    if (dif .gt. prec) then
      t = (xi-xd(nd-1))/(xd(nd)-xd(nd-1))
      yi = real(1.0_wp-t, lrk)*yd(nd-1)+real(t*yd(nd), lrk)
    else
      yi = yd(nd)
    end if
  else
    do k = 2,nd
      if (xd(k-1) .le. xi .and. xi .le. xd(k)) then
        t = (xi-xd(k-1))/(xd(k)-xd(k-1))
        yi = real(1.0_lrk-t, lrk)*yd(k-1)+real(t*yd(k), lrk)
        exit
      end if
    end do
  end if
!
  return
!
end subroutine interp_1d
!
!     ==================================================================
!>    @brief add equally divided marks.
!
!>    @param[in] vx x data vector.
!>    @param[in] vy y data vector.
!>    @param[in] sz mark size <0 plot coordinates.
!>    @param[in] cn number of points in data vectors.
!>    @param[in] dm dimension of data vectors.
!>    @param[in] dv number of desired divisions.
!>    @param[in] kd kind of mark. @see mark.
!
subroutine marks(vx,vy,sz,cn,dm,dv,kd)
  use rd_kinds, only: lrk
  implicit none
!     arguments
  integer :: cn, dm, dv, kd
  real(lrk) :: sz
  real(lrk) :: vx, vy
  dimension vx(dm),vy(dm)
!
  call markt(vx,vy,sz,cn,dm,dv,kd,.true.)
!
  return
!
end subroutine marks
!
!     ==================================================================
!>    @brief add equally divided marks.
!
!>    @param[in] vx x data vector.
!>    @param[in] vy y data vector.
!>    @param[in] sz mark size <0 plot coordinates.
!>    @param[in] cn number of points in data vectors.
!>    @param[in] dm dimension of data vectors.
!>    @param[in] dv number of desired divisions.
!>    @param[in] kd kind of mark. @see mark.
!>    @param[in] ct init first with plus half dx.
!
subroutine markt(vx,vy,sz,cn,dm,dv,kd,ct)
  use rd_kinds, only: lrk
  implicit none
!     arguments
  integer :: cn, dm, dv, kd
  logical :: ct
  real(lrk) :: sz
  real(lrk) :: vx, vy
  dimension vx(dm),vy(dm)
!     locals
  integer :: i
  real(lrk) :: dx, px, py
!
  if (ct) then
!       divisions
    dx = (vx(cn)-vx(1))/dv
    px = vx(1)+dx/2
  else
    dx = (vx(cn)-vx(1))/(dv-1)
    px = vx(1)
  end if
!
  do i = 1,dv
    call interp_1d(cn,dm,vx,vy,px,py)
    call mark(px,py,sz,kd)
    px = px+dx
  end do
!
  return
!
end subroutine markt
!
!     ==================================================================
!>    @brief plot a legend
!
!>    @param[in] x x plot position
!>    @param[in] y y plot position
!>    @param[in] kd kind of mark, <0 no mark
!>    @param[in] sz relative size of mark, kd <0 ignored
!>    @param[in] pn pen number, @see pen.
!>    @param[in] ll lenght of legend text.
!>    @param[in] st legend text.
!
subroutine legend(x,y,kd,sz,pn,ll,st)
  use rd_kinds, only: lrk
  implicit none
!     arguments
  real(lrk) :: x, y, sz
  integer :: kd, pn, ll
  character(len=*) :: st
!
!     line
  call pen(pn)
  call plot(x,y,3)
  call plot(x+0.8_lrk,y,2)
!     mark
  if (kd .ge. 0) then
    call mark(x+0.4_lrk,y,-250._lrk*sz,kd)
  end if
!     legend text
  call symbol(x+1._lrk,y,sz,st,0,0._lrk,ll)
!
  return
!
end subroutine legend
!
!     ==================================================================
!>    @brief plot a symbol in the graphics coordinates
!
!>    @param[in] x x graphic value position
!>    @param[in] y y graphic value position
!>    @param[in] height symbol height
!>    @param[in] ibcd symbol characters, ascii 3 is the terminator.
!>    @param[in] inteq
!>    @param[in] angle rotation angle
!>    @param[in] nchar number of characters in symbol
!
subroutine csymbol(x, y, height, ibcd, inteq, angle, nchar)
  use rd_kinds, only: lrk
  implicit none
!
  integer :: inteq, nchar
  real(lrk) :: x, y, height, angle
  character(len=*) :: ibcd
!
  real(lrk) :: xpag, ypag, gval
!
  xpag=gval(x,1)
  ypag=gval(y,2)
!
  call symbol(xpag, ypag, height, ibcd, inteq, angle, nchar)
!
  return
!
end subroutine csymbol
!
!     ==================================================================
!>    @brief plot a number
!
!>    @param[in] xpage x value position in cm
!>    @param[in] ypage y value position in cm
!>    @param[in] height number height
!>    @param[in] fpn
!>    @param[in] angle rotation angle
!>    @param[in] ndec number of decimal places
!
subroutine number(xpage, ypage, height, fpn, angle, ndec)
  use rd_kinds, only: lrk
  implicit none
!
!     NUMBER SCHREIBT EINE ZAHL
!
  real(lrk) :: xpage, ypage, height, fpn, angle
  integer :: ndec
!
  real(lrk) :: fpv
  integer :: i, ii, ilp, inteq, j, k, maxn, mn, n, kk
!
  character(len=20) :: num
  character(len=1) :: minus, izero, ipoint
  data minus/'-'/,izero/'0'/,ipoint/'.'/
!
  intrinsic :: abs, char, ichar, int, log10, real
!
  ii = 0
  fpv = fpn
  n = ndec
  maxn = 9
  inteq = 0
!
  if ((n-maxn) <= 0) go to 20
  go to 10
10 n = maxn
20 if ((n+maxn) < 0) go to 30
go to 40
30 n = -maxn
40 if ((fpv) < 0) go to 50
go to 60
50 ii = ii+1
  num(ii:ii) = minus
60 mn = -n
  if ((n) < 0) go to 70
  go to 80
70 mn = mn-1
80 fpv = abs(fpv)+(0.5_lrk*10._lrk**mn)
  i = int(log10(fpv)+1.0_lrk)
  ilp = i
  if ((n+1) < 0) go to 90
  go to 100
90 ilp = ilp+n+1
100 if ((ilp) <= 0) go to 110
go to 120
110 ii = ii+1
  num(ii:ii) = izero
  go to 190
120 if ((ilp+n-18) <= 0) go to 150
go to 130
130 n = -1
  if ((ilp-19) <= 0) go to 150
  go to 140
140 ilp = 19
150 do j=1,ilp
    k = int(fpv*(10._lrk**(j-i)))
    if ((k-9) <= 0) go to 170
    go to 160
160 k = 9
170 ii = ii+1
    kk = ichar(izero)+k
    num(ii:ii) = char(kk)
    fpv = fpv-(real(k, lrk)*(10._lrk**(i-j)))
end do
190 if ((n) < 0) go to 250
go to 200
200 ii = ii+1
  num(ii:ii) = ipoint
  if ((n) <= 0) go to 250
  go to 210
210 do j = 1,n
    k = int(fpv*10._lrk)
    if ((k-9) <= 0) go to 230
    go to 220
220 k = 9
230 ii = ii+1
    kk = ichar(izero)+k
    num(ii:ii) = char(kk)
    fpv = fpv*10._lrk-real(k, lrk)
end do
!
250 call symbol(xpage, ypage, height, num, inteq, angle, ii)
!
  return
!
end subroutine number
!
!     ==================================================================
!>    @brief set the line type.
!>    reset to zero is needed.
!
!>    @param[in] ilt line type.
!>    Kind of line of standard to pattern number is as follows:
!>    <ul>
!>    <li>&lt;0 restore line type to default</li>
!>    <li>0 Point is plotted at specifying point</li>
!>    <li>1 Dotted line of point</li>
!>    <li>2 Short dotted line</li>
!>    <li>3 Long dotted line</li>
!>    <li>4 Short dashed line</li>
!>    <li>5 Long dashed line</li>
!>    <li>6 Two-point phantom line</li>
!>    </ul>
!>    @param[in] ipct pattern length. Specifies the length of one
!>     pattern by the percentage of the distance between scaling
!>     point P1 and P2. It will become 4% if there is no designation.
!
subroutine ltype(ilt,ipct)
  implicit none
!
  integer :: ilt, ipct
!
  character(len=1) :: smc, c1
  character(len=2) :: c2
  character(len=3) :: c3
  character(len=128) :: sdev
  parameter(smc = ';',c1 = ',',c2='LT',c3 = 'LT;')
!
  if (ilt .lt. 0) then
!       restore line type
    write (sdev,5) c3
    call bwrite(sdev,0)
  else
    if (ilt .ge. 0 .and. ilt .le. 6) then
      if (ipct .gt. 0 .and. ipct .le. 128) then
        if (ipct .lt. 10) write (sdev,15) c2,ilt,c1,ipct,smc
        if (ipct .lt. 100) write (sdev,25) c2,ilt,c1,ipct,smc
        if (ipct .le. 1000) write (sdev,35) c2,ilt,c1,ipct,smc
      else
        write (sdev,45) c2,ilt,smc
      end if
      call bwrite(sdev,0)
    end if
  end if
!
  return
!
5 format(a)
15 format(a,i1,a,i1,a)
25 format(a,i1,a,i2,a)
35 format(a,i1,a,i3,a)
45 format(a,i1,a)
!
end subroutine ltype
!
!     ==================================================================
!>    @brief plot a line for a sequence of x,y points, last two points
!>    should contain the minimum value and the delta.
!
!>    @param[in] x vector of x value position in cm
!>    @param[in] y vector of y value position in cm
!>    @param[in] npts number of point in the vectors
!>    @param[in] k multiply value for more than one sequence of points
!>    @param[in] j
!>    @param[in] l
!
subroutine line(x, y, npts, k, j, l)
  use rd_kinds, only: lrk
  implicit none
!
  real(lrk) :: x, y
  dimension x(*), y(*)
  integer :: npts, k, j, l
!
!     LINE ZEICHNET EINEN LINIENZUG
!
  real(lrk) :: dx, dy, xmin, xn, ymin, yn
  integer :: i, ic, ica, is, isa, kk, ldx, lmin, lsw, n, na, neg, nf, nt
  character(len=1) :: blank
  parameter (blank = ' ')
!
  na = 0
  if ((npts) < 0) go to 10
  if ((npts) == 0) go to 140
  go to 20
10 n = -npts
  neg = -1
  go to 30
20 n = npts
  neg = 1
30 kk = k
  lmin = n*kk+1
  ldx = lmin+kk
  xmin = x(lmin)
  ymin = y(lmin)
  dx = x(ldx)
  dy = y(ldx)
  ic = 3
  is = -1
  nt = abs(j)
  nf = 1
  na = nt
  if ((j) < 0) go to 40
  if ((j) == 0) go to 50
  go to 60
40 ica = 3
  isa = -1
  lsw = 1
  go to 70
50 na = ldx
60 ica = 2
  isa = -2
  lsw = 0

70 do i = 1,n
    xn = (x(nf)-xmin)/dx
    yn = (y(nf)-ymin)/dy*neg
    if ((na-nt) < 0) go to 90
    if ((na-nt) == 0) go to 80
    go to 100
80  if (lsw .eq. 0) call plot (xn, yn, ic)
    call symbol (xn, yn, 0.20_lrk, blank, l, 0.0_lrk, is)
    na=0
    na=1
    go to 120
90 if ((lsw) /= 0) go to 110
go to 100
100 call plot (xn, yn, ic)
110 na=na+1
120 ic=ica
    is=isa
    nf=nf+kk
130 end do
!
140 return
!
end subroutine line
!
!     ==================================================================
!>    @brief get axis bound limits.
!>     call axis draw routines to set, return zero otherwise.
!
!>    @param isy true for y axis
!>    @param ismx true for maximum value
!
real(lrk) function getaxbf(isy,ismx)
  use com_yscl, only: scxmn, scxmx, scymn, scymx
  use rd_kinds, only: lrk
  implicit none
!
  logical :: isy, ismx
!
  real(lrk) :: ret
!
!     axis limit
!
  if (isy) then
!       y axis
    if (ismx) then
!         maximum
      ret = scymx
    else
      ret = scymn
    end if
  else
!       x axis
    if (ismx) then
      ret = scxmx
    else
      ret = scxmn
    end if
  end if
!
  getaxbf = ret
!
  return
!
end function getaxbf
!
!     ==================================================================
!>    @brief draws alinear x axis.
!
!>    @param[in] xpage x origin in cm
!>    @param[in] ypage y origin in cm
!>    @param[in] ibcd axis title
!>    @param[in] kkn axis title length, signal...
!>    @param[in] ax axis number of tics = divisions + 1
!>    @param[in] angle axis rotation in 90 degree steps
!>    @param[in] firstv axis origin value, 10^x
!>    @param[in] deltav axis increment, 10^x2 - 10^x1
!>    @param[in] zdec axis numbers length
!>    @param[in] nbesch numbers every nbesch tics
!>    @param[in] nline
!
subroutine axisx(xpage, ypage, ibcd, kkn, ax, angle, firstv,&
&deltav, zdec, nbesch, nline)
  use com_grids, only: hgrid, vgrid
  use com_yscl, only: scxmn, scxmx, scymn, scymx
  use rd_kinds, only: lrk
  implicit none
!
!     AXISX ZEICHET EINE X-ACHSE (VGL. AXIS)
!
!     PC-VERSION VOM 28. 5. 1990   --  KARSTEN BRACH
!     ORIGINALVERSION VOM RRZN, ERWEITERT UM 'NDEC', GROESSE DER ZEICHEN
!                  'NDEC' =  ZAHL DER NACHKOMMASTELLEN BEI BESCHRIFTUNG
!                            (VGL. UNTERPROGRAMM NUMBER, GDV.HPS 2)
!
!     AENDERUNG VOM 5. 2. 1991     --  BERND PONICK
!     ZUSAETZLICH ERWEITERT UM 'NBESCH' (ALTE VERSION: NBESCH=2)
!                'NBESCH' =  GIBT AN, ALLE WIEVIEL EINHEITEN DIE
!                            ACHSE BESCHRIFTET WIRD
!                 'NLINE' =  GIBT AN, ALLE WIEVIEL EINHEITEN EIN
!                            TEILSTRICH AN DIE ACHSE GEZEICHNET WIRD
!                  'ZDEC' =  ZAHL DER ZAEHLSTELLEN BEI DER BESCHRIFTUNG
!     NDEC IST KEIN FORMALPARAMETER MEHR, SONDERN WIRD SO GESETZT, DASS
!     DIE BESCHRIFTUNG ZDEC ZAEHLSTELLEN UMFASST
!
!     AENDERUNG VOM 12. 7. 1991    -- BERND PONICK
!     ZUSAETZLICH ERWEITERT UM 'GRSCHR'
!                'GRSCHR' =  IST GRSCHR=.TRUE., WIRD DIE GROESSE DER
!                            ACHSBESCHRIFTUNG VERDOPPELT
!
  real(lrk) :: ax, angle, deltav, firstv, xpage, ypage
  integer :: kkn, nbesch, nline
!
  real(lrk) :: a, adx, axlen, cth, dxb, dyb, ex
  integer :: ndec, zdec
  character(len=*) :: ibcd
  character(len=3) :: malz
  parameter (malz = '*10')
  real(lrk) :: texth, zahlh, nbr, sth, xn, xt, xval, yn, yt, z
  integer :: i, j, kn, nanf, nnull, ntic
!
!     grid state
!
!     axis limit
!
  intrinsic :: asin, cos, int, log10, max, mod, nint, sign, sin
!
  j = 0
  texth = 0.42_lrk
  zahlh = 0.31_lrk
!
  axlen = ax
  kn = kkn
  sth = angle*asin(1.0_lrk)/90.0_lrk
  cth = cos(sth)
  sth = sin(sth)
  a = 1.0_lrk
  if (kn .lt. 0) then
    a = -a
    kn = -kn
  end if
!
  if (axlen.lt.0) axlen=0
  ex = 0.0_lrk
!
  adx = max(-firstv,firstv+int(ax)*deltav)
  do while  (adx.gt.10.0_lrk**zdec-1)
    adx = adx*0.001_lrk
    ex = ex+3.0_lrk
  end do
!
  do while (adx.le.0.999_lrk)
    adx = adx*1000.0_lrk
    ex = ex-3.0_lrk
  end do
!
  xval = firstv*10.0_lrk**(-ex)
  adx = deltav*10.0_lrk**(-ex)
!
  ndec = max(0,zdec-1-int(log10(max(-xval,xval+int(ax)*adx))))
!
  dxb = -0.254_lrk
  dyb = 0.381_lrk*a-0.127_lrk
  xn = xpage-dyb*sth+dxb*cth*1.2_lrk
  yn = ypage+dxb*sth+dyb*cth*1.35_lrk
!
  ntic = int(axlen+1.0_lrk)
!
  if (xval .gt. 0.0_lrk) then
    nnull = 0
    nanf = 0
  else
    nnull = nint(-xval/adx)
    nanf = mod(nnull,nbesch)
    xval = xval+adx*nanf
    xn = xn+cth*nanf
    yn = yn+sth*nanf
  end if
!     numbers
  do i = nanf+1,ntic,nbesch
    z = sign(0.25_lrk*10.0_lrk**(-ndec),xval)
    nbr = xval+z
!       min scale number
    if (i .eq. nanf) scxmn = nbr-z
    call number (xn, yn, zahlh, nbr, angle, ndec)
!       max scale number
    scxmx = nbr-z
    xval = xval+adx*nbesch
    xn = xn+cth*nbesch
    yn = yn+sth*nbesch
  end do
!
  z = kn
  if (abs(ex) .gt. 1.e-36_lrk) z = z+7
  dxb = -0.178_lrk*z+axlen*0.5_lrk
  dyb = 0.825_lrk*a-0.191_lrk
  xt = xpage+dxb*cth-dyb*sth
  yt = ypage+dxb*sth+dyb*cth*1.50_lrk
!
  if (kn .ne. 0) call symbol (xt, yt, texth, ibcd, j, angle, kn)
  if (abs(ex).ge.1.e-36_lrk) then
    z=kn+2
    xt=xt+z*cth*texth
    yt=yt+z*sth*texth
    call symbol (xt, yt, texth, malz, j, angle, 3)
    xt=xt+(3.0_lrk*cth-0.8_lrk*sth)*texth
    yt=yt+(3.0_lrk*sth+0.8_lrk*cth)*texth
!       EXPO
    call number (xt, yt, zahlh, ex, angle, -1)
!       apply scale
    scxmn = scxmn*10**ex
    scxmx = scxmx*10**ex
  end if
!
!     TICS
  call plot (xpage+axlen*cth, ypage+axlen*sth, 3)
  dxb=-zahlh*a*sth
  dyb=+zahlh*a*cth
  a=ntic-1
  xn=xpage+a*cth
  yn=ypage+a*sth
!
  do i=1,ntic
    if (mod(ntic-nnull-i,nline).eq.0) then
      call plot (xn, yn, 2)
      call plot (xn+dxb, yn+dyb, 2)
!         GRID
      if (i.lt.ntic .and. hgrid) then
        do j=1,16
          call plot (xn+dxb, yn+.99_lrk*j, 1)
          call plot (xn+dxb, yn+.99_lrk*j+0.2_lrk, 2)
        end do
        call plot (xn+dxb, yn+dyb, 1)
      end if
    end if
    call plot (xn, yn, 2)
    xn=xn-cth
    yn=yn-sth
  end do
!
  return
!
end subroutine axisx
!
!     ==================================================================
!>    @brief logarithmic axis x
!
!>    @param xpage x origin in cm
!>    @param ypage y origin in cm
!>    @param ibcd axis title
!>    @param kkn axis title length, signal...
!>    @param ax axis number of tics = divisions + 1
!>    @param angle axis rotation in 90 degree steps
!>    @param firstv axis origin value, 10^x
!>    @param deltav axis increment, 10^x2 - 10^x1
!
subroutine laxisx(xpage, ypage, ibcd, kkn, ax, angle, firstv,&
&deltav)
  use com_grids, only: hgrid, vgrid
  use com_yscl, only: scxmn, scxmx, scymn, scymx
  use rd_kinds, only: lrk
  implicit none
!
  integer :: kkn
  real(lrk) :: xpage, ypage, ax, angle, firstv, deltav
  character(len=*) :: ibcd
  character(len=3) :: malz
!
  real(lrk) :: texth, zahlh, mxv, nbr, t, t1, eps, gval
  real(lrk) :: a, axlen, cth, dxb, dyb, ex, sth, xt, yn, yt, z
  integer :: i, j, k, l, iex, lex, ndec, ndc, ntic, ima, lma, kn
!     malz=expoent text
  parameter (malz = '*10',eps = 1e-18_lrk)
!
!     grid state
!     axis limit
!
  intrinsic :: abs, asin, cos, int, log10, real, sin
!
!     machine precision

!     text height
  texth = 0.42_lrk
!     number height
  zahlh = 0.3_lrk
!     axis tics = divisions + 1
  axlen = ax
  kn = kkn
!     axis rotation in 90 degree steps
  sth = angle*asin(1.0_lrk)/90._lrk
  cth = cos(sth)
  sth = sin(sth)
!
  a = 1.0_lrk
  if (kn .lt. 0) then
    a = -a
    kn = -kn
  end if
!???
  if (axlen.lt.0) axlen = 0
!     initial expoent
  iex = int(log10(firstv)+eps)
!     maximal value
  mxv = firstv+(ax-1._lrk)*deltav
!     last expoent
  lex = int(log10(mxv)+eps)
!     number of logarithmic decades
  ndc = lex-iex
!     expoent minimum + decades / 2
  ex = iex+ndc/2
!     initial integer mantissa
  ima = int(firstv/10._lrk**iex+eps)
!     last integer mantissa
  lma = int(mxv/10._lrk**lex+eps)
  if (mxv/10._lrk**lex .gt. lma) lma = lma+1
!     number of tics 10 * ndc + lma -ima -ndc+1
  ntic = (10*(ndc-1))+lma+(10-ima)-ndc+1
!     tic numbers
  dxb = -0.254_lrk
  dyb = 0.381_lrk*a-0.127_lrk
  yn = ypage+dxb*sth+dyb*cth*1.35_lrk
  t1 = 0._lrk
  j = iex
  k = ima
  do i = 1,ntic
    nbr = k*10._lrk**j+eps
    t = xpage+gval(log10(nbr),1)
    if (t .gt. 0) then
      if (t1 .eq. 0._lrk .or. abs(t-t1) .gt. 1.9_lrk) then
        z = 10**(-ex)
!           min scale number
        if (t1 .eq. 0) scxmn = nbr*z
!           number of decimal places of tic numbers
        if (nbr*z .lt. 1) then
          ndec = 3
        else if (nbr*z .ge. 1 .and. nbr*z .lt. 10) then
          ndec = 2
        else if (nbr*z .ge. 10 .and. nbr*z .lt. 100) then
          ndec = 1
        else
          ndec = 0
        end if
        call number (t, yn, zahlh, nbr*z, angle, ndec)
!           max scale number
        scxmx = nbr*z
        t1 = t
      end if
    end if
    k = k+1
    if (k.eq. 10) then
      j = j+1
      k = 1
    end if
  end do
!
  z=kn
  if (abs(ex) .gt. 1.e-36_lrk) z=z+7
  dxb = -0.178_lrk*z+axlen*0.5_lrk
  dyb = 0.825_lrk*a-0.191_lrk
  xt = xpage+dxb*cth-dyb*sth
  yt = ypage+dxb*sth+dyb*cth*1.50_lrk
!
  if (kn.ne.0) call symbol (xt, yt, texth, ibcd, j, angle, kn)
!
  if (abs(ex) .ge. 1.e-36_lrk) then
    z = kn+2
    xt = xt+z*cth*texth
    yt = yt+z*sth*texth
!
    call symbol (xt, yt, texth, malz, j, angle, 3)
    xt = xt+(3.0_lrk*cth-0.8_lrk*sth)*texth
    yt = yt+(3.0_lrk*sth+0.8_lrk*cth)*texth
!       EXPO
    call number (xt, yt, zahlh, ex, angle, -1)
!       apply scale
    scxmn = scxmn*10**ex
    scxmx = scxmx*10**ex
  end if
!
!     TICS
  t = xpage+axlen*cth
  call plot (t, ypage+axlen*sth, 3)
  yn = ypage+(a*sth)
  dxb = -zahlh*a*sth
  dyb = zahlh*a*cth
!
  j = iex
  k = ima
  do i = 1,ntic
    nbr = k*10._lrk**j+eps
    if (nbr .le. eps) goto 300
    t = xpage+gval(log10(nbr),1)
    call plot (t, yn, 2)
    call plot (t+dxb, yn+dyb, 2)
    if (i.gt.1 .and. hgrid) then
      do l = 1,16
        call plot (t+dxb, yn+.99_lrk*l, 1)
        call plot (t+dxb, yn+.99_lrk*l+0.2_lrk, 2)
      end do
      call plot (t+dxb, yn+dyb, 1)
    end if
    call plot (t, yn, 2)
300 continue
    k = k+1
    if (k.eq.10) then
      j = j+1
      k = 1
    end if
  end do
!
  return
!
end subroutine laxisx
!
!     ==================================================================
!>    @brief draw y axis.
!
!>    @param[in] xpage x origin in cm
!>    @param[in] ypage y origin in cm
!>    @param[in] ibcd axis title
!>    @param[in] kkn axis title length, signal...
!>    @param[in] ax axis number of tics = divisions + 1
!>    @param[in] angle axis rotation in 90 degree steps
!>    @param[in] firstv axis origin value, 10^x
!>    @param[in] deltav axis increment, 10^x2 - 10^x1
!>    @param[in] zdec axis numbers length
!>    @param[in] nbesch numbers every nbesch tics
!>    @param[in] nline
!
subroutine axisy(xpage, ypage, ibcd, kkn, ax, angle, firstv,&
&deltav, zdec, nbesch, nline)
  use com_grids, only: hgrid, vgrid
  use com_yscl, only: scxmn, scxmx, scymn, scymx
  use rd_kinds, only: lrk
  implicit none
!
!     AXISY ZEICHET EINE Y-ACHSE MIT WAAGERECHTER BESCHRIFTUNG (VGL. AXI
!     PC-VERSION VOM 28. 5. 1990      --  KARSTEN BRACH
!     ORIGINALVERSION VOM RRZN, ERWEITERT UM 'NDEC', GROESSE DER ZEICHEN
!                  'NDEC' =  ZAHL DER NACHKOMMASTELLEN BEI
!                            BESCHRIFTUNG
!                            (VGL. UNTERPROGRAMM NUMBER, GDV.HPS 2)
!
!     AENDERUNG VOM 5. 2. 1991        --  BERND PONICK
!     ZUSAETZLICH ERWEITERT UM 'NBESCH' (ALTE VERSION: NBESCH=2)
!                'NBESCH' =  GIBT AN, ALLE WIEVIEL TEILSTRICHE DIE
!                            ACHSE BESCHRIFTET WIRD
!                 'NLINE' =  GIBT AN, ALLE WIEVIEL EINHEITEN EIN
!                            TEILSTRICH AN DIE ACHSE GEZEICHNET WIRD
!                  'ZDEC' =  ZAHL DER ZAEHLSTELLEN BEI DER BESCHRIFTUNG
!     NDEC IST KEIN FORMALPARAMETER MEHR, SONDERN WIRD SO GESETZT, DASS
!     DIE BESCHRIFTUNG ZDEC ZAEHLSTELLEN UMFASST
!
!     AENDERUNG VOM 12. 7. 1991       --  BERND PONICK
!     ZUSAETZLICH ERWEITERT UM 'GRSCHR'
!                'GRSCHR' =  IST GRSCHR=.TRUE., WIRD DIE GROESSE DER
!                            ACHSBESCHRIFTUNG VERDOPPELT
!
  real(lrk) :: xpage, ypage, ax, angle, firstv, deltav
  integer :: kkn, nbesch, zdec, nline
  character(len=*) :: ibcd
  character(len=3) :: malz
  parameter (malz = '*10')
  real(lrk) :: a, adx, anw, axlen, cth, dxb, dyb, ex, sth, xn, xt, xval, yn, yt, z
  real(lrk) :: texth, zahlh, nbr, t, t1
  integer :: kn, f, ndec, ntic, nnull, nanf
  integer :: i, j
!
!     grid state
!     axis limit
!
  intrinsic :: abs, asin, cos, int, log10, max, mod, nint, real, sign, sin
!
  texth=0.42_lrk
  zahlh=0.31_lrk
!
  axlen=ax
  kn=kkn
  anw=angle-90
  sth=angle*asin(1.0_lrk)/90.0_lrk
  cth=cos(sth)
  sth=sin(sth)
  a=1.0_lrk
!
  if (kn.lt.0) then
    a=-a
    kn=-kn
  end if
!
  ex=0.0_lrk
  adx=max(-firstv,firstv+int(ax)*deltav)
!
  do while (adx.gt.10.0_lrk**zdec-1)
    adx=adx*0.001_lrk
    ex=ex+3.0_lrk
  end do
!
  do while (adx.le.0.999_lrk)
    adx=adx*1000.0_lrk
    ex=ex-3.0_lrk
  end do
!
  xval=firstv*10.0_lrk**(-ex)
  adx=deltav*10.0_lrk**(-ex)
!
  ndec=max(0,zdec-1-int(log10(max(-xval,xval+int(ax)*adx))))
  dxb=-0.06_lrk
  dyb=1.0_lrk*a+0.7_lrk
  xn=xpage+dxb*cth-dyb*sth*1.1_lrk*1.15_lrk
!
  yn=ypage+dyb*cth+dxb*sth
  ntic=int(axlen+1)
!
  if (xval.gt.0.0_lrk) then
    nnull=0
    nanf=0
  else
    nnull=nint(-xval/adx)
    nanf=mod(nnull,nbesch)
    xval=xval+adx*nanf
    xn=xn+cth*nanf
    yn=yn+sth*nanf
  end if
!
!     NUMBERS
  t1=0._lrk
  do i=nanf+1,ntic,nbesch
    f=-2
    if (abs(xval).lt.10000) f=-1
    if (abs(xval).lt.1000) f=0
    if (abs(xval).lt.100) f=1
    if (abs(xval).lt.10) f=2
    if (xval.ge.0.0_lrk) f=f+1
!
    if (ndec.ge.0) then
      f=f+2-ndec
    else
      f=f+3
    end if
    z=sign(0.25_lrk*10.0_lrk**(-ndec),xval)
    nbr=xval+z
    t=yn
    if (t1 .eq. 0._lrk.or.abs(t-t1).gt.1.9_lrk) then
!         min scale number
      if (t1 .eq. 0) scymn = nbr-z
      call number (xn+f*0.18_lrk, t, zahlh, nbr, anw, ndec)
!         max scale number
      scymx = nbr-z
      t1=t
    end if
    xval=xval+adx*real(nbesch, lrk)
    xn=xn+cth*real(nbesch, lrk)
    yn=yn+sth*real(nbesch, lrk)
  end do
!
  z=kn
  if (abs(ex).gt.1.e-36_lrk) z=z+7.0_lrk
  dxb=-0.178_lrk*z+axlen*0.5_lrk
  dyb=2.025_lrk*a-0.191_lrk
  xt=xpage+dxb*cth-dyb*sth*1.1_lrk*1.15_lrk
!     AXIS TITLE
  yt=ypage+dyb*cth+dxb*sth
  if (kn.ne.0) call symbol (xt, yt, texth, ibcd, j, angle, kn)
!     EXPO
  if (abs(ex).ge.1.e-36_lrk) then
    z=kn+2
    xt=xt+z*cth*texth
    yt=yt+z*sth*texth
    call symbol (xt, yt, texth, malz, j, angle, 3)
    xt=xt+(3.0_lrk*cth-0.8_lrk*sth)*texth
    yt=yt+(3.0_lrk*sth-0.8_lrk*cth)*texth
    call number (xt, yt, zahlh, ex, angle, -1)
!       apply scale
    scymn = scymn*10**ex
    scymx = scymx*10**ex
  end if
!
!     TICS
  call plot (xpage+axlen*cth, ypage+axlen*sth, 3)
  dxb=-zahlh*a*sth
  dyb=zahlh*a*cth
  a=ntic-1
  xn=xpage+a*cth
  yn=ypage+(a*sth)
!
  do i=1,ntic
    if (mod(ntic-nnull-i,nline).eq.0) then
      call plot (xn, yn, 2)
      call plot (xn+dxb, yn+dyb, 2)
!         GRID
      if (i.lt.ntic-1 .and. vgrid) then
        do j=1,21
          call plot (xn+0.94_lrk*j, yn, 1)
          call plot (xn+0.94_lrk*j+0.2_lrk, yn, 2)
        end do
        call plot (xn+dxb, yn+dyb, 1)
      end if
    end if
    call plot (xn, yn, 2)
    xn=xn-cth
    yn=yn-sth
  end do
!
  return
!
end subroutine axisy
!
!     ==================================================================
!>    @brief draw logarithmic y axis.
!
!>    @param[in] xpage x origin in cm
!>    @param[in] ypage y origin in cm
!>    @param[in] ibcd axis title
!>    @param[in] kkn axis title length, signal...
!>    @param[in] ax axis number of tics = divisions + 1
!>    @param[in] angle axis rotation in 90 degree steps
!>    @param[in] firstv axis origin value, 10^x
!>    @param[in] deltav axis increment, 10^x2 - 10^x1
!
subroutine laxisy(xpage, ypage, ibcd, kkn, ax, angle, firstv,&
&deltav)
  use com_grids, only: hgrid, vgrid
  use com_yscl, only: scxmn, scxmx, scymn, scymx
  use rd_kinds, only: lrk
  implicit none
!
  real(lrk) :: xpage, ypage, ax, angle, firstv, deltav
  integer :: kkn
  character(len=*) :: ibcd
!
  character(len=3) :: malz
  real(lrk) :: a, anw, axlen, cth, dxb, dyb, ex, sth, xn, xt, yt, z
  real(lrk) :: texth, zahlh, nbr, t, t1, mxv, mxp, eps, gval
  integer :: kn, ndec, ntic, ndc
  integer :: i, j, k, l, iex, ima, lex, lma
!     malz=expoent text
  parameter (malz = '*10',eps = 1e-18_lrk)
!
!     grid state
!     axis limit
!
  intrinsic :: abs, asin, cos, int, log10, real, sin
!
  texth = 0.42_lrk
  zahlh = 0.31_lrk
!
  axlen = ax
  mxp = ypage+axlen
  kn = kkn
  anw = angle-90
  sth = angle*asin(1.0_lrk)/90.0_lrk
  cth = cos(sth)
  sth = sin(sth)
  a = 1.2_lrk
!
  if (kn .lt. 0) then
    a = -a
    kn = -kn
  end if
!
  if (axlen .lt. 0) axlen = 0
!     initial expoent
  iex = int(log10(firstv)+eps)
!     maximal value
  mxv = firstv+(ax-1._lrk)*deltav
!     last expoent
  lex = int(log10(mxv)+eps)
!     number of logarithmic decades
  ndc = lex-iex
!     expoent minimum + decades / 2
  ex = iex+ndc/2
!     initial integer mantissa
  ima = int(firstv/10._lrk**iex+eps)
!     last integer mantissa
  lma = int(mxv/10._lrk**lex+eps)
  if (mxv/10._lrk**lex.gt.lma) lma = lma+1
!     number of tics
  ntic = (10*(ndc-1))+lma+(10-ima)-ndc+1
!     tic numbers
  dxb = -0.06_lrk
  dyb = 1.0_lrk*a+0.7_lrk
  xn = xpage+dxb*cth-dyb*sth*1.15_lrk
!
  t1 = 0._lrk
  j = iex
  k = ima
  do i = 1,ntic
    nbr = k*10._lrk**j+eps
    t = ypage+gval(log10(nbr),2)
    if (t .gt. 0) then
      nbr = nbr*10._lrk**(-ex)
      if (t1 .eq. 0._lrk .or. abs(t-t1).gt.0.6_lrk .and. t .lt. mxp) then
!           min scale number
        if (t1 .eq. 0) scymn = nbr
!           number of decimal places of tic numbers
        if (nbr .lt. 1) then
          ndec = 3
        else if (nbr .ge. 1  .and. nbr .lt. 10) then
          ndec = 2
        else if (nbr .ge. 10  .and. nbr .lt. 100) then
          ndec = 1
        else
          ndec = 0
        end if
        call number (xn, t, zahlh, nbr, anw, ndec)
!           max scale number
        scymx = nbr
        t1 = t
      end if
    end if
    k = k+1
    if (k.eq.10) then
      j = j+1
      k = 1
    end if
  end do
!
  z = kn
  if (abs(ex) .gt. 1.e-36_lrk) z = z+7.0_lrk
  dxb = -0.178_lrk*z+axlen*0.5_lrk
  dyb = 2.025_lrk*a-0.191_lrk
  xt = xpage+dxb*cth-dyb*sth*1.1_lrk*1.15_lrk
!     AXIS TITLE
  yt = ypage+dyb*cth+dxb*sth
  if (kn .ne. 0) call symbol (xt, yt, texth, ibcd, j, angle, kn)
!     EXPO
  if (abs(ex) .ge. 1.e-16_lrk) then
    z = kn+2
    xt = xt+z*cth*texth
    yt = yt+z*sth*texth
    call symbol (xt, yt, texth, malz, j, angle, 3)
    xt = xt+(3.0_lrk*cth-0.8_lrk*sth)*texth
    yt = yt+(3.0_lrk*sth-0.8_lrk*cth)*texth
    call number (xt, yt, zahlh, ex, angle, -1)
!       apply scale
    scymn = scymn*10**ex
    scymx = scymx*10**ex
  end if
!     TICS
  call plot (xpage+axlen*cth, ypage+axlen*sth, 3)
  dxb = -zahlh*a*0.5_lrk*sth
  dyb = zahlh*a*0.5_lrk*cth
  a = ntic-1
  xn = xpage+a*cth
  call plot (xn, ypage, 2)
!
  i = 1
  j = iex
  k = ima
  do while (i.le.ntic.or.t.le.mxp)
    nbr = k*10._lrk**j+eps
    if (nbr .le. eps) goto 300
    t = ypage+gval(log10(nbr),2)
    if (t.lt.t1) then
      call plot (xn, t, 2)
      call plot (xn+dxb, t+dyb, 2)
      if (i.gt.1 .and. vgrid) then
        do l = 1,21
          call plot (xn+0.94_lrk*l, t+dyb, 1)
          call plot (xn+0.94_lrk*l+0.2_lrk, t+dyb, 2)
        end do
        call plot (xn+dxb, t+dyb, 1)
      end if
      call plot (xn, t, 2)
    end if
300 continue
    k = k+1
    if (k.eq.10) then
      j = j+1
      k = 1
    end if
    i = i+1
  end do
!
  return
!
end subroutine laxisy
!
!     ==================================================================
!>    @brief calculates a fine delta to determine the
!>      number of divisions.
!
!>    @param[in] nul minimum number as zero
!>    @param[in] mini minimum value
!>    @param[in] maxi maximum value
!>    @param[in] l number of divisions
!>    @return best number of divisions
!
real(lrk) function delta(nul,mini,maxi,l)
  use rd_kinds, only: lrk
  implicit none
!
!     DELTA BESTIMMT EINE FEINERE ACHSLAENGENSTUFUNG ALS SCALE ES ERMOEG
!
  real(lrk) :: l, maxi, mini, nul
!
  real(lrk) :: a, b, c, dif
!
  intrinsic :: int, log10
!
  dif=1._lrk+.5_lrk/l
  a=(maxi-mini)/l
  if (a.gt.nul) then
    b=10._lrk**int(log10(a))
  else
    b=10._lrk
  end if
!
  if (a.lt.1._lrk) b = b/10._lrk
  c=a/b
  if (c.gt.8._lrk*dif) then
    delta=10._lrk*b
  else if (c.gt.7._lrk*dif) then
    delta=8._lrk*b
  else if (c.gt.6._lrk*dif) then
    delta=7._lrk*b
  else if (c.gt.5._lrk*dif) then
    delta=6._lrk*b
  else if (c.gt.4._lrk*dif) then
    delta=5._lrk*b
  else if (c.gt.3.5_lrk*dif) then
    delta=4._lrk*b
  else if (c.gt.3._lrk*dif) then
    delta=3.5_lrk*b
  else if (c.gt.2.5_lrk*dif) then
    delta=3._lrk*b
  else if (c.gt.2._lrk*dif) then
    delta=2.5_lrk*b
  else if (c.gt.1.5_lrk*dif) then
    delta=2._lrk*b
  else if (c.gt.1.25_lrk*dif) then
    delta=1.5_lrk*b
  else
    delta=1.25_lrk*b
  end if
!
  return
!
end function delta
!
!     ==================================================================
!>    @brief skalierung, hold scale transformation factors.
!
!>    @param[in] vmin minimum
!>    @param[in] vmax maximum
!>    @param[in] deltav delta relative to the number of divisions,
!>    vmax = ndiv * deltav
!>    @param[in] idx index, 1=x, 2=y
!
subroutine ska(vmin, vmax, deltav, idx)
  use com_lset, only: vm, dl, vx
  use com_skaset, only: isset
  use rd_kinds, only: lrk
  implicit none
  real(lrk) :: vmin, vmax, deltav
  integer :: idx
!
!
  vm(idx) = vmin
  dl(idx) = deltav
  vx(idx) = vmax
  isset(idx) = .true.
!
  return
!
end subroutine ska
!
!     ==================================================================
!>    @brief calculate scale.
!
!>    @param[in] vmin minimum value
!>    @param[in] vmax maximum value
!>    @param[in] div number of divisions
!>    @param[in] zero limit for zero
!>    @param[out] smin minimum value
!>    @param[out] dl delta relative to the number of divisions,
!>    maximum = delta * div
!
subroutine scaling(vmin, vmax, div, zero, smin, dl)
  use rd_kinds, only: lrk
  implicit none
!
  real(lrk) :: vmin, vmax, div, zero, smin, dl
  real(lrk) :: delta
!
  intrinsic :: aint
!
  dl = delta(zero,vmin,vmax,div)
  smin = aint(vmin/dl)*dl
  if (vmin .lt. -0.05_lrk*dl) smin = smin-dl
!
  return
!
end subroutine scaling

!     ==================================================================
!>    @brief reset current scales so that in the next call of malhap
!.     first page the scales will be recalculated.
!
!>    @param k variable index, 1 for x and 2 for y and 3 for both.
!
subroutine sreset(k)
  use com_skaset, only: isset
  implicit none
!
  integer :: k
!
!
  if (k .eq. 1 .or. k .eq. 2) then
    isset(k) = .false.
  else
    isset(1) = .false.
    isset(2) = .false.
  end if
!
  return
!
end subroutine sreset
!
!=======================================================================
!>    @brief line plot sequence scale factors realocation,
!>     for intermediate malhap calls with different number of points
!>     but preserving the already defined scalings in the last two point
!
!>    @param[in] xwert x values vector
!>    @param[in] ywert y values vector
!>    @param[in] store xwert and ywert vectors dimension
!>    @param[in] anz number of points, scaling will be put +1 and +2
!
subroutine rscale(xwert,ywert,store,anz)
  use com_pscale, only: xsn, ysn
  use rd_kinds, only: lrk
  implicit none
!
  integer :: store, anz
  real(lrk) :: xwert, ywert
  dimension xwert(store+2), ywert(store+2)
!
!     x
  xwert(anz+1) = xsn(1)
  xwert(anz+2) = xsn(2)
!     y
  ywert(anz+1) = ysn(1)
  ywert(anz+2) = ysn(2)
!
  return
!
end subroutine rscale
!
!=======================================================================
!>    @brief default curve plot routine, @see malhapax.
!
!>    @param[in] anz number of curve points to plot
!>    @param[in] format page size for malhap (format = 4 --> din a4)
!>    @param[in] ft page count for malhap (ft = 1 --> new page)
!>    @param[in] store dimension of the point vectors
!>    @param[in] xachse plot x axis
!>    @param[in] yachse plot y axis
!>    @param[in] nul minimum value non zero
!>    @param[in] maxrig maximum values at right
!>    @param[in] maxtop maximum values at top
!>    @param[in] minbot minimum values at bottom
!>    @param[in] minlef minimum values at left
!>    @param[in] xwert x points vector
!>    @param[in] ywert y points vector
!>    @param[in] reke calculation info
!>    @param[in] zeit time stamp
!>    @param[in] datum date stamp
!>    @param[in] abszis x axis tag
!>    @param[in] ordina y axis tag
!>    @param[in] name user name
!>    @param[in] titelo main title
!>    @param[in] titelu sub title
!>    @param[in] prname program name stamp
!>    @param[in] color hpgl pen number
!>
subroutine malhap(anz, format, ft, store, xachse, yachse, nul,&
&maxrig, maxtop, minbot, minlef, xwert, ywert, reke, zeit, datum,&
&abszis, ordina, name, titelo, titelu, prname, color)
  use rd_kinds, only: lrk
!     arguments
  integer :: anz, format, ft, store, xachse, yachse, color
!
  real(lrk) :: nul, maxrig, maxtop, minbot, minlef
  real(lrk) :: xwert(*), ywert(*)
!
  character(len=70) :: reke
  character(len=8) :: zeit
  character(len=10) :: datum
  character(len=30) :: name, abszis, ordina
  character(len=31) :: titelo, titelu
  character(len=8) :: prname
  logical :: xlog, ylog
!
  xlog=.false.
  ylog=.false.
!
  call malhapax (anz, format, ft, store, xachse, yachse, nul,&
  &maxrig, maxtop, minbot, minlef, xwert, ywert, reke, zeit, datum,&
  &abszis, ordina, name, titelo, titelu, prname, color, xlog, ylog)
!
  return
!
end subroutine malhap
!
!=======================================================================
!>    @brief curve plot routine with axis control, based on a
!>    <a href="http://www.ial.uni-hannover.de">IAL</a> fortran routine.
!
!>    @param[in] anz number of curve points to plot
!>    @param[in] format page size for malhap (format = 4 --> din a4)
!>    @param[in] ft page count for malhap (ft = 1 --> new page)
!>    @param[in] store dimension of the point vectors
!>    @param[in] xachse plot x axis
!>    @param[in] yachse plot y axis
!>    @param[in] nul minimum value non zero
!>    @param[in] maxrig maximum values at right
!>    @param[in] maxtop maximum values at top
!>    @param[in] minbot minimum values at bottom
!>    @param[in] minlef minimum values at left
!>    @param[in] xwert x points vector
!>    @param[in] ywert y points vector
!>    @param[in] reke calculation info
!>    @param[in] zeit time stamp
!>    @param[in] datum date stamp
!>    @param[in] abszis x axis tag
!>    @param[in] ordina y axis tag
!>    @param[in] name user name
!>    @param[in] titelo main title
!>    @param[in] titelu sub title
!>    @param[in] prname program name stamp
!>    @param[in] color hpgl pen number, <0 does not plot line
!>    @param[in] logx x axis is logarithmic
!>    @param[in] logy y axis is logarithmic
!>
subroutine malhapax(anz, format, ft, store, xachse, yachse, nul,&
&maxrig, maxtop, minbot, minlef, xwert, ywert, reke, zeit, datum,&
&abszis, ordina, name, titelo, titelu, prname, color, logx, logy)
  use com_frax, only: fzax
  use com_htrc, only: ctr
  use com_inf, only: vrs, dta, rgh, sid
  use com_pscale, only: xsn, ysn
  use com_skaset, only: isset
  use rd_kinds, only: lrk
!
!***********************************************************************
!     MALHAP zeichnet zweidimensionale Kurvenverlaeufe mit der RRZN-
!     HAPS-software - letzte Aenderungen von Bernd Ponick, 25.4.91
!***********************************************************************
!
!     Bedeutung der verwendeten Parameter:
!     ------------------------------------
!
!     NBESCH (I): Alle NBESCH Einheiten wird die Achse beschriftet
!     NMARK  (I): Alle NMARK Einheiten wird die Achse unterteilt (Teilst
!     XNULL  (R): Verschiebung des Koordinatenursprungs in x-Richtung
!     YNULL  (R): Verschiebung des Koordinatenursprungs in y-Richtung
!     ZSTEL  (I): Anzahl der Zaehlstellen der Achsenbeschriftung
!
  implicit none
!
!     arguments
  integer :: anz, format, ft, store, xachse, yachse, color
!
  real(lrk) :: nul, maxrig, maxtop, minbot, minlef, xwert, ywert
  dimension xwert(store+2), ywert(store+2)
!
  character(len=8) :: zeit, prname
  character(len=10) :: datum
  character(len=30) :: name, abszis, ordina
  character(len=31) :: titelo, titelu
  character(len=70) :: reke, ttit
  logical :: logx, logy
!
  integer :: i
  logical :: zax
  integer :: zstel, nbesch, nmark
  parameter (zstel=4,nbesch=4,nmark=2)
!
  real(lrk) :: xay, yax, fakt, xnull, ynull, xaxi, yaxi, dm1, dm2
  parameter  (xnull=3.5_lrk,ynull=2.0_lrk,xaxi=21._lrk,yaxi=17._lrk)
!
!
!
!     force axis
!
!     calculation indentification
!     header truncation
!
  intrinsic :: len_trim, sqrt
!
!     ---------------------------------------------
!     Festlegen der Zeichengroesse (Faktor 19./21.,
!     da Laserdrucker nur 19.5 cm breit druckt)
!     ---------------------------------------------
!
  if (ft .eq. 1) then
    fakt = sqrt(2._lrk)**(4-format)
    call factor (fakt)
    call newplot (0, 0, 29.7_lrk, 21._lrk, 0)
!
    fakt = 19._lrk/21._lrk*fakt
    call factor (fakt)
!       ever black pen
    call pen (1)
!
!       ---------------------------------------------
!       Beschriften einer neuen Zeichnung, falls FT=1
!       ---------------------------------------------
!
    call plot (1.5_lrk*sqrt(2._lrk), 1.5_lrk, -3)
    call symbol (28.5_lrk, 19.25_lrk, .25_lrk, reke, 0, -90._lrk, 70)
    call symbol (27.5_lrk, 3.1_lrk, .25_lrk, datum, 0, -90._lrk, 10)
    call symbol (26.5_lrk, 3.1_lrk, .25_lrk, zeit, 0, -90._lrk, 8)
    call symbol (25.6_lrk, 3._lrk, .25_lrk, prname, 0, -90._lrk, 8)
    call symbol (26.5_lrk, 20.5_lrk, .25_lrk, name(1:12), 0, -90._lrk, 12)
    call symbol (27.3_lrk, 16.5_lrk, .4_lrk, titelo, 0, -90._lrk, 31)
    ttit = titelu//ctr
    i = len_trim(ttit)
    call symbol (25.9_lrk, 20.5_lrk, .33_lrk, ttit, 0, -90._lrk, i)
    call plot (25.5_lrk, .5_lrk, 3)
    call plot (25.5_lrk, 20.5_lrk, 2)
!
!       ---------------------------------
!       Berechnen der Skalierungsfaktoren
!       ---------------------------------
!
    call scaling (minlef, maxrig, 20._lrk, nul, dm1, dm2)
    xwert(anz+2) = dm2
    xwert(anz+1) = dm1
    xsn(1) = dm1
    xsn(2) = dm2
!
!       negative x values
    if (xwert(anz+1).lt.-nul) then
!         search for positive values to plot a zero line
      zax = .false.
      do i = 1,anz
        if (xwert(i) .gt. 0._lrk)then
          zax = .true.
          cycle
        end if
      end do
!         to have zero y axis must have positive and negative values
      if ((yachse .eq. 1 .and. zax) .or. fzax(2)) then
        xay = xnull-xwert(anz+1)/xwert(anz+2)
        call plot (xay, ynull, 3)
        call plot (xay, ynull+yaxi, 2)
      end if
    else
      xay = xnull
    end if
!
    call scaling (minbot, maxtop, 16._lrk, nul, dm1, dm2)
    ywert(anz+2) = dm2
    ywert(anz+1) = dm1
    ysn(1)=dm1
    ysn(2)=dm2
!
!       negative y values
    if (ywert(anz+1) .lt. 0._lrk) then
!         search for positive values to plot a zero line
      zax = .false.
      do i = 1,anz
        if (ywert(i) .gt. 0._lrk)then
          zax = .true.
          cycle
        end if
      end do
!         to have zero x axis must have positive and negative values
      if ((xachse .eq. 1 .and. zax) .or. fzax(1)) then
        yax=ynull-ywert(anz+1)/ywert(anz+2)
        call plot (xnull,yax,3)
        call plot (xnull+xaxi,yax,2)
      end if
    else
      yax = ynull
    end if
!
!       Skalierungsfaktoren
    if (.not.isset(1)) call ska (xwert(anz+1),xwert(anz+2)*20._lrk,xwert(anz+2),1)
    if (.not.isset(2)) call ska (ywert(anz+1),ywert(anz+2)*16._lrk,ywert(anz+2),2)
!
!       -----------------------------------
!       Zeichnen und Beschriften der Achsen
!       -----------------------------------
!
    if (xachse .eq. 1) then
      if (.not. logx) then
        dm1 = xwert(anz+1)
        dm2 = xwert(anz+2)
        call axisx (xnull, ynull, abszis, -30, xaxi, 0._lrk, dm1, dm2,zstel, nbesch, nmark)
      else
        dm1=10._lrk**minlef
        dm2=(10._lrk**maxrig-dm1)/20._lrk
        call laxisx (xnull, ynull, abszis, -30, xaxi, 0._lrk, dm1, dm2)
      end if
    end if
!
    if (yachse .eq. 1) then
      if (.not. logy) then
        dm1 = ywert(anz+1)
        dm2 = ywert(anz+2)
        call axisy (xnull, ynull, ordina, 30, yaxi, 90._lrk, dm1, dm2,zstel, nbesch, nmark)
      else
        dm1 = 10._lrk**minbot
        dm2 = (10._lrk**maxtop-dm1)/16._lrk
        call laxisy (xnull, ynull, ordina, 30, yaxi, 90._lrk, dm1, dm2)
      end if
    else if (xachse .eq. 0) then
      call symbol (xay, yax, 1._lrk, datum, 3, 0._lrk, -1)
    end if
!
!       WEG
    i = len_trim(rgh)
    if (i .gt. 0) call symbol (1._lrk, 20.5_lrk, .16_lrk, rgh, 0, 0._lrk, i)
!
    call plot (xnull, ynull, -3)
!       ft if
  end if
!
!-----------------------------------------------------------------------
!     Einzeichnen des Kurvenverlaufs
!-----------------------------------------------------------------------
!
  call pen (abs(color))
!
  if (color .gt. 0) call line (xwert, ywert, anz, 1, 0, 0)
!
  ft=ft+1
!
  return
!
end subroutine malhapax
!
!=======================================================================
!>    @brief multiply two 4x4 matrices m3 = m1 x m2
!
!>    @param[in] m1 first 4x4 input matrix
!>    @param[in] m2 second 4x4 input matrix
!>    @param[out] m3 result 4x4 matrix = m1 x m2
!
subroutine mult4(m1, m2, m3)
  use rd_kinds, only: lrk
  implicit none
!
  real(lrk) :: m1, m2, m3
  dimension m1(4,4),m2(4,4),m3(4,4)
!
  integer :: i

  do i=1,4
    m3(i,1)=m1(i,1)*m2(1,1)+m1(i,2)*m2(2,1)+m1(i,3)*m2(3,1)+m1(i,4)*&
    &m2(4,1)
    m3(i,2)=m1(i,1)*m2(1,2)+m1(i,2)*m2(2,2)+m1(i,3)*m2(3,2)+m1(i,4)*&
    &m2(4,2)
    m3(i,3)=m1(i,1)*m2(1,3)+m1(i,2)*m2(2,3)+m1(i,3)*m2(3,3)+m1(i,4)*&
    &m2(4,3)
    m3(i,4)=m1(i,1)*m2(1,4)+m1(i,2)*m2(2,4)+m1(i,3)*m2(3,4)+m1(i,4)*&
    &m2(4,4)
  end do
!
  return
!
end subroutine mult4
!
!=======================================================================
!>    @brief multiply vector 1x4 by 4x4 matrix m3 = m1 x m2
!
!>    @param[in] m1 input 1x4 vector
!>    @param[in] m2 4x4 input matrix
!>    @param[out] m3 result 1x4 vector =  m1 x m2
!
subroutine mult4x1 (m1, m2, m3)
  use rd_kinds, only: lrk
  implicit none
!
  real(lrk) :: m1, m2, m3
  dimension m1(4),m2(4,4),m3(4)
!
  m3(1)=m1(1)*m2(1,1)+m1(2)*m2(2,1)+m1(3)*m2(3,1)+m1(4)*m2(4,1)
  m3(2)=m1(1)*m2(1,2)+m1(2)*m2(2,2)+m1(3)*m2(3,2)+m1(4)*m2(4,2)
  m3(3)=m1(1)*m2(1,3)+m1(2)*m2(2,3)+m1(3)*m2(3,3)+m1(4)*m2(4,3)
  m3(4)=m1(1)*m2(1,4)+m1(2)*m2(2,4)+m1(3)*m2(3,4)+m1(4)*m2(4,4)
!
  return
!
end subroutine mult4x1
!
!=======================================================================
!>    @brief return an diagonal 4x4 matrix
!
!>    @param[in] v diagonal value
!>    @param[out] m 4x4 eye matrix
!
subroutine diag4(v, m)
  use rd_kinds, only: lrk
  implicit none
!
  real(lrk) :: v, m
  dimension m(4,4)
!
  integer :: i, j
!
  do i=1,4
    do j=1,4
      m(i,j)=0._lrk
      if (i.eq.j) m(i,j)=v
    end do
  end do
!
  return
!
end subroutine diag4
!
!=======================================================================
!>    @brief camera view matrix set
!
!>    @param[in] ax rotation x rad
!>    @param[in] ay rotation y rad
!>    @param[in] az rotation z rad
!>    @param[in] px position x
!>    @param[in] py position y
!>    @param[in] pz position z
!>    @param[in] sc scale
!>    @param[out] m camera matrix
!
subroutine cset(ax, ay, az, px, py, pz, sc, m)
  use com_view, only: vv
  use com_ws, only: c, w
  use rd_kinds, only: lrk
  implicit none
!
  real(lrk) :: ax, ay, az, px, py, pz, sc
  real(lrk) :: m(4,4)
!
  integer :: i, j
  real(lrk) :: cx, sx, cy, sy, cz, sz, rx, ry, rz, ct, cs
  dimension rx(4,4),ry(4,4),rz(4,4),ct(4,4),cs(4,4)
!     3d set flags
!     view common
!
  intrinsic :: cos, sin
!
  cx=cos(-ax)
  sx=sin(-ax)
  cy=cos(-ay)
  sy=sin(-ay)
  cz=cos(-az)
  sz=sin(-az)
!     rotation x
  call diag4 (1._lrk, rx)
  rx(2,2)=cx
  rx(2,3)=sx
  rx(3,2)=-sx
  rx(3,3)=cx
!     rotation y
  call diag4 (1._lrk, ry)
  ry(1,1)=cy
  ry(1,3)=-sy
  ry(3,1)=sy
  ry(3,3)=cy
!     rotation z
  call diag4 (1._lrk, rz)
  rz(1,1)=cz
  rz(1,2)=sz
  rz(2,1)=-sz
  rz(2,2)=cz
!     translation
  call diag4 (1._lrk, ct)
  ct(4,1)=-px
  ct(4,2)=-py
  ct(4,3)=-pz
!     scale
  call diag4 (sc, cs)
  cs(4,4)=1._lrk
!
!     order matters
  call diag4 (1._lrk, m)
!     translation
  call mult4 (m, ct, m)
!     rotation
  call mult4 (m, rz, m)
  call mult4 (m, ry, m)
  call mult4 (m, rx, m)
!     scale
  call mult4 (m, cs, m)
!     copy to common
  do i=1,4
    do j=1,4
      vv(i,j)=m(i,j)
    end do
  end do
!
  c=.true.
!
  return
!
end subroutine cset
!
!=======================================================================
!>    @brief camera view matrix set, common convenience set
!
!>    @param[in] ax rotation x rad
!>    @param[in] ay rotation y rad
!>    @param[in] az rotation z rad
!>    @param[in] px position x
!>    @param[in] py position y
!>    @param[in] pz position z
!>    @param[in] sc scale
!
subroutine csetc(ax, ay, az, px, py, pz, sc)
  use rd_kinds, only: lrk
  implicit none
!
  real(lrk) :: ax, ay, az, px, py, pz, sc
!     dummy,  common set
  real(lrk) :: m
  dimension m(4,4)
!
  call cset(ax, ay, az, px, py, pz, sc, m)
!
  return
!
end subroutine csetc
!>    @brief camera view matrix set, vectors
!>      and common convenience
!
!>    @param[in] a rotation x,y,z rad
!>    @param[in] p position x,y,z
!>    @param[in] sc scale
!
subroutine csetvc(a, p, sc)
  use rd_kinds, only: lrk
  implicit none
!
  real(lrk) :: sc, a, p
  dimension a(3),p(3)
!
!     dummy,  common set
  real(lrk) :: m
  dimension m(4,4)
!
  call cset (a(1), a(2), a(3), p(1), p(2), p(3), sc, m)
!
  return
!
end subroutine csetvc
!
!=======================================================================
!>    @brief world matrix set
!
!>    @param[in] ax rotation x rad
!>    @param[in] ay rotation y rad
!>    @param[in] az rotation z rad
!>    @param[in] px position x
!>    @param[in] py position y
!>    @param[in] pz position z
!>    @param[in] sc scale
!>    @param[out] m world matrix
!
subroutine wset(ax, ay, az, px, py, pz, sc, m)
  use com_world, only: ww
  use com_ws, only: c, w
  use rd_kinds, only: lrk
  implicit none
!
  real(lrk) :: ax, ay, az, px, py, pz, sc, m
  dimension m(4,4)
!
  integer :: i, j
  real(lrk) :: cx, sx, cy, sy, cz, sz, rx, ry, rz, ct, cs
  dimension rx(4,4),ry(4,4),rz(4,4),ct(4,4),cs(4,4)
!     3d set flags
!     world common
!
  intrinsic :: cos, sin
!
  cx=cos(ax)
  sx=sin(ax)
  cy=cos(ay)
  sy=sin(ay)
  cz=cos(az)
  sz=sin(az)
!     rotation x
  call diag4 (1._lrk, rx)
  rx(2,2)=cx
  rx(2,3)=sx
  rx(3,2)=-sx
  rx(3,3)=cx
!     rotation y
  call diag4 (1._lrk, ry)
  ry(1,1)=cy
  ry(1,3)=-sy
  ry(3,1)=sy
  ry(3,3)=cy
!     rotation z
  call diag4 (1._lrk, rz)
  rz(1,1)=cz
  rz(1,2)=sz
  rz(2,1)=-sz
  rz(2,2)=cz
!     translation
  call diag4 (1._lrk, ct)
  ct(4,1)=px
  ct(4,2)=py
  ct(4,3)=pz
!     scale
  call diag4 (sc, cs)
  cs(4,4)=1._lrk
!
!     order matters
  call diag4 (1._lrk, m)
!     scale
  call mult4 (m, cs, m)
!     rotation
  call mult4 (m, rx, m)
  call mult4 (m, ry, m)
  call mult4 (m, rz, m)
!     translation
  call mult4 (m, ct, m)
!     copy to common
  do i=1,4
    do j=1,4
      ww(i,j)=m(i,j)
    end do
  end do
!
  w=.true.
!
  return
!
end subroutine wset
!
!=======================================================================
!>    @brief world matrix set, common convenience
!
!>    @param[in] ax rotation x rad
!>    @param[in] ay rotation y rad
!>    @param[in] az rotation z rad
!>    @param[in] px position x
!>    @param[in] py position y
!>    @param[in] pz position z
!>    @param[in] sc scale
!
subroutine wsetc(ax, ay, az, px, py, pz, sc)
  use rd_kinds, only: lrk
  implicit none
!
  real(lrk) :: ax, ay, az, px, py, pz, sc
!     dummy,  common set
  real(lrk) :: m
  dimension m(4,4)
!
  call wset (ax, ay, az, px, py, pz, sc, m)
!
  return
!
end subroutine wsetc
!
!=======================================================================
!>    @brief world matrix set, vectors
!>      and common convenience
!
!>    @param[in] a rotation x,y,z rad
!>    @param[in] p position x,y,z
!>    @param[in] sc scale
!
subroutine wsetvc (a, p, sc)
  use rd_kinds, only: lrk
  implicit none
!
  real(lrk) :: a, p, sc
  dimension a(3),p(3)
!
!     dummy,  common set
  real(lrk) :: m
  dimension m(4,4)
!
  call wset (a(1), a(2), a(3), p(1), p(2), p(3), sc, m)
!
  return
!
end subroutine wsetvc
!
!=======================================================================
!>    @brief get 2d representation points from a 3d geometry
!
!>    @param[in] vm camera view 4x4 matrix
!>    @param[in] wm world 4x4 matrix
!>    @param[in] px x input point
!>    @param[in] py y input point
!>    @param[in] pz z input point
!>    @param[in] ox x x output offset
!>    @param[in] oy y y output offset
!>    @param[in] sc output scale
!>    @param[out] x x output point
!>    @param[out] y y output point
!
subroutine get2d(vm, wm, px, py, pz, ox, oy, sc, x, y)
  use com_ws, only: c, w
  use rd_kinds, only: lrk
  implicit none
!
  real(lrk) :: vm, wm, px, py, pz, ox, oy, sc, x, y
  dimension vm(4,4),wm(4,4)
!
  character(len=30) :: msg
  parameter (msg = 'get2d:3d view or world not set')
  real(lrk) :: tm, ip, rp
  dimension tm(4,4),ip(4),rp(4)
!     3d set flags
!
  if (.not. c .or. .not. w) then
    call lmsg(1,msg)
  end if
!     transform combined matrix world and camera view
  call mult4 (wm, vm, tm)
!     input 3d point
  ip(1)=px
  ip(2)=py
  ip(3)=pz
  ip(4)=1._lrk
!     result 2d point
  call mult4x1 (ip, tm, rp)
!     apply scaling and offset
!     symple perstpective / z
  x=rp(1)/rp(3)*sc+ox
  y=rp(2)/rp(3)*sc+oy
!
  return
!
end subroutine get2d
!
!=======================================================================
!>    @brief get 2d representation points from a 3d geometry,
!>     convenience input and output vectors
!
!>    @param[in] vm camera view 4x4 matrix
!>    @param[in] wm world 4x4 matrix
!>    @param[in] p3 input point (x,y,z)
!>    @param[in] o2 output offset (x,y)
!>    @param[in] sc output scale
!>    @param[out] p2 output point (x,y)
!
subroutine get2dv(vm, wm, p3, o2, sc, p2)
  use rd_kinds, only: lrk
  implicit none
!
  real(lrk) :: vm, wm, p3, p2, o2, sc, x, y
  dimension vm(4,4),wm(4,4),p3(3),p2(2),o2(2)
!
  call get2d (vm, wm, p3(1), p3(2), p3(3), o2(1), o2(2), sc, x, y)
!
  p2(1)=x
  p2(2)=y
!
  return
!
end subroutine get2dv
!
!=======================================================================
!>    @brief get 2d representation points from a 3d geometry,
!>     convenience input and output vectors
!>     and common view and world matrices
!
!>    @param[in] p3 input point (x,y,z)
!>    @param[in] o2 output offset (x,y)
!>    @param[in] sc output scale
!>    @param[out] p2 output point (x,y)
!
subroutine get2dvc (p3, o2, sc, p2)
  use com_view, only: vv
  use com_world, only: ww
  use rd_kinds, only: lrk
  implicit none
!
  real(lrk) :: p3, p2, o2, sc, x, y
  dimension p3(3),p2(2),o2(2)
!     world common
!     view common
!
  call get2d (vv, ww, p3(1), p3(2), p3(3), o2(1), o2(2), sc, x, y)
!
  p2(1)=x
  p2(2)=y
!
  return
!
end subroutine get2dvc
!
