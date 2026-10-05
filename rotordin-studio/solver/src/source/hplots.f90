!     $Id: hplots.f 6723 2021-12-22 17:34:59Z francisco $
!     ==================================================================
!
!>    @file hplots.f
!>    @author
!>    francisco
!>    @date
!>    28-may-15
!>    @brief program plot outputs, last changes:<br>
!>    new module - francisco - 28-may-15<br>
!>    added bearing parameter scale sc - francisco - oct-15<br>
!>    added angular critical speed map support - francisco - nov-17<br>
!>    added "open" geometry support - francisco - feb-19<br>
!>    added bearing parameters plot - francisco - feb-19<br>
!>    changed angles in elin from rad to deg - francisco - feb-19<br>
!>    changed plot text to common messages - francisco - mar-19<br>
!>    added system speed on mode shape plot - francisco - apr-19<br>
!>    changed bearing stiff curves, add pu param - francisco - apr-19<br
!>    changed beap, add std flag - francisco - apr-19<br>
!>    changed mxb = 99 francisco - apr-20<br>
!>    added bearing number on critical speed map - francisco - jun-20<br
!>    added bending stress plots on elin - francisco - dec-20<br>
!>    added nmd arg. on tmsor, tmssh and tmors - francisco - feb-21<br>
!>    moved beastiff to parmanv - francisco mar-21<br>
!>    added nit number of crossings on cpbl - francisco mar-21<br>
!>    added maximum displacment and agle on tmsor - francisco mar-21<br>
!>    added setable crossing lines on Campbell - francisco - apr-21<br>
!>    updated bearing parameters, calculations on sbeapar output<br>
!>    updated cpbl intersection lines, check y min - francisco - aug-21<
!>    updated tmors show on support  - francisco - aug-21<br>
!>    updated csmp bearing stiffnes scale - francisco - sep-21<br>
!>    updated beap, add param bearing check - francisco - sep-21<br>
!>    updated cpbl, added extrapolation message - franciso - sep-21<br>
!>    updated unbr, fixed y axis name - francisco - oct-21<br>
!>    add zero mg check on mshp, tmsor and tmssh - francisco nov-21<br>
!>    updated elin, displacement module value terms squared without
!>     conversion to real. FPE issue on Linux - francisco dec-21<br>
!>    updated beap, sbug fix on parameter marker index - francisco -dec-
!>    added vertical line fornominal speed at unbalance response graphic
!
!     ==================================================================
!>    @brief get plot header info.
!
!>    @param[out] reke calculation info
!>    @param[out] zeit time stamp
!>    @param[out] datum date stamp
!>    @param[out] name user name
!>    @param[out] titelu sub title
!
subroutine header(reke,zeit,datum,name,titelu)
  use com_cab, only: unam => name, descript
  use com_htrc, only: ctr
  use com_inf, only: vrs, dta, rgh, sid
  use com_lif, only: usr, dtu, tmu
  use com_nmi, only: inm, sbv
  implicit none
!
  character(len=8) :: zeit
  character(len=10) :: datum
  character(len=30) :: name
  character(len=31) :: titelu
  character(len=70) :: reke
!
  integer :: i, j
!     date time
  character(len=1) :: c1
  character(len=10) :: ds, ts
  dimension c1(2)
  parameter (c1 = (/'-',':'/))
!     header
!     program info
!     internal name
!
!     system identification
!     user info
!     header truncation, see also malhapx. titelu(31), 50-31=19
!
!
  intrinsic :: len, len_trim, min
!
!     date and time -> ds,ts
  call sdate(ds,ts)
!
  i = len_trim(inm)
  j = len_trim(sid)
!     titles
  write(reke,5) inm(1:i),c1(1),sid(1:j),c1(2),vrs,sbv
  j = min(len(zeit),len(ts))
  zeit = ts(1:j)
  j = min(len(datum),len(ds))
  datum = ds(1:j)
  j = min(len(name),len(usr))
  name  = usr(1:j)
  i = len(descript)
  j = min(len(titelu),i)
  titelu = descript(1:j)
!     truncation
  ctr = ' '
  if (i .gt. j) ctr = descript(j+1:)
!
  return
!
5 format(6a)
!
end subroutine header
!
!     ==================================================================
!>    @brief Campbell diagram, logarithmic decrement plots.
!
!>    @param[in] nl number of logarithmic decrements (eigenvalues)
!>    @param[in] nc number of calculated speeds (non interpolated)
!>    @param[in] npi number of interpolated points
!>    @param[in] ncc number of desired critical frequencies
!>    @param[in] ncs number of critical speeds 1x,2x and 1/2x
!>    @param[in] nit number of crossing lines 1, 2, 0.5 x
!>    @param[in] spdn rated speed (rpm)
!>    @param[in] nini initial speed (rad/s)
!>    @param[in] ndw speed increment
!>    @param[in] gi interpolated output frequencies
!>    @param[in] rcr critical speeds (rad/s)
!>    @param[in] fcr critical frequencies (rad/s)
!>    @param[in] dmp crossing rpm times pu vector (1,2,0.5)
!>    @param[in] omg calculated speeds
!>    @param[in] ld logarithmic decrement
!>    @param[in] ldm maximum allowed value of logarithmic decrement
!>    @param[in] mxc rcr(mxm,mxc),fcr(mxm,mxc) dimension
!>    @param[in] mxm rcr(mxm,mxc), fcr(mxm,mxc), ncs(mxm),dmp(mxm)
!>     dimension
!>    @param[in] mtg gi ant ld dimension
!>    @param[in] stdio flag for standard input and output
!>    @param[in] pfn output file name, used for non standard output
!>    @param[out] errmsg return an error message
!>    @param[out] ok return flag. unsuccessful if <0.
!
subroutine cpbl(nl,nc,npi,ncc,ncs,nit,spdn,nini,ndw,gi,&
&rcr,fcr,dmp,omg,ld,ldm,mxc,mxm,mtg,stdio,pfn,errmsg,ok)
  use rd_textfun, only: pltmsgf
  use com_opt, only: std
  use rd_kinds, only: lrk, wp
  implicit none
!     arguments
  integer :: nl, nc, npi, ncc, ncs, nit, mxc, mxm, mtg, ok
  real(lrk) :: spdn, nini, ndw, gi, rcr, fcr, dmp, omg, ldm
  real(wp) :: ld
  dimension gi(mtg,mtg),omg(mtg),ld(nc,ncc),&
  &rcr(mxm,mxc),fcr(mxm,mxc),ncs(mxm),dmp(mxm)
  character(len=99) :: errmsg
  character(len=255) :: pfn
  logical :: stdio
!
!     locals
  real(lrk) :: xv, yx, yn, zv, thz, dr, mx, rad2rpmf, rad2hzf, mxdec, getaxbf
  integer :: i, j, l, k, m, n, mxk, mxmarkf
  character(len=1) :: blank
  character(len=4) :: nmm, c4
  character(len=5) :: c5
  character(len=8) :: c8
  character(len=50) :: txt
!     mxdec - log dec max upper limit
  parameter(thz = 60.0_lrk,mxdec = 20,blank = ' ',nmm = 'cpbl',c4 = '.plt',c5 = 'x rpm',c8 = 'max.dec=')
!
!     output section name
  character(len=10) :: sec
  character(len=20) :: out
!     plot messages
  character(len=30) :: buf
!
!     malhap stuff
  integer :: ft, format, xachse, yachse, color
  real(lrk) :: nul, maxrig, maxtop, minbot, minlef, xwert, ywert
  dimension xwert(npi+2),ywert(npi+2)
  character(len=8) :: zeit, prname
  character(len=10) :: datum
  character(len=30) :: name, abszis, ordina
  character(len=31) :: titelo, titelu
  character(len=70) :: reke
!
  data ft/1/
!
!     std plot output
!
  intrinsic :: abs, len_trim, real
!
!     initialization
  ok = -1
  errmsg = blank
!
!     standard io
  std = stdio
!
!     ndw rad/s
!     rad/s -> rpm
  dr = rad2rpmf(ndw)
!
!     maximum natural frequency
  mx = 0
  do k = 1,ncc
    do l = 1,npi
!         rad/s -> Hz
      zv = rad2hzf(gi(k,l))
      if (zv .gt. mx) mx = zv
    end do
  end do
!
!     malhap settings
  format = 5
  xachse = 1
  yachse = 1
  nul = 0.001_lrk
!     max rpm
  maxrig = nini+(npi-1)*dr
!     maximum frquency (y) limit, rpm -> Hz
  maxtop = mx
!     minimum frquency (y) limit, rpm -> Hz
  minbot = nini/thz
  minlef = nini
!
!     titles
  call header(reke,zeit,datum,name,titelu)
  abszis = pltmsgf(30, 1)
  ordina = pltmsgf(30, 2)
  titelo = pltmsgf(30, 3)
  prname = nmm
!     standard plot marker
  if (stdio) then
    write(sec,5) nmm,c4
    call marksec(sec,0)
  end if

!     line colors
!     default Pen colors: 1:Black, 2:Red, 3:Green, 4:Yellow, 5:Blue,
!      6:Magenta
  color = 1
!     initialize hpgl plot
  call plots(0,0,pfn)
!     line plot counter
  m = 0
!     plot frequency lines
  do k = 1,ncc
    xv = nini
    l = 0
    do j = 1,npi
      l = l+1
!         rad/s -> Hz
      zv = rad2hzf(gi(k,l))
      xwert(l) = xv
      ywert(l) = zv
      xv = xv+dr
    end do
!       check for at least two points to line
    if (l .gt. 1) then
      m = m+1
!         reposition scale
      if (m .gt. 1) call rscale(xwert,ywert,npi,l)
      ft = m
      call malhap (l, format, ft, npi, xachse, yachse,&
      &nul, maxrig, maxtop, minbot, minlef,&
      &xwert, ywert,&
      &reke, zeit, datum, abszis, ordina,&
      &name, titelo, titelu, prname,color)
    end if
!
  end do
!
!     rated speed
  if (spdn .gt. 0) then
    color = 2
    xwert(1) = spdn
    xwert(2) = spdn
!       get y scale min and max values
    xv = getaxbf(.true.,.false.)
    zv = getaxbf(.true.,.true.)
    ywert(1) = xv
    ywert(2) = zv
    call rscale(xwert,ywert,npi,2)
    ft = ft+1
    call malhap (2, format, ft, npi, xachse, yachse,&
    &nul, maxrig, maxtop, minbot, minlef,&
    &xwert, ywert,&
    &reke, zeit, datum, abszis, ordina,&
    &name, titelo, titelu, prname,color)
!       rated speed
    buf = pltmsgf(30, 64)
    k = len_trim(buf)
    write(txt,35) buf(1:k),spdn
    k = len_trim(txt)
    call symbol (15.5_lrk,17.0_lrk,.25_lrk,txt(1:k),0,0._lrk,k)
  end if
!
!     maximal number of "marks"
  mxk = mxmarkf()
!     mark index kind, 0=x,1=+,2=square,3=triangle,4=circle,
!     5=upside down triangle,6=diamond.
  n = 2
!
!     plot crossing lines
  color = 5
  call ltype(2,0)
!
  do m = 1,nit
!       reposition scale
    call rscale(xwert,ywert,npi,2)
!       initial x rpm line
    xwert(1) = minlef
!       initial y rpm line
    ywert(1) = xwert(1)*dmp(m)/thz
    if (ywert(1) .lt. minbot) then
!         set to 0 min
      ywert(1) = minbot
!         set new x
      xwert(1) = minbot/dmp(m)*thz
    end if
    xwert(2) = maxrig
!       final x rpm line
    ywert(2) = xwert(2)*dmp(m)/thz
!       check is over max
    if (ywert(2) .gt. maxtop) then
!         set to max given from y axis
      ywert(2) = getaxbf(.true.,.true.)
!         set new x
      xwert(2) = ywert(2)/dmp(m)*thz
    end if
    call malhap (2, format, ft, npi, xachse, yachse,&
    &nul, maxrig, maxtop, minbot, minlef,&
    &xwert, ywert,&
    &reke, zeit, datum, abszis, ordina,&
    &name, titelo, titelu, prname,color)
!       critical speeds markers
    do k = 1,ncs(m)
!         rad/s -> rpm
      xv = rad2rpmf(fcr(m,k))/dmp(m)
!         rad/s -> Hz
      zv = rad2hzf(rcr(m,k))*dmp(m)
      call mark(xv,zv,50.0_lrk,n)
    end do
    write (buf,25) dmp(m),c5
    j = len_trim(buf)
!       x position
    xv = 0.5_lrk+3.1_lrk*(m-1)
    call legend(xv,16.5_lrk,n,0.2_lrk,color,j,buf)
!       increment mark index
    n = n+1
!       check maximal index and "rotate"
    if (n .gt. mxk) n = 0
  end do
!     eventual speed extrapolation message, see parmanv.f
  call pen(1)
  call eplwrg(-1,txt)
  k = len_trim(txt)
  if (k .gt. 0) call symbol (0.0_lrk, 17.0_lrk, 0.2_lrk, txt(1:k), 0, 0._lrk, k)
!
!     logarithmic decrements
!     titles
  abszis = pltmsgf(30, 1)
  ordina = pltmsgf(30, 4)
  titelo = pltmsgf(30, 5)
!     maximum and minimum
!     limit value to mxdec due numerical issues
  do k = 1,nl
!       speeds
    do i = 1,nc
      if(k .eq. 1 .and. abs(ld(i,k)) .le. mxdec) then
        yn = real(ld(i,k), lrk)
        yx = real(ld(i,k), lrk)
      else
        if (ld(i,k) .gt. yx .and.abs(ld(i,k)) .le. mxdec) yx = real(ld(i,k), lrk)
        if (ld(i,k) .lt. yn .and.abs(ld(i,k)) .le. mxdec) yn = real(ld(i,k), lrk)
      end if
    end do
  end do
!
!     line plot counter
  m = 0
!     first / last speed, rad/s -> rpm
  minlef = rad2rpmf(omg(1))
  maxrig = rad2rpmf(omg(nc))
  minbot = yn
  maxtop = yx
!     number of eigenvalues
  do k = 1,nl
!       speeds
    l = 0
    do i = 1,nc
      if (abs(ld(i,k)) .le. mxdec) then
        l = l+1
        xwert(l) = rad2rpmf(omg(i))
        ywert(l) = real(ld(i,k), lrk)
      end if
    end do
!       check number of points
    if (l .gt. 1) then
!         have more than two points
      m = m+1
!
      if (m .eq. 1) then
        ft = 1
!           reset scales, calculate new.
        call sreset(3)
!           should not plot the line, color = -1
        call malhap (l, format, ft, npi, xachse, yachse,&
        &nul, maxrig, maxtop, minbot, minlef,&
        &xwert, ywert,&
        &reke, zeit, datum, abszis, ordina,&
        &name, titelo, titelu, prname,-1)
        call pen(5)
      end if
!         plot x marks
      do i = 1,l
        call  mark(xwert(i), ywert(i), 50._lrk, 0)
      end do
!         points in
    end if
!       eigenvalues do
  end do
!     first eigenvalues
  buf = pltmsgf(30, 6)
  l = len_trim(buf)
  write(out,15) buf(1:l),nl
  i = len_trim(out)
  call pen(1)
  call symbol (0.2_lrk, 17.4_lrk, .3_lrk, out(1:i), 0, 0._lrk, i)
!     max dec
  l = len_trim(c8)
  write(out,45) c8(1:l),ldm
  i = len_trim(out)
  call symbol (15.2_lrk, 17.4_lrk, .3_lrk, out(1:i), 0, 0._lrk, i)
!     eventual speed extrapolation message, see parmanv.f
  call eplwrg(-1,txt)
  k = len_trim(txt)
  if (k .gt. 0) call symbol (0.0_lrk, 17.0_lrk, 0.2_lrk, txt(1:k), 0, 0._lrk, k)
!
!     standard plot marker
  if (stdio) then
    call bflush
    call marksec(sec,1)
  else
    call plots(1,0,pfn)
  end if
!
!     return ok
  ok = 0
!
  return
!
5 format(2a)
15 format(a,i2)
25 format(f3.1,a)
35 format(a,f7.1)
45 format(a,f5.1)
!
end subroutine cpbl
!
!     ==================================================================
!>    @brief unbalance response plot.
!
!>    @param[in] rpg speed rpm
!>    @param[in] amp complex amplitudes matrix in m
!>    @param[in] nap number of amplitude points
!>    @param[in] nr number of speeds
!>    @param[in] mxp amplitude first and rpg dimension
!>    @param[in] mte amplitude second dimension
!>    @param[in] frl true if plot vertical scale is logarithmic
!>    @param[in] stdio flag for standard input and output
!>    @param[in] pfn output file name, used for non standard output
!>    @param[out] errmsg return an error message
!>    @param[out] ok return flag. unsuccessful if <0.
!
subroutine unbr(rpg,amp,nap,nr,mxp,mte,frl,&
&stdio,pfn,errmsg,ok)
  use rd_textfun, only: pltmsgf
  use com_cpbn, only: spdn
  use com_opt, only: std
  use com_unb, only: nini_r, nfin_r, dw_r
  use com_unb0, only: pr, desp, ori, np, nm
  use rd_kinds, only: lrk, wp
  implicit none
!
  integer :: nap, nr, mxp, mte, ok
  real(lrk) :: rpg, getaxbf
  real(lrk) :: xv, zv ! included xv,zv - angelo jun-22
  complex(wp) :: amp
  dimension rpg(mte),amp(mxp,mte)
  character(len=99) :: errmsg
  character(len=255) :: pfn
  logical :: frl, stdio
!
  integer :: i, j, k, mki, mxki, npi
  real(lrk) :: an, am, mn, mx, dl, sc
  real(lrk) :: todegf, todegpf, rargf, degseqf
!     output section name
  character(len=1) :: pos, blank, cp
  character(len=3) :: c3
  character(len=4) :: nmm, c4
  character(len=7) :: cps
  character(len=9) :: c9
  character(len=10) :: sec
!     plot messages
  character(len=30) :: crd
  character(len=30) :: buf !* buf - angelo jun-22
  character(len=50) :: txt
  dimension c3(2),cp(5)
  parameter(blank = ' ',c3 = (/'log','(m)'/),&
  &nmm = 'unbr',c4 = '.plt',c9 = 'pos. (mm)',&
  &cp = (/'?','h','v','@','-'/),sc = 1000)
!     malhap stuff
  integer :: ft, format, xachse, yachse, color
  real(lrk) :: nul, maxrig, maxtop, minbot, minlef, xwert, ywert
  dimension xwert(mte+2),ywert(mte+2)
  character(len=8) :: zeit, prname
  character(len=10) :: datum
  character(len=30) :: name, abszis, ordina
  character(len=31) :: titelo, titelu
  character(len=70) :: reke
!     unbalance
  integer :: mxr
  parameter (mxr = 9)
!     std plot output

!     nominal speed (rpm) - angelo jun-22
!
  intrinsic :: abs, real
!
!     initialize
  ok = -1
  errmsg = blank
!     standard io
  std = stdio
!     malhap settings
  format = 5
  xachse = 1
  yachse = 1
  nul = 1E-12_lrk
!     speed range
  minlef = rpg(1)
  maxrig = rpg(nr)
!     titles
  call header(reke,zeit,datum,name,titelu)
  abszis = pltmsgf(30, 1)
  ordina = blank
  titelo = pltmsgf(30, 7)
  prname = nmm
!     standard plot marker
  if (stdio) then
    write(sec,5)nmm,c4
    call marksec(sec,0)
  end if
!     kind index
  mki = 0
!     max kind index
  call mxmarks(mxki)
!     line colors
  color = 1
!     initialize hpgl plot
  call plots(0,0,pfn)
!
!     limits
  mn = 1E9_lrk
  mx = -1e9_lrk
  do i = 1,nap
    do j = 1,nr
!         check for log scale
      if (.not. frl) then
!           linear
        am = real(abs(amp(i,j)), lrk)
      else
!           logarithmic
        am = log10(real(abs(amp(i,j)), lrk))
      end if
      if(am .le. mn) mn = am
      if(am .ge. mx) mx = am
    end do
  end do
  minbot = mn
  maxtop = mx
!     'Displacement Amplitude' !30
  txt = pltmsgf(30, 30)
  i = len_trim(txt)

  if (.not. frl) then
!       linear
    write(ordina,45) txt(1:i),blank,c3(2)
  else
!       log
    write(ordina,55) txt(1:i),cp(5),c3(1),blank,c3(2)
  end if
!
!     plot frequency lines
  ft = 1
  do i = 1,nap
    do j = 1,nr
      if (.not. frl) then
        am = real(abs(amp(i,j)), lrk)
      else
        am = log10(real(abs(amp(i,j)), lrk))
      end if
      xwert(j) = rpg(j)
      ywert(j) = am
    end do
    if (.not. frl) then
      call malhap (nr, format, ft, mte, xachse, yachse,&
      &nul, maxrig, maxtop, minbot, minlef,&
      &xwert, ywert,&
      &reke, zeit, datum, abszis, ordina,&
      &name, titelo, titelu, prname,color)
    else
      call malhapax (nr, format, ft, mte, xachse, yachse,&
      &nul, maxrig, maxtop, minbot, minlef,&
      &xwert, ywert,&
      &reke, zeit, datum, abszis, ordina,&
      &name, titelo, titelu, prname,color,.false.,.true.)
    end if
!       marks
    call marks(xwert,ywert,80._lrk,nr,mte+2,8,mki)
    if (ft .eq. 2) then
      crd = c9
      k = len_trim(crd)
      call symbol (1._lrk,18._lrk,.28_lrk,crd(1:k),0,0._lrk,k)
    end if
!       response position info (*1000 = mm)
!       position info 1=horizontal, 2=vertical
    pos = cp(1)
    if (desp(i) .eq. 1._lrk) then
      pos = cp(2)
    else if (desp(i) .eq. 2._lrk) then
      pos = cp(3)
    end if
!       check suuport < 0
    if (.not. pr(i) .lt. 0) then
!         position (mm)
      write (cps,25) pr(i)*sc
    else
!         'supp='
      crd = pltmsgf(30, 66)
      k = len_trim(crd)
      write (cps,35) crd(1:k),nint(abs(pr(i)))
    end if
!       rad -> deg
    an = todegf(ori(i))
    write(crd,15) pos,cps,cp(4),an
    k = len_trim(crd)
    dl = 17.6_lrk-(i-1)*0.38_lrk
    call legend (1._lrk,dl,mki,.28_lrk,color,k,crd(1:k))
!       marker id
    mki = mki+1
    if (mki .gt. mxki) mki = 0
  end do

!     rated speed - angelo jun-22
  if (spdn .gt. 0) then
    color = 2
    xwert(1) = spdn
    xwert(2) = spdn
!       get y scale min and max values
    xv = getaxbf(.true.,.false.)
    zv = getaxbf(.true.,.true.)
    ywert(1) = xv
    ywert(2) = zv
    call rscale(xwert,ywert,npi,2)
    ft = ft+1
    call malhap (2, format, ft, npi, xachse, yachse,&
    &nul, maxrig, maxtop, minbot, minlef,&
    &xwert, ywert,&
    &reke, zeit, datum, abszis, ordina,&
    &name, titelo, titelu, prname,color)
!       rated speed
    buf = pltmsgf(30, 64)
    k = len_trim(buf)
    write(txt,65) buf(1:k),spdn
    k = len_trim(txt)
    call symbol (15.5_lrk,17.0_lrk,.25_lrk,txt(1:k),0,0._lrk,k)
    color = 1
    call pen (abs(color))
  end if

!     eventual speed extrapolation message, see parmanv.f
  call eplwrg(-1,txt)
  k = len_trim(txt)
  if (k .gt. 0) call symbol (9.0_lrk, 18.0_lrk, 0.2_lrk, txt(1:k), 0, 0._lrk, k)
!
!     angle radian
  mn = 1E9_lrk
  mx = 0._lrk
  do i = 1,nap
    dl = 0._lrk
    do j = 1,nr
      an = rargf(amp(i,j))
      am = todegpf(an,1)
!         angle sequence correction
      if (j .gt. 1) am = degseqf(am,dl)
      dl = am
      if(am .le. mn) mn = am
      if(am .ge. mx) mx = am
    end do
  end do
  minbot = mn
  maxtop = mx
!     'angle (deg)' !9
  ordina = pltmsgf(30, 9)
!
!     reset scales
  call sreset(3)
  mki = 0
  ft = 1
  do i = 1,nap
    dl = 0._lrk
    do j = 1,nr
      an = rargf(amp(i,j))
!         degree
      am = todegpf(an,1)
!         angle sequence correction
      if (j .gt. 1) am = degseqf(am,dl)
      dl = am
      xwert(j) = rpg(j)
      ywert(j) = am
    end do
    call malhap (nr, format, ft, mte, xachse, yachse,&
    &nul, maxrig, maxtop, minbot, minlef,&
    &xwert, ywert,&
    &reke, zeit, datum, abszis, ordina,&
    &name, titelo, titelu, prname,color)
!       marks
    call marks(xwert,ywert,80._lrk,nr,mte+2,8,mki)
    if (ft .eq. 2) then
      crd = c9
      k = len_trim(crd)
      call symbol (1._lrk,18._lrk,.28_lrk,crd(1:k),0,0._lrk,k)
    end if
    pos = cp(1)
    if (desp(i) .eq. 1._lrk) then
      pos = cp(2)
    else if (desp(i) .eq. 2._lrk) then
      pos = cp(3)
    end if
!       check suuport < 0
    if (.not. pr(i) .lt. 0) then
!         position (mm)
      write (cps,25) pr(i)*sc
    else
!         'supp='
      crd = pltmsgf(30, 66)
      k = len_trim(crd)
      write (cps,35) crd(1:k),nint(abs(pr(i)))
    end if
    write(crd,15) pos,cps,cp(4),an
!       rad -> deg
    an = todegf(ori(i))
    write(crd,15) pos,cps,cp(4),an
    k = len_trim(crd)
    dl = 17.6_lrk-(i-1)*0.38_lrk
    call legend (1._lrk,dl,mki,.28_lrk,color,k,crd(1:k))
!
!       marker id
    mki = mki+1
    if (mki .gt. mxki) mki = 0
!
  end do
!
!     eventual speed extrapolation message, see parmanv.f
  call eplwrg(-1,txt)
  k = len_trim(txt)
  if (k .gt. 0) call symbol (9.0_lrk, 18.0_lrk, 0.2_lrk, txt(1:k), 0, 0._lrk, k)

!     standard plot marker
  if (stdio) then
    call bflush
    call marksec(sec,1)
  else
    call plots(1,0,pfn)
  end if

!     return ok
  ok = 0
!
  return
!
5 format(2a)
15 format(3a,f5.1)
25 format(f7.1)
35 format(a,i2)
45 format(3a)
55 format(5a)
65 format(a,f7.1) ! angelo jun-22
!
end subroutine unbr
!
!     =================================================================
!>    @brief plot elastic line.
!
!>    @param[in] nb number of bearings lateral analysis
!>    @param[in] nt number of section positions (y coordinates)
!>    @param[in] ns number of shear section positions (y coordinates)
!>    @param[in] py section position vector (m)
!>    @param[in] dl general dispacement vector x,z,fi,theta
!>    @param[in] bs bending stress on all sections (Pa)
!>    @param[in] mi bending moment on section coordinates y (Nm)
!>    @param[in] yh shear force on section coordinates (m)
!>    @param[in] si constant shear force on ys section coordinates (m)
!>    @param[in] isf bending stress median filter width
!>    @param[in] rm general bearing reaction vector x,z,mom fi,mom theta
!>    @param[in] bst total bearing stiffness column 1=x,2=z.
!>    @param[in] mts section vector dimension
!>    @param[in] mtg displacemtne vector dimension
!>    @param[in] mxr bearing reaction vector dimension
!>    @param[in] mxb bearing total stiffness dimension
!>    @param[in] lst flag using bearing stiffness
!>    @param[in] stdio flag standard input and output
!>    @param[in] pfn output file name, used for non standard output
!
subroutine elin(nb,nt,ns,py,dl,bs,&
&mi,yh,si,isf,rm,bst,mts,mtg,mxr,mxb,&
&lst,stdio,pfn)
  use rd_textfun, only: ciff, pltmsgf
  use com_opt, only: std
  use com_ymc, only: nbrg, rks, pc
  use rd_kinds, only: lrk, wp
  implicit none
!
  integer :: nb, nt, ns, isf, mts, mtg, mxr, mxb
  real(lrk) :: py, rm, bst, bs, mi, yh, si
  real(wp) :: dl
  dimension isf(2),py(mts),rm(mxr),dl(mtg),bst(mxb,2),&
  &bs(mts),mi(mts,2),yh(mts),si(mts,2)
  character(len=255) :: pfn
  logical :: lst, stdio
!
!     local
  integer :: iiff, ifidxof
  integer :: i, j, k, l, m, np
  real(lrk) :: todegf, rpif, riff
  real(lrk) :: an, am, mn, mx, my, ny, rg, un, ss, sc, as, x, y, ys, xp, yp, zp, br, c, v
  character(len=1) :: cop
  character(len=2) :: bn, cds
  character(len=3) :: crd
  character(len=4) :: nmm
  character(len=6) :: ccr
  dimension br(2),c(4,4), v(4,4),cop(4),nmm(3),crd(5)
  parameter(cop = (/':','r','m',' '/),cds = '**',&
  &nmm = (/'elin','.plt','(kN)'/),&
  &ccr = '(kN/m)',crd = (/'x  ','z  ','fi ','psi','(m)'/))
!
!     output section name
  character(len=10) :: sec
!     plot messages
  character(len=30) :: ptxt
!     bearing reaction
  character(len=50) :: react
!     malhap stuff
  integer :: ft, format, xachse, yachse, color
  real(lrk) :: nul, maxrig, maxtop, minbot, minlef
  real(lrk) :: xwert(ns+2), ywert(ns+2)
  character(len=8) :: zeit, prname
  character(len=10) :: datum
  character(len=30) :: name, abszis, ordina
  character(len=31) :: titelo, titelu
  character(len=70) :: reke
!
!     bearing position dimension
  integer :: mxm
  parameter (mxm = 9)
!     positions
!     number of bearings
!     std plot output
!
!     coordinate names
!     3d scalings
  data br/0._lrk,0._lrk/,ss/1000._lrk/,sc/500.0_lrk/,as/0.05_lrk/,ys/15._lrk/,np/31/,mx/0/,mn/0/,my/0/,ny/0/
!
  intrinsic :: abs, cos, len_trim, real, sqrt, sin
!
!     standard io
  std = stdio
!     malhap settings
  format = 5
  xachse = 1
  yachse = 1
  nul = 1E-9_lrk
!     x shaft length
  minlef = py(1)
  maxrig = py(nt)
!     titles
  call header(reke,zeit,datum,name,titelu)
!     y(m)
  abszis = pltmsgf(30, 46)
  ordina = cop(4)
!     'Static Elastic Line' !11
  titelo = pltmsgf(30, 11)
  prname = nmm(1)
!     standard plot marker
  if (stdio) then
    write(sec,5)nmm(1),nmm(2)
    call marksec(sec,0)
  end if
!     max reactions, 2 displacements (index=1),
!                    2 rotations (index=2)
  do i = 1,nb
!        index x,z,fi,psi
    j = ifidxof(i,1)
    if (abs(rm(j)) .ge. abs(br(1))) br(1) = rm(j)
    if (abs(rm(j+1)) .ge. abs(br(1))) br(1) = rm(j+1)
    if (abs(rm(j+2)) .ge. abs(br(2))) br(2) = rm(j+2)
    if (abs(rm(j+3)) .ge. abs(br(2))) br(2) = rm(j+3)
  end do
!     initialize hpgl plot
  call plots(0,0,pfn)
!     force zero x axis to be plot
  call forzax(1,.true.)
!     z x
!     |/_ y
!     coordinates loop: x, z, rot x (fi), rot z (psi)
  do i = 1,4
!       recalculate scales
    call sreset(3)
    mn = 1E9_lrk
    mx = 0._lrk
    do k = 1,nt
      j = ifidxof(k,i)
!         displacements
      am = real(dl(j), lrk)
!         i<3 => displacement
!                rotation, angles -> deg
      am = riff(i.lt.3,am,todegf(am))
!         max /min
      if(am .le. mn) mn = am
      if(am .ge. mx) mx = am
      xwert(k) = py(k)
      ywert(k) = am
    end do
!       range
    rg = mx-mn
    minbot = mn
    maxtop = mx
!       check for zero at top, give a margin
    if (maxtop .eq. 0) maxtop = 0.05_lrk*abs(mn)
    m = len_trim(crd(i))
    if (i .lt. 3) then
!         'Displacement Amp.' !30
      ptxt = pltmsgf(30, 30)
      j = len_trim(ptxt)
      write(ordina,65)ptxt(1:j),cop(1),crd(i)(1:m),crd(5)
    else
!       'angle (deg)' !9
      ptxt = pltmsgf(30, 9)
      j = len_trim(ptxt)
      write(ordina,15)ptxt(1:j),cop(1),crd(i)(1:m)
    end if
!       line color, blue
    color = 5
    ft = 1
    call malhap (nt, format, ft, ns, xachse, yachse,&
    &nul, maxrig, maxtop, minbot, minlef,&
    &xwert, ywert,&
    &reke, zeit, datum, abszis, ordina,&
    &name, titelo, titelu, prname,color)
!       bearing reactions, calculates a proportional line length
!       i<3 => displacements
!              rotations
    m = iiff(i.lt.3,1,2)
!       magenta
    color = 6
!       scale stuff, two points
    call rscale(xwert,ywert,ns,2)
    do k = 1,nb
!         reaction index for x,z,fi,psi
      j = ifidxof(k,i)
      xwert(1) = pc(k)
      ywert(1) = 0._lrk
      xwert(2) = pc(k)
      ywert(2) = 0.1_lrk*rg*rm(j)/abs(br(m))
      ft = 2+k
      call malhap (2, format, ft, ns, xachse, yachse,&
      &nul, maxrig, maxtop, minbot, minlef,&
      &xwert, ywert,&
      &reke, zeit, datum, abszis, ordina,&
      &name, titelo, titelu, prname,color)
!         bearing number
      if (k.lt.100) then
        write (bn,25) k
!         over 99 bearings, should not ever happen.
      else
        bn = cds
      end if
!         x position
      am = pc(k)+maxrig*0.01_lrk
!         y position
      mn = 0._lrk+(maxtop-minbot)*0.01_lrk
      call  csymbol (am,mn,.25_lrk,bn,0,0._lrk,2)
!         bearing reaction values list
      l = len_trim(crd(i))
      ptxt(1:1) = ciff(30, i .lt. 3,cop(2),cop(3))
!         /1000 -> kN, kNm
      write(react,35)ptxt(1:1),crd(i)(1:l),bn,rm(j)/1e3_lrk
      call pen(1)
!         unit
      if (k .eq.1) then
        ptxt(1:6) = ciff(30, i .lt. 3,nmm(3),ccr)
        call symbol (4.5_lrk,17.1_lrk,.28_lrk,ptxt(1:6),0,0._lrk,6)
      end if
!         y offset
      am = 17.6_lrk-k*0.4_lrk
      l = len_trim(react)
      call symbol (0.5_lrk,am,.28_lrk,react(1:l),0,0._lrk,l)
!         bearing stiffness
      if (lst) then
        write(react,45)bn,crd(1),nint(bst(k,1)/1e6_lrk),crd(2),nint(bst(k,2)/1e6_lrk)
        l = len_trim(react)
        call symbol (14._lrk,am,.28_lrk,react(1:l),0,0._lrk,l)
      end if
!         bearing loop
    end do
!       coordinate loop
  end do
!
!     composite 3d view
  xachse = 0
  yachse = 0
  abszis = cop(4)
  ordina = cop(4)
!     set 3d world
  call wset(0._lrk,0._lrk,0._lrk,0._lrk,0._lrk,0._lrk,1._lrk,c)
!     set 3d camera view
  call cset(-0.1_lrk,-0.75_lrk,0.1_lrk,2._lrk,-3._lrk,-3._lrk,1.0_lrk,v)
!     length of the 3d coordinate axis
  un= (py(nt)-py(1))*as
!     calcultate 3d scale
!     search for the max amplitude
  do i = 1,nt
    j = ifidxof(i,1)
    am = real(sqrt(dl(j)**2 + dl(j+1)**2), lrk)
    if (i .eq. 1) then
      rg = am
    else
      if (am .gt. rg)rg = am
    end if
  end do
!     shaft length / maximum displacement.
  sc =py(nt)/rg/ys
!     search for the max x and y
  do i = 1,nt
    yp = py(i)
    j = ifidxof(i,1)
    am = real(sqrt(dl(j)**2+dl(j+1)**2), lrk)
    do j = 0,np-1
      an = 2 * rpif()*j/(np-1)
      xp = am * cos(an)
      zp = am * sin(an)
      call get2d(c,v,xp*sc,zp*sc,yp,0._lrk,0._lrk,ss,x,y)
      if (i .eq. 1 .and. j .eq. 0) then
        mx = x
        mn = x
        my = y
        ny = y
      else
        if (x.gt.mx) mx = x
        if (x.lt.mn) mn = x
        if (y.gt.my) my = y
        if (y.lt.ny) ny = y
      end if
    end do
  end do
!
!     maximum and minimum
  maxrig = mx
  minlef = mn
  maxtop = my
  minbot = ny
!     plot page shaft displacement
  ft = 1
!     reset draw scales
  call sreset(3)
  color = 2
  do i = 1,nt
    yp = py(i)
    j = ifidxof(i,1)
    xp = real(dl(j), lrk)
    zp = real(dl(j+1), lrk)
    call get2d(c,v,xp*sc,zp*sc,yp,0._lrk,0._lrk,ss,x,y)
    xwert(i) = x
    ywert(i) = y
  end do
  call malhap (nt, format, ft, ns, xachse, yachse,&
  &nul, maxrig, maxtop, minbot, minlef,&
  &xwert, ywert,&
  &reke, zeit, datum, abszis, ordina,&
  &name, titelo, titelu, prname,color)
!     point marker (must first call malhap to scaling)
  call pen(2)
  do i = 1,nt
    call circle(xwert(i),ywert(i),15._lrk)
  end do
!     shaft center line
  color = 1
  ft = ft+1
  do i = 1,nt
    yp = py(i)
    xp = 0._lrk
    zp = 0._lrk
    call get2d(c,v,xp*sc,zp*sc,yp,0._lrk,0._lrk,ss,x,y)
    xwert(i) = x
    ywert(i) = y
  end do
  call malhap (nt, format, ft, ns, xachse, yachse,&
  &nul, maxrig, maxtop, minbot, minlef,&
  &xwert, ywert,&
  &reke, zeit, datum, abszis, ordina,&
  &name, titelo, titelu, prname,color)
!     shaft circles, np points
!     reset scale position
  color = 5
  call rscale(xwert,ywert,ns,np)
  do i = 1,nt
    yp = py(i)
    j = ifidxof(i,1)
!       displacement radius
    am = real(sqrt(dl(j)**2+dl(j+1)**2), lrk)
!       circle points
    do j = 0,np-1
      an = 2*rpif()*j/(np-1)
      xp = am*cos(an)
      zp = am*sin(an)
      call get2d(c,v,xp*sc,zp*sc,yp,0._lrk,0._lrk,ss,x,y)
      xwert(j+1) = x
      ywert(j+1) = y
    end do
    ft = ft + 1
!        call malhap (np, format, ft, mts, xachse, yachse,
!     & nul, maxrig, maxtop, minbot, minlef,
!     & xwert, ywert,
!     & reke, zeit, datum, abszis, ordina,
!     & name, titelo, titelu, prname,color)
  end do
!     shaft torsion
!     reset scale position
  call rscale(xwert,ywert,ns,2)
  do i = 1,nt
    yp = py(i)
    xp = 0._lrk
    zp = 0._lrk
    call get2d(c,v,xp*sc,zp*sc,yp,0._lrk,0._lrk,ss,x,y)
    xwert(1) = x
    ywert(1) = y
    j = ifidxof(i,1)
    xp = real(dl(j), lrk)
    zp = real(dl(j+1), lrk)
    call get2d(c,v,xp*sc,zp*sc,yp,0._lrk,0._lrk,ss,x,y)
    xwert(2) = x
    ywert(2) = y
    ft = ft+1
    call malhap (2, format, ft, ns, xachse, yachse,&
    &nul, maxrig, maxtop, minbot, minlef,&
    &xwert, ywert,&
    &reke, zeit, datum, abszis, ordina,&
    &name, titelo, titelu, prname,color)
  end do
  ft = ft+1
!     3d coordinate axis
  call axis3d(un,ss,ns,c,v,format, ft, xachse, yachse,&
  &nul, maxrig, maxtop, minbot, minlef,&
  &reke, zeit, datum, abszis, ordina,&
  &name, titelo, titelu, prname)
!     plot name
  call pen(1)
  react = pltmsgf(30, 12)
  j = len_trim(react)
  call symbol (0.5_lrk,17.1_lrk,.28_lrk,react(1:j),0,0._lrk,j)
!
!     bending stress moment and shear force
!
  xachse = 1
  yachse = 1
  ptxt = pltmsgf(30, 58)
  j = len_trim(ptxt)
  titelo = ptxt(1:j)
  abszis = pltmsgf(30, 46)
  write(ordina,15) ptxt(1:j),cop(1),crd(1)(1:1)
!     x shaft length
  minlef = py(1)
  maxrig = py(nt)
  mn = 1E9_lrk
  mx = -1E9_lrk
  color = 5
!
  do k = 1,nt
!       max /min
    if(mi(k,1) .le. mn) mn = mi(k,1)
    if(mi(k,1) .ge. mx) mx = mi(k,1)
    xwert(k) = py(k)
    ywert(k) = mi(k,1)
  end do
  maxtop = mx
  minbot = mn
  ft = 1
  call malhap (nt, format, ft, ns, xachse, yachse,&
  &nul, maxrig, maxtop, minbot, minlef,&
  &xwert, ywert,&
  &reke, zeit, datum, abszis, ordina,&
  &name, titelo, titelu, prname,color)
!
  write(ordina,15)ptxt(1:j),cop(1),crd(2)(1:1)
  mn = 1E9_lrk
  mx = -1E9_lrk
  do k = 1,nt
!       max /min
    if(mi(k,2) .le. mn) mn = mi(k,2)
    if(mi(k,2) .ge. mx) mx = mi(k,2)
    ywert(k) = mi(k,2)
  end do
  maxtop = mx
  minbot = mn
  ft = 1
  call malhap (nt, format, ft, ns, xachse, yachse,&
  &nul, maxrig, maxtop, minbot, minlef,&
  &xwert, ywert,&
  &reke, zeit, datum, abszis, ordina,&
  &name, titelo, titelu, prname,color)
!
!     shear force
  ptxt = pltmsgf(30, 59)
  j = len_trim(ptxt)
  titelo = ptxt(1:j)
  write(ordina,15)ptxt(1:j),cop(1),crd(1)(1:1)
!
  mn = 1E9_lrk
  mx = -1E9_lrk
  do k = 1,2*(nt-1)
!       max /min
    if(si(k,1) .le. mn) mn = si(k,1)
    if(si(k,1) .ge. mx) mx = si(k,1)
    xwert(k) = yh(k)
    ywert(k) = si(k,1)
  end do
  maxtop = mx
  minbot = mn
  ft = 1
  call malhap (2*(nt-1), format, ft, ns, xachse, yachse,&
  &nul, maxrig, maxtop, minbot, minlef,&
  &xwert, ywert,&
  &reke, zeit, datum, abszis, ordina,&
  &name, titelo, titelu, prname,color)
!     filter width text
  ptxt = pltmsgf(30, 60)
  k = len_trim(ptxt)
  write (react,55)ptxt(1:k),isf(1)
  k = len_trim(react)
!     color black
  call pen(1)
  call symbol (0.5_lrk,17.1_lrk,.3_lrk,react(1:k),0,0._lrk,k)
!
  ptxt = pltmsgf(30, 59)
  j = len_trim(ptxt)
  write(ordina,15) ptxt(1:j),cop(1),crd(2)(1:1)
  mn = 1E9_lrk
  mx = -1E9_lrk
  do k = 1,2*(nt-1)
!       max /min
    if(si(k,2) .le. mn) mn = si(k,2)
    if(si(k,2) .ge. mx) mx = si(k,2)
    ywert(k) = si(k,2)
  end do
  maxtop = mx
  minbot = mn
  ft = 1
  call malhap (ns, format, ft, ns, xachse, yachse,&
  &nul, maxrig, maxtop, minbot, minlef,&
  &xwert, ywert,&
  &reke, zeit, datum, abszis, ordina,&
  &name, titelo, titelu, prname,color)
!     filter width text
  ptxt = pltmsgf(30, 60)
  k = len_trim(ptxt)
  write (react,55) ptxt(1:k),isf(2)
  k = len_trim(react)
!     clolor black
  call pen(1)
  call symbol (0.5_lrk,17.1_lrk,.3_lrk,react(1:k),0,0._lrk,k)
!
!     bending stress
  ptxt = pltmsgf(30, 65)
  j = len_trim(ptxt)
  titelo = ptxt(1:j)
  write(ordina,15) ptxt(1:j),cop(1),crd(1)(1:1)
!
  ptxt = pltmsgf(30, 65)
  j = len_trim(ptxt)
  titelo = ptxt(1:j)
  write(ordina,5) ptxt(1:j),cop(4)
!
  mn = 1E9_lrk
  mx = -1E9_lrk
  do k = 1,nt
    yp = bs(k)/1e6_lrk
!       max /min
    if(yp .le. mn) mn = yp
    if(yp .ge. mx) mx = yp
    xwert(k) = py(k)
    ywert(k) = yp
  end do
  maxtop = mx
  minbot = mn
  ft = 1
  call malhap (nt, format, ft, ns, xachse, yachse,&
  &nul, maxrig, maxtop, minbot, minlef,&
  &xwert, ywert,&
  &reke, zeit, datum, abszis, ordina,&
  &name, titelo, titelu, prname,color)
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
15 format(3a)
25 format(i2)
35 format(3a,f7.2)
45 format(a,' kN/mm',2(1x,a1,':',i4))
55 format(a,i2)
65 format(4a)
!
end subroutine elin
!
!     ==================================================================
!>    @brief plot critical speed map.
!
!>    @param[in] spd current speed
!>    @param[in] vr stiffnesses vector
!>    @param[in] rcr critical speed matrix
!>    @param[in] sped ni output speed points
!>    @param[in] spdn rated speed (rpm)
!>    @param[in] xx ni output bearing kxx stiffness points
!>    @param[in] zz ni output bearing kzz stiffness points
!>    @param[in] nk number of stiffness
!>    @param[in] npi number of pu points to interpolate, pu dimension
!>    @param[in] bn number of bearing stifness
!>    @param[in] ncc number of critical speeds
!>    @param[in] mxc critical speed matrix rows number
!>    @param[in] mxk critical speed matrix column number
!>     and speed vector dimension
!>    @param[in] mxm spd,xx,zz matrices first dimension, second is npi,
!>     should be equal to maximum bearing number, mxm parameter
!>    @param[in] ang true for angular stiffness map
!>    @param[in] stdio flag for standard input and output
!>    @param[in] bok will be true when all spd,xx,zz pu points
!>     where calculated ok
!>    @param[in] pfn output file name, used for non standard output
!
subroutine csmp(spd,vr,rcr,sped,spdn,xx,zz,nk,npi,bn,&
&ncc,mxc,mxk,mxm,ang,stdio,bok,pfn)
  use rd_textfun, only: pltmsgf
  use com_opt, only: std
  use rd_kinds, only: lrk
  implicit none
!
  integer :: nk, npi, bn, ncc, mxk, mxc, mxm
  real(lrk) :: spd, vr, rcr, sped, spdn, xx, zz
  dimension vr(mxk),rcr(mxc,mxk),&
  &sped(mxm,npi),xx(mxm,npi),zz(mxm,npi)
  character(len=255) :: pfn
  logical :: ang, stdio, bok
!
  character(len=1) :: ddot
  character(len=2) :: brn
  character(len=4) :: c4, nmm
!     plot messages
  character(len=30) :: buf
  integer :: i, j, k, iiff
  real(lrk) :: am, mnv, mxv, mv, dv, ms, bno, bns, rad2rpmf
  dimension nmm(2),bno(2)
!     marker size
!     bno - bearing number text position offset
!     bns - bearing number text size
  parameter (ddot = ':',c4 = '.plt',nmm = (/'csmp','acsm'/),ms = 40._lrk,bno = (/0.01_lrk,0.005_lrk/),bns = 0.16_lrk)
!
  logical :: xok, zok
  data xok/.false./,zok/.false./
!
  character(len=30) :: pmsg
!     output section name
  character(len=10) :: sec
!     malhap stuff
  integer :: ft, format, xachse, yachse, color
  real(lrk) :: nul, maxrig, maxtop, minbot, minlef, xwert, ywert
  dimension xwert(mxk+2),ywert(mxk+2)
  character(len=8) :: zeit, prname
  character(len=10) :: datum
  character(len=30) :: name, abszis, ordina
  character(len=31) :: titelo, titelu
  character(len=70) :: reke
!
!     std plot output
!
  intrinsic :: len_trim, log10
!
!     standard io
  std = stdio
!     malhap settings
  format = 5
  xachse = 1
  yachse = 1
  nul = 1E-12_lrk
!     titles
  call header(reke,zeit,datum,name,titelu)
  ordina = pltmsgf(30, 1)
  if (.not. ang) then
    prname = nmm(1)
    titelo = pltmsgf(30, 13)
    abszis = pltmsgf(30, 14)
  else
    prname = nmm(2)
    titelo = pltmsgf(30, 15)
    abszis = pltmsgf(30, 16)
  end if
!     standard plot marker
  if (stdio) then
    i = iiff(.not. ang,1,2)
    write(sec,5) nmm(i),c4
    call marksec(sec,0)
  end if
!     maximal and minimal critical speed rpm
  mnv = 1E9_lrk
  mxv = 0._lrk
  do i = 1,ncc
    do j = 1,nk
!         speed rpm->am
      am = rad2rpmf(rcr(i,j))
      if(am .le. mnv) mnv = am
      if(am .ge. mxv) mxv = am
    end do
  end do
!     maximum and minimum
  maxrig = log10(vr(nk))
  minlef = log10(vr(1))
  maxtop = log10(mxv)
  minbot = log10(mnv)
!     scaling, only for logarithmic
  call scaling(minlef,maxrig,20._lrk,nul,mv,dv)
  call ska(mv,dv*20._lrk,dv,1)
  call scaling(minbot,maxtop,16._lrk,nul,mv,dv)
  call ska(mv,dv*16._lrk,dv,2)
!
  color = 5
!     initialize hpgl plot
  call plots(0,0,pfn)
!
  do i = 1,ncc
    do j = 1,nk
      xwert(j) = log10(vr(j))
      am = rad2rpmf(rcr(i,j))
      ywert(j) = log10(am)
    end do
!
    ft = i
    call malhapax(nk, format, ft, mxk, xachse, yachse,&
    &nul, maxrig, maxtop, minbot, minlef,&
    &xwert, ywert,&
    &reke, zeit, datum, abszis, ordina,&
    &name, titelo, titelu, prname,color,&
    &.true.,.true.)
!
  end do
!
!     fixed npi points of bearing stiffness
  if (bok .and. .not. ang) Then
!       bearing stiffness legend control
    xok = .false.
    zok = .false.
!       number of bearings
    do i = 1,bn
!         bearing number
      brn = '-'
      if (i .lt. 10) write(brn,15) i
      if (i .ge. 10 .and. i .lt. 100) write(brn,25) i
!         x stiffness
      k = 0
      do j = 1,npi
        mxv = log10(xx(i,j))
        mnv = log10(sped(i,j))
!           check range
        if (mxv .ge.  minlef  .and. mxv .le. maxrig .and.&
        &mnv .ge.  minbot  .and. mnv .le. maxtop) then
!             value in display range
          k = k+1
          xwert(k) = mxv
          ywert(k) = mnv
!             x legend
          xok = .true.
        end if
      end do
      if (k .gt. 1) then
!           moving scaling factors
        call rscale(xwert,ywert,mxk,k)
!           magenta for x
        color = 6
        ft = ft+1
        call malhapax(k, format, ft, mxk, xachse, yachse,&
        &nul, maxrig, maxtop, minbot, minlef,&
        &xwert, ywert,&
        &reke, zeit, datum, abszis, ordina,&
        &name, titelo, titelu, prname,color,&
        &.true.,.true.)
!           triangle marker at end
        dv = 1-bno(1)
        am = 1+(i-1)*bno(2)/1.2_lrk
        call mark(xwert(k),ywert(k), ms, 3)
        call csymbol(xwert(1)*am,ywert(1)*dv,bns,brn,0, 0._lrk,2)
      end if
!         z stiffness
      k = 0
      do j = 1,npi
        mxv = log10(zz(i,j))
        mnv = log10(sped(i,j))
!           check range
        if (mxv .ge.  minlef  .and. mxv .le. maxrig .and.&
        &mnv .ge.  minbot  .and. mnv .le. maxtop) then
          k = k+1
          xwert(k) = mxv
          ywert(k) = mnv
!             z legend
          zok = .true.
        end if
      end do
      if (k .gt. 1) then
!           red for z
        color = 2
        ft = ft+1
        call malhapax(k, format, ft, mxk, xachse, yachse,&
        &nul, maxrig, maxtop, minbot, minlef,&
        &xwert, ywert,&
        &reke, zeit, datum, abszis, ordina,&
        &name, titelo, titelu, prname,color,&
        &.true.,.true.)
!           square marker at begin
        dv = 1+bno(2)
        am = 1+(i-1)*bno(2)/1.2_lrk
        call mark(xwert(1),ywert(1), ms, 2)
        call csymbol(xwert(k)*am,ywert(k)*dv,bns,brn,0, 0._lrk,2)
      end if
!
    end do
!
  end if
!
!     annotations
!     gyroscopic effect
  if (.not. ang .and. spd .eq. 0)then
!       gyroscopic effect message
    pmsg = pltmsgf(30, 17)
  else
!       speed message
    buf = pltmsgf(30, 18)
    j = len_trim(buf)
    write(pmsg,'(2a,f6.0)')buf(1:j),ddot,spd
  end if
!
  call pen(1)
  j = len_trim(pmsg)
  call symbol (0.5_lrk,17.1_lrk,.28_lrk,pmsg(1:j),0,0._lrk,j)
!
  if (bok .and. .not. ang) then
    if (xok) then
!         stiffness x
      pmsg = pltmsgf(30, 19)
      j = len_trim(pmsg)
      call pen(1)
      call symbol (0.8_lrk,16.7_lrk,.25_lrk,pmsg(1:j),0,0._lrk,j)
!         stiffness marker
!         magenta triangle, x
      call pen(6)
!         size<0 use plot coordinates
      call mark(0.5_lrk,16.8_lrk,-ms,3)
    end if
    if (zok) then
!         stiffness z
      pmsg = pltmsgf(30, 20)
      j = len_trim(pmsg)
      call pen(1)
      call symbol (4.3_lrk,16.7_lrk,.25_lrk,pmsg(1:j),0,0._lrk,j)
!         stiffness marker
!         red square z
      call pen(2)
      call mark(4.0_lrk,16.8_lrk,-ms,2)
    end if
  end if
!     rated speed
  if (spdn .gt. 0) then
    am = log10(spdn)
!       check y range
    if (am .ge. minbot .and. am .le. maxtop) then
!         blue
      color = 5
!         dashed line type
      call ltype(2,1)
      xwert(1) = minlef
      xwert(2) = maxrig
      ywert(1) = am
      ywert(2) = am
!         moving scaling factors
      call rscale(xwert,ywert,mxk,2)
      ft = ft+1
      call malhapax(2, format, ft, mxk, xachse, yachse,&
      &nul, maxrig, maxtop, minbot, minlef,&
      &xwert, ywert,&
      &reke, zeit, datum, abszis, ordina,&
      &name, titelo, titelu, prname,color,&
      &.true.,.true.)
    end if
!       rated (rpm)
    buf = pltmsgf(30, 64)
    j = len_trim(buf)
    write(pmsg,35) buf(1:j),spdn
    j = len_trim(pmsg)
!       blue
    call pen(5)
    call symbol (15.5_lrk,17.1_lrk,.25_lrk,pmsg(1:j),0,0._lrk,j)
  end if
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
35 format(a,f7.1)
!
end subroutine csmp
!
!     ==================================================================
!>    @brief prepare plot geometry.
!
!>    @param[in] idx element index
!>    @param[in] idk element kind index
!>     1=section,2=division,3=disk,4=lateral bearing,5=torsion restricti
!>     6=unbalance excitation,7=static force,8=static torque,9=transitor
!>     10=harmonic torque,11=frequency response excit,12=frequency respo
!>     13=concentrated,0=line,14=time response
!>    @param[in] cnt element nodes count if lt zero do not close,
!>    see egeom
!>    @param[in] dm dimension of node geometry vector
!>    @param[in] xc element x coordinates
!>    @param[in] yc element y coordinates

subroutine pgeom(idx,idk,cnt,dm,xc,yc)
  use rd_textfun, only: fomsgf
  use com_exgeo, only: cc, ik, cx, cy, mxx, mnx, last
  use rd_kinds, only: lrk
  implicit none
!
  integer :: idx, idk, cnt, dm
  real(lrk) :: xc, yc
  dimension xc(dm),yc(dm)
!
  real(lrk) :: mm
!     convert to millimeter
  parameter (mm = 1000._lrk)
!
  integer :: i
  character(len=5) :: nmm, clb
  parameter(nmm = 'pgeom',clb = 'cx,cy')
!     error messages
  character(len=99) :: errmsg
!     geometry
  integer :: npgx, npix
  parameter(npgx = 999,npix = 9)
!     index and order,1->2->3->4->1
!     number of elements
!
  intrinsic :: abs
!
!     check dimension and count for bounds
  if (dm .gt. npix .or. abs(cnt) .gt. npix) then
    errmsg = fomsgf(99, nmm,5,clb,0)
    call lmsg(1,errmsg)
  end if
!
!     check element index for bound
  if (idx .gt. npgx) then
    errmsg = fomsgf(99, nmm,5,clb,0)
    call lmsg(1,errmsg)
  end if

!     millimeters
  if (idx .eq. 1) then
    mxx = xc(1)*mm
    mnx = mxx
  end if
!     coordinates count
  cc(idx) = cnt
!     element kind
  ik(idx) = idk
!     millimeters
!     added < 0 for not close geometry - francisco - feb-19
  do i = 1,abs(cnt)
    cx(idx,i) = xc(i)*mm
    cy(idx,i) = yc(i)*mm
!       max x coordinate, shaft length
    if (xc(i)*mm .ge. mxx) mxx = xc(i)*mm
    if (xc(i)*mm .lt. mnx) mnx = xc(i)*mm
  end do
!     store last index
  last = idx
!
  return
!
end subroutine pgeom
!
!     ==================================================================
!>    @brief plot geometry.
!
!>    @param[in] cm center of mass
!>    @param[in] stdio flag for standard input and output
!
subroutine egeom(cm,stdio)
  use rd_textfun, only: fomsgf, pltmsgf
  use com_exgeo, only: cc, ik, cx, cy, mxx, mnx, last
  use com_opt, only: std
  use rd_kinds, only: lrk
  implicit none
!
  real(lrk) :: cm
  logical :: stdio
!
  real(lrk) :: mv, cs, st
!     center of mass size
  integer :: i, j, k
  character(len=1) :: ddot
  character(len=3) :: c3
  character(len=4) :: pmm, c4
  character(len=5) :: nmm
  character(len=40) :: ccm
!     plot messages
  character(len=30) :: buf
!     error messages
  character(len=99) :: errmsg
!     output section name
  character(len=10) :: sec
!     output file name, used for non standard output
  character(len=255) :: pfn
!     malhap stuff
  integer :: ft, format, xachse, yachse, color, mxw
  parameter (mxw = 20,cs = 75,st = 1000,&
  &pmm = 'geom',nmm = 'egeom',&
  &c3 = 'mxw',c4='.plt',ddot = ':')
  real(lrk) :: nul, maxrig, maxtop, minbot, minlef, xwert, ywert
  dimension xwert(mxw+2),ywert(mxw+2)
  character(len=70) :: reke
  character(len=8) :: zeit
  character(len=10) :: datum
  character(len=30) :: name, abszis, ordina
  character(len=31) :: titelo, titelu
  character(len=8) :: prname
!     std plot output
!     geometry
  integer :: npgx, npix
  parameter(npgx = 999,npix = 9)
!     index and order,1->2->3->4->1
!     number of elements
!
  intrinsic :: abs, len_trim
!
!     standard io
  std = stdio
!
  if (.not. std) then
!       output file name
    call mntfnm(-7,i,pfn)
  end if
!
!     malhap settings
  format = 5
  xachse = 1
  yachse = 1
  nul = 1E-12_lrk
!     titles
  call header(reke,zeit,datum,name,titelu)
  abszis = pltmsgf(30, 61)
  ordina = pltmsgf(30, 21)
  titelo = pltmsgf(30, 22)
  prname = pmm
!     standard plot marker
  if (stdio) then
    write(sec,5)pmm,c4
    call marksec(sec,0)
  end if
!     maximum and minimum
  maxrig = mxx
  minlef = mnx
  maxtop = mxx/2
  minbot = -mxx/2
!
  color = 5
!     initialize hpgl plot
  call plots(0,0,pfn)
!
  do i = 1,last
    k = abs(cc(i))
!       added check max number - francisco - feb-19
    if (k .gt. mxw) then
      errmsg = fomsgf(99, nmm,4,c3,0)
      call lmsg(1,errmsg)
    end if
!
    do j = 1,k
      xwert(j) = cx(i,j)
      ywert(j) = cy(i,j)
    end do
!       added negative cout to not close - francisco - feb-19
    if (.not. cc(i) .lt. 0) then
!         "close" geometry
      k = k+1
      xwert(k) = cx(i,1)
      ywert(k) = cy(i,1)
    end if
!       if number of point not the same, adjust
    if (i .gt. 1) call rscale(xwert,ywert,mxw,k)
    ft = i
!       changed to accomodate "k" number of points
!       plot k points
    call malhap(k, format, ft, mxw, xachse, yachse,&
    &nul, maxrig, maxtop, minbot, minlef,&
    &xwert, ywert,&
    &reke, zeit, datum, abszis, ordina,&
    &name, titelo, titelu, prname,color)
!
  end do
!     center of mass * 1000. = mm
  mv = cm*st
  call pen(2)
  call mark(mv,0._lrk,cs,0)
  call pen(1)
  buf = pltmsgf(30, 23)
  j = len_trim(buf)
  write(ccm,15)buf(1:j),ddot,mv
  k = len_trim(ccm)
  call symbol (1.5_lrk,17._lrk,.28_lrk,ccm(1:k),0,0._lrk,k)
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
15 format(2a,f7.2)
!
end subroutine egeom
!
!     ==================================================================
!>    @brief plot 3d coordinate axis, main argumens to call malhap.
!
!>    @param[in] un coordinate axis length
!>    @param[in] ss 3d -> 2d coordinate transformation scale
!>    @param[in] npo number of x and y plot points vector dimension
!>    @param[in] c 3d world settings matrix
!>    @param[in] v 3d view settings matrix
!>    @param[in] format page size for malhap (format = 4 --> din a4)
!>    @param[in] ft page count for malhap (ft = 1 --> new page)
!>    @param[in] xachse plot x axis
!>    @param[in] yachse plot y axis
!>    @param[in] nul minimum value non zero
!>    @param[in] maxrig maximum values at right
!>    @param[in] maxtop maximum values at top
!>    @param[in] minbot minimum values at bottom
!>    @param[in] minlef minimum values at left
!>    @param[in] reke calculation info
!>    @param[in] zeit time stamp
!>    @param[in] datum date stamp
!>    @param[in] abszis x axis tag
!>    @param[in] ordina y axis tag
!>    @param[in] name user name
!>    @param[in] titelo main title
!>    @param[in] titelu sub title
!>    @param[in] prname program name stamp
!
subroutine axis3d(un,ss,npo,c,v,format, ft, xachse, yachse,&
&nul, maxrig, maxtop, minbot, minlef,&
&reke, zeit, datum, abszis, ordina,&
&name, titelo, titelu, prname)
  use rd_kinds, only: lrk
  implicit none
!
  real(lrk) :: un, ss, c, v
  integer :: npo
!     malhap stuff
  integer :: ft, format, xachse, yachse, color
  real(lrk) :: nul, maxrig, maxtop, minbot, minlef
  character(len=8) :: zeit, prname
  character(len=10) :: datum
  character(len=30) :: name, abszis, ordina
  character(len=31) :: titelo, titelu
  character(len=70) :: reke
!     local
  character :: an
  real(lrk) :: xp, yp, zp, ox, oy, oz, x, y, of, cs, xwert, ywert
  dimension c(4,4),v(4,4),xwert(npo+2),ywert(npo+2),an(3)
  parameter(an = (/'x','y','z'/))
!
  data of/1.05_lrk/,cs/0.34_lrk/
!
!     3d axis
  color = 1
!     reset scale position
  call rscale(xwert,ywert,npo,2)
!     3d coordinates axis icon origin
  ox = -2.5_lrk*un
  oy = 3.5_lrk*un
  oz = -2.0_lrk*un
!
!     x axis
  yp = oy
  xp = ox
  zp = oz
  call get2d(c,v,xp,zp,yp,0._lrk,0._lrk,ss,x,y)
  xwert(1) = x
  ywert(1) = y
  yp = oy
  xp = ox+un
  zp = oz
  call get2d(c,v,xp,zp,yp,0._lrk,0._lrk,ss,x,y)
  xwert(2) = x
  ywert(2) = y
  call malhap(2, format, ft, npo, xachse, yachse,&
  &nul, maxrig, maxtop, minbot, minlef,&
  &xwert, ywert,&
  &reke, zeit, datum, abszis, ordina,&
  &name, titelo, titelu, prname,color)
  call get2d(c,v,xp*of,zp*of,yp*of,0._lrk,0._lrk,ss,x,y)
  call csymbol(x, y, cs, an(1), 0, -90._lrk, 1)
  ft = ft+1
!
!     y axis
  yp = oy+un
  xp = ox
  zp = oz
  call get2d(c,v,xp,zp,yp,0._lrk,0._lrk,ss,x,y)
  xwert(2) = x
  ywert(2) = y
  call malhap(2, format, ft, npo, xachse, yachse,&
  &nul, maxrig, maxtop, minbot, minlef,&
  &xwert, ywert,&
  &reke, zeit, datum, abszis, ordina,&
  &name, titelo, titelu, prname,color)
  call get2d(c,v,xp*of,zp*of,yp*of,0._lrk,0._lrk,ss,x,y)
  call csymbol(x, y, cs, an(2), 0, -90._lrk, 1)
  ft = ft + 1
!
!     z axis
  yp = oy
  xp = ox
  zp = oz+un
  call get2d(c,v,xp,zp,yp,0._lrk,0._lrk,ss,x,y)
  xwert(2) = x
  ywert(2) = y
  call malhap(2, format, ft, npo, xachse, yachse,&
  &nul, maxrig, maxtop, minbot, minlef,&
  &xwert, ywert,&
  &reke, zeit, datum, abszis, ordina,&
  &name, titelo, titelu, prname,color)
  call get2d(c,v,xp*of,zp*of,yp*of,0._lrk,0._lrk,ss,x,y)
  call csymbol(x, y, cs, an(3), 0, -90._lrk, 1)
!
  return
!
end subroutine axis3d
!
!     ==================================================================
!>    @brief plot mode shapes.
!
!>    @param[in] rpm speed for bearing dependent paramenters
!>    @param[in] fmd whril frequencies (Hz)
!>    @param[in] py section axial coordinates (m)
!>    @param[in] orx orbit x points (m)
!>    @param[in] orz orbit z points (m)
!>    @param[in] npv number of shaft sections
!>    @param[in] npo number of orbit points
!>    @param[in] qtm number of modes
!>    @param[in] xmd frequency and orbit dimension
!>    @param[in] mts position and orbit dimension
!>    @param[in] stdio flag for standard input and output
!>    @param[in] di whril direction
!>    @param[in] pfn output file name, used for non standard output
!
subroutine mshp(rpm,fmd,py,orx,orz,npv,npo,qtm,&
&xmd,mts,stdio,di,pfn)
  use rd_textfun, only: pltmsgf
  use com_mdd2, only: imd, nsm
  use com_opt, only: std
  use rd_kinds, only: lrk
  implicit none
!     arguments
  integer :: npv, npo, qtm, xmd, mts
  real(lrk) :: rpm, fmd, py, orx, orz
  character(len=2) :: di
  character(len=255) :: pfn
  dimension orx(xmd,mts,npo),orz(xmd,mts,npo),&
  &fmd(xmd),py(mts),di(xmd)
  logical :: stdio
!     locals
  integer :: i, im, j, l, n, nm
  real(lrk) :: c, v, mg, mx, my, nx, ny, xp, yp, zp, x, y, ss, sc, ys, un, as
  character(len=1) :: blank, ddot
  character(len=4) :: pmm, c4
  character(len=5) :: c5
  parameter (blank = ' ',ddot = ':',pmm = 'mshp',c4 = '.plt',&
  &c5 = '(Hz):')
  character(len=10) :: sec
  character(len=30) :: frq, ptxt
  character(len=50) :: txt
!     malhap stuff, mts MUST be greater than npo
  integer :: ft, format, xachse, yachse, color
  real(lrk) :: nul, maxrig, maxtop, minbot, minlef, xwert, ywert
  dimension im(xmd),c(4,4),v(4,4),xwert(npo+2),ywert(npo+2)
  character(len=8) :: zeit, prname
  character(len=10) :: datum
  character(len=30) :: name, abszis, ordina
  character(len=31) :: titelo, titelu
  character(len=70) :: reke
!
!     modes to show
!     max number of modes,search for commons
!     nmx = xmd
  integer :: nmx
  parameter (nmx = 19)
!
!     std plot output
!     3d scalings
  data ss/1000._lrk/,sc/500.0_lrk/,as/0.05_lrk/,ys/12._lrk/,mx/0/,nx/0/,my/0/,ny/0/
!
  intrinsic :: abs, len_trim
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
!     standard io
  std = stdio
!     set 3d world
  call wset(0._lrk,0._lrk,0._lrk,0._lrk,0._lrk,0._lrk,1._lrk,c)
!     set 3d camera view
  call cset(-0.1_lrk,-0.75_lrk,0.1_lrk,2._lrk,-3._lrk,-3._lrk,1.0_lrk,v)
!     length of the 3d coordinate axis
  un = (py(npv)-py(1))*as
!     malhap settings
  format = 5
  xachse = 0
  yachse = 0
  nul = 1E-12_lrk
!     titles
  call header(reke,zeit,datum,name,titelu)
  abszis = blank
  ordina = blank
  prname = pmm
!     standard plot marker
  if (stdio) then
    write(sec,5)pmm,c4
    call marksec(sec,0)
  end if
!     initialize hpgl plot
  call plots(0,0,pfn)
!     loop on the modes
  do n = 1,nm
!       each mode shape
    i = im(n)
!       search for the xp,zp maximum
    mg = 1
    do j = 1,npv
      do l = 1,npo
        xp = orx(i,j,l)
        zp = orz(i,j,l)
        if (j .eq. 1 .and. l .eq. 1 .and.&
        &(abs(xp) .gt. nul .or. abs(zp) .gt. nul)) then
          mg = xp
          if (zp .gt. mg) mg = zp
        else
          if (xp .gt. mg) mg = xp
          if (zp .gt. mg) mg = zp
        end if
      end do
    end do
!       recalcultate 3d scale.
!       shaft length / maximum displacement.
    sc = py(npv)/mg/ys
!       search for the x,y limits
    do j = 1,npv
      yp = py(j)
      do l = 1,npo
        xp = orx(i,j,l)
        zp = orz(i,j,l)
        call get2d(c,v,xp*sc,zp*sc,yp,0._lrk,0._lrk,ss,x,y)
        if (j .eq. 1 .and. l .eq. 1) then
          mx = x
          nx = x
          my = y
          ny = y
        else
          if (x .gt. mx) mx=x
          if (x .lt. nx) nx=x
          if (y .gt. my) my=y
          if (y .lt. ny) ny=y
        end if
      end do
    end do
!       maximum and minimum
    maxrig = mx
    minlef = nx
    maxtop = my
    minbot = ny
!       plot page
    ft = 1
!       reset draw scales
    call sreset(3)
    color = 5
!       mode number
    ptxt = pltmsgf(30, 24)
    j = len_trim(ptxt)
    write(titelo,15)ptxt(1:j),ddot,i
!       orbit paths
    do j = 1,npv
      yp = py(j)
      do l = 1,npo
        xp = orx(i,j,l)
        zp = orz(i,j,l)
        call get2d(c,v,xp*sc,zp*sc,yp,0._lrk,0._lrk,ss,x,y)
        xwert(l) = x
        ywert(l) = y
      end do
      call malhap(npo, format, ft, npo, xachse, yachse,&
      &nul, maxrig, maxtop, minbot, minlef,&
      &xwert, ywert,&
      &reke, zeit, datum, abszis, ordina,&
      &name, titelo, titelu, prname,color)
      ft = ft+1
    end do
    color = 1
!       frequency and whril direction
    write(frq,25) fmd(i),c5,di(i)
    call symbol (0.2_lrk, 17.2_lrk, .3_lrk, frq, 0, 0._lrk, 20)
!       mode speed rpm
    if (.not. rpm .lt. 0) then
      ptxt = pltmsgf(30, 1)
      j = len_trim(ptxt)
      write(frq,35) ptxt(1:j),rpm
      call symbol (0.2_lrk, 0.2_lrk, .3_lrk, frq, 0, 0._lrk,30)
    end if
!       shaft center line
    do j = 1,npv
      xp = 0._lrk
      yp = py(j)
      zp = 0._lrk
      call get2d(c,v,xp*sc,zp*sc,yp,0._lrk,0._lrk,ss,x,y)
      xwert(j) = x
      ywert(j) = y
    end do
    call rscale(xwert,ywert,npo,npv)
    call malhap(npv, format, ft, npo, xachse, yachse,&
    &nul, maxrig, maxtop, minbot, minlef,&
    &xwert, ywert,&
    &reke, zeit, datum, abszis, ordina,&
    &name, titelo, titelu, prname,color)
    ft = ft+1
!       torsion
    color = 2
    call pen(2)
    do j = 1,npv
      xp = orx(i,j,1)
      zp = orz(i,j,1)
      yp = py(j)
      call get2d(c,v,xp*sc,zp*sc,yp,0._lrk,0._lrk,ss,x,y)
      xwert(j) = x
      ywert(j) = y
      call circle(x,y,15._lrk)
    end do
    call malhap(npv, format, ft, npo, xachse, yachse,&
    &nul, maxrig, maxtop, minbot, minlef,&
    &xwert, ywert,&
    &reke, zeit, datum, abszis, ordina,&
    &name, titelo, titelu, prname,color)
    ft = ft+1
!       for each orbit shape line 0.,0.,y -> x(1),z(1),y
    color = 5
    do j = 1,npv
      yp = py(j)
      xp = 0._lrk
      zp = 0._lrk
      call get2d(c,v,xp*sc,zp*sc,yp,0._lrk,0._lrk,ss,x,y)
      xwert(1) = x
      ywert(1) = y
      xp = orx(i,j,1)
      zp = orz(i,j,1)
      call get2d(c,v,xp*sc,zp*sc,yp,0._lrk,0._lrk,ss,x,y)
      xwert(2) = x
      ywert(2) = y
      if (j .eq. 1) call rscale(xwert,ywert,npo,2)
      call malhap(2, format, ft, npv, xachse, yachse,&
      &nul, maxrig, maxtop, minbot, minlef,&
      &xwert, ywert,&
      &reke, zeit, datum, abszis, ordina,&
      &name, titelo, titelu, prname,color)
      ft = ft+1
    end do
!       3d coordinate axis
    call axis3d(un,ss,npo,c,v,format, ft, xachse, yachse,&
    &nul, maxrig, maxtop, minbot, minlef,&
    &reke, zeit, datum, abszis, ordina,&
    &name, titelo, titelu, prname)
!
!       eventual speed extrapolation message, see parmanv.f
    call eplwrg(-1,txt)
    j = len_trim(txt)
    if (j .gt. 0) call symbol (9.0_lrk, 18.0_lrk, 0.2_lrk, txt(1:j), 0, 0._lrk, j)
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
15 format(2a,i2)
25 format(f6.1,2a)
35 format(a,f6.1)
!
end subroutine mshp
!
!     ==================================================================
!>    @brief plot time response orbit shape for a given single speed.
!
!>    @param[in] rpg speed (rpm)
!>    @param[in] py orbit section position vector (m)
!>    @param[in] u orbit x points (m)
!>    @param[in] w orbit z points (m)
!>    @param[in] a1 angle of maximum displacment value (rad)
!>    @param[in] d1 maximum displacment value (m)
!>    @param[in] np number of section points, py
!>    @param[in] npo number of orbit points, orbit dimension
!>    @param[in] nmd number of considered modes
!>    @param[in] mxp orbit dimension
!>    @param[in] mte orbit matrix dimension
!>    @param[in] stdio flag for standard input and output
!>    @param[in] di whril direction
!>    @param[in] pfn output file name, used for non standard output
!
subroutine tmsor(rpg,py,u,w,a1,d1,np,&
&npo,nmd,mxp,mte,stdio,di,pfn)
  use rd_textfun, only: pltmsgf
  use com_opt, only: std
  use rd_kinds, only: lrk
  implicit none
!
  integer :: np, npo, nmd, mxp, mte
  real(lrk) :: rpg, py, u, w, a1, d1
  character(len=2) :: di
  dimension py(mxp),u(mxp,mte,npo),w(mxp,mte,npo),&
  &di(mxp,mte),a1(np),d1(np)
  character(len=255) :: pfn
  logical :: stdio
!
!     local
  character(len=1) :: ddot
  character(len=4) :: pmm, c4
  parameter (ddot = ':',pmm = 'tsor',c4 = '.plt')
  integer :: i, j, ls
  real(lrk) :: mx, mn, yp, ad, todegpf
!     output section name
  character(len=10) :: sec
!     plot messages
  character(len=30) :: txt
  character(len=55) :: out
!     malhap stuff, mte MUST be greater than npo
  integer :: ft, format, xachse, yachse, color
  real(lrk) :: nul, maxrig, maxtop, minbot, minlef
  real(lrk) :: xwert, ywert
  dimension xwert(npo+2),ywert(npo+2),txt(3),ls(3)
  character(len=8) :: zeit, prname
  character(len=10) :: datum
  character(len=30) :: name, abszis, ordina
  character(len=31) :: titelo, titelu
  character(len=70) :: reke
!     std plot output
!
  intrinsic :: len_trim
!
!     standard io
  std = stdio
!     malhap settings
  format = 5
  xachse = 1
  yachse = 1
  nul = 1E-12_lrk
  color = 5
!     titles
  call header(reke,zeit,datum,name,titelu)
  abszis = pltmsgf(30, 26)
  ordina = pltmsgf(30, 27)
  prname = pmm
  titelo = pltmsgf(30, 25)
!     standard plot marker
  if (stdio) then
    write(sec,5) pmm,c4
    call marksec(sec,0)
  end if
!     initialize hpgl plot
  call plots(0,0,pfn)
!     plot page
  ft = 1
!     sections vector
  do i = 1,np
!       max values in 10-6 m
    do j = 1,npo
      xwert(j) = u(i,1,j)
      ywert(j) = w(i,1,j)
      if (j .eq. 1) then
        mx = xwert(j)
        mn = xwert(j)
        if (ywert(j) .gt. xwert(j)) mx = ywert(j)
        if (ywert(j) .lt. xwert(j)) mn = ywert(j)
      else
        if(xwert(j) .gt. mx .or. ywert(j) .gt. mx) then
          mx = xwert(j)
          if (ywert(j) .gt. xwert(j)) mx = ywert(j)
        end if
        if(xwert(j) .lt. mn .or. ywert(j) .lt. mn) then
          mn = xwert(j)
          if (ywert(j) .lt. xwert(j)) mn = ywert(j)
        end if
      end if
    end do
!       maximum and minimum
    maxrig = mx
    minlef = mn
    maxtop = mx
    minbot = mn
!       reset draw scales
    call sreset(3)
    call malhap(npo, format, ft, npo, xachse, yachse,&
    &nul, maxrig, maxtop, minbot, minlef,&
    &xwert, ywert,&
    &reke, zeit, datum, abszis, ordina,&
    &name, titelo, titelu, prname,color)
!       position, speed and whril direction
!       y (m)'
    txt(1) = pltmsgf(30, 46)
!       max. (m)'
    txt(2) = pltmsgf(30, 63)
!       angle (deg)
    txt(3) = pltmsgf(30, 9)
    do j =1,3
      ls(j)= len_trim(txt(j))
    end do
!       maximal angle degree
    ad = todegpf(a1(i),1)
!       first quadrant
    if (ad .gt. 180) ad = ad-180
    write(out,15)&
    &txt(1)(1:ls(1)),ddot,py(i),ddot,di(1,i),ddot,&
    &txt(2)(1:ls(2)),ddot,d1(i),ddot,txt(3)(1:ls(3)),ddot,ad
    j = len_trim(out)
    call pen(1)
    yp = 17.5_lrk-(i-1)*0.25_lrk
    call symbol (0.17_lrk,yp,0.2_lrk,out(1:j),0,0._lrk,j)
    ft = ft+1
!
  end do
!     speed (rpm)
  txt(1) = pltmsgf(30, 18)
!     modes
  txt(2) = pltmsgf(30, 62)
  do j =1,2
    ls(j)= len_trim(txt(j))
  end do
!     speed and modes
  write(out,25)&
  &txt(1)(1:ls(1)),ddot,rpg,ddot,&
  &txt(2)(1:ls(2)),ddot,nmd
  j = len_trim(out)
  call symbol (14.9_lrk,17.5_lrk,0.2_lrk,out(1:j),0,0._lrk,j)
!     eventual speed extrapolation message, see parmanv.f
  call eplwrg(-1,out)
  j = len_trim(out)
  if (j .gt. 0) call symbol (1.0_lrk, 0.3_lrk, 0.2_lrk, out(1:j), 0, 0._lrk, j)
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
15 format(2a,f7.2,5a,e10.4,3a,f5.1)
25 format(2a,f6.0,3a,i3)
!
end subroutine tmsor
!
!     ==================================================================
!>    @brief plot orbit shapes for a given single speed, speed shape.
!
!>    @param[in] rpg speed (rpm)
!>    @param[in] py orbit section position vector (m)
!>    @param[in] u orbit x points (m)
!>    @param[in] w orbit z points (m)
!>    @param[in] np number of section points, py
!>    @param[in] npo number of orbit points, orbit dimension
!>    @param[in] nmd number of considered modes
!>    @param[in] mxp orbit dimension
!>    @param[in] mte orbit matrix dimension
!>    @param[in] stdio flag for standard input and output
!>    @param[in] pfn output file name, used for non standard output
!
subroutine tmssh(rpg,py,u,w,np,npo,nmd,mxp,mte,stdio,pfn)
  use rd_textfun, only: cadjf, pltmsgf
  use com_opt, only: std
  use rd_kinds, only: lrk
  implicit none
!
  integer :: np, npo, nmd, mxp, mte
  real(lrk) :: rpg, py, u, w
  dimension py(mxp),u(mxp,mte,npo),w(mxp,mte,npo)
  character(len=255) :: pfn
  logical :: stdio
!     local
  integer :: i, j
  real(lrk) :: mg, mx, my, nx, ny, xp, yp, zp, x, y, as, ss, sc, ys, un, c, v
  character(len=1) :: blank
  character(len=3) :: c3
  character(len=4) :: pmm, c4
  !     output section name
  character(len=10) :: sec
!     plot messages
  character(len=30) :: out, txt
  character(len=50) :: msg
  parameter (blank = ' ',c3 = ' = ',pmm = 'tssh',&
  &c4 = '.plt')
!
!     malhap stuff, mte MUST be greater than npo
  integer :: ft, format, xachse, yachse, color
  real(lrk) :: nul, maxrig, maxtop, minbot, minlef, xwert, ywert
  dimension c(4,4),v(4,4),xwert(mxp+2),ywert(mxp+2)
  character(len=8) :: zeit, prname
  character(len=10) :: datum
  character(len=30) :: name, abszis, ordina
  character(len=31) :: titelo, titelu
  character(len=70) :: reke
!     std plot output
!     3d scaling
  data ss/1000._lrk/,as/0.05_lrk/,ys/10._lrk/,mx/0/,my/0/,nx/0/,ny/0/
!
  intrinsic :: abs, len_trim
!
!     standard io
  std = stdio
!     set 3d world
  call wset(0._lrk,0._lrk,0._lrk,0._lrk,0._lrk,0._lrk,1._lrk,c)
!     set 3d camera view
  call cset(-0.1_lrk,-0.75_lrk,0.1_lrk,2._lrk,-3._lrk,-3._lrk,1.0_lrk,v)
!     malhap settings
  format = 5
  xachse = 0
  yachse = 0
  nul = 1E-12_lrk
  color = 5
!     titles
  call header(reke,zeit,datum,name,titelu)
  abszis = blank
  ordina = blank
  prname = pmm
  titelo = pltmsgf(30, 28)
!     standard plot marker
  if (stdio) then
    write(sec,5) pmm,c4
    call marksec(sec,0)
  end if
!     length of the 3d coordinate axis
  un = (py(np)-py(1))*as
!     maximum and minimum
  mg = 1
  do i = 1,np
!       number of orbit points each section
    do j = 1,npo
      xp = u(i,1,j)
      zp = w(i,1,j)
      if (i .eq. 1 .and. j .eq. 1 .and.&
      &(abs(xp) .gt. nul .or. abs(zp) .gt. nul)) then
        mg = xp
        if (zp .gt. mg) mg = zp
      else
        if(xp .gt. mg) mg = xp
        if(zp .gt. mg) mg = zp
      end if
    end do
  end do
!     recalcultate 3d scale, displacement is speed dependent.
!     shaft length / maximum displacement.
  sc = py(np)/mg/ys
!     number of sections
  do i=1,np
    yp = py(i)
!       number of orbit points each section
    do j = 1,npo
      xp = u(i,1,j)
      zp = w(i,1,j)
      call get2d(c,v,xp*sc,zp*sc,yp,0._lrk,0._lrk,ss,x,y)
      if (i .eq. 1 .and. j .eq. 1) then
        mx = x
        my = y
        nx = x
        ny = y
      else
        if(x .gt. mx) mx = x
        if(y .gt. my) my = y
        if(x .lt. nx) nx = x
        if(y .lt. ny) ny = y
      end if
    end do
  end do
!     maximum and minimum
  maxrig = mx
  minlef = nx
  maxtop = my
  minbot = ny
!     initialize hpgl plot
  call plots(0,0,pfn)
!     plot page
  ft = 1
!     sections vector
  do i=1,np
!       section
    yp = py(i)
    do j = 1,npo
      xp = u(i,1,j)
      zp = w(i,1,j)
      call get2d(c,v,xp*sc,zp*sc,yp,0._lrk,0._lrk,ss,x,y)
      xwert(j) = x
      ywert(j) = y
    end do
!       orbit points
    call malhap(npo, format, ft, mxp, xachse, yachse,&
    &nul, maxrig, maxtop, minbot, minlef,&
    &xwert, ywert,&
    &reke, zeit, datum, abszis, ordina,&
    &name, titelo, titelu, prname,color)
    ft = ft+1
  end do
!     shaft center line
  color = 1
  do j = 1,np
    xp = 0._lrk
    yp = py(j)
    zp = 0._lrk
    call get2d(c,v,xp*sc,zp*sc,yp,0._lrk,0._lrk,ss,x,y)
    xwert(j)=x
    ywert(j)=y
  end do
  call rscale(xwert,ywert,mxp,np)
  call malhap(np, format, ft, mxp, xachse, yachse,&
  &nul, maxrig, maxtop, minbot, minlef,&
  &xwert, ywert,&
  &reke, zeit, datum, abszis, ordina,&
  &name, titelo, titelu, prname,color)
  ft = ft+1
!     torsion
  color = 2
  call pen(2)
  do i=1,np
    xp = u(i,1,1)
    zp = w(i,1,1)
    yp = py(i)
    call get2d(c,v,xp*sc,zp*sc,yp,0._lrk,0._lrk,ss,x,y)
    xwert(i)=x
    ywert(i)=y
    call circle(x,y,15._lrk)
  end do
  call malhap(np, format, ft,mxp, xachse, yachse,&
  &nul, maxrig, maxtop, minbot, minlef,&
  &xwert, ywert,&
  &reke, zeit, datum, abszis, ordina,&
  &name, titelo, titelu, prname,color)
  ft = ft+1
!     for each orbit shape line 0.,0.,y -> x(1),z(1),y
  color = 5
  do i=1,np
    yp = py(i)
    xp = 0._lrk
    zp = 0._lrk
    call get2d(c,v,xp*sc,zp*sc,yp,0._lrk,0._lrk,ss,x,y)
    xwert(1)=x
    ywert(1)=y
    xp = u(i,1,1)
    zp = w(i,1,1)
    call get2d(c,v,xp*sc,zp*sc,yp,0._lrk,0._lrk,ss,x,y)
    xwert(2) = x
    ywert(2) = y
    if (i.eq.1) call rscale(xwert,ywert,mxp,2)
    call malhap(2, format, ft, mxp, xachse, yachse,&
    &nul, maxrig, maxtop, minbot, minlef,&
    &xwert, ywert,&
    &reke, zeit, datum, abszis, ordina,&
    &name, titelo, titelu, prname,color)
    ft = ft+1
  end do
!     3d coordinate axis
  call axis3d(un,ss,mxp,c,v,format, ft, xachse, yachse,&
  &nul, maxrig, maxtop, minbot, minlef,&
  &reke, zeit, datum, abszis, ordina,&
  &name, titelo, titelu, prname)
!
!     speed, rpm
  out = pltmsgf(30, 1)
!     remove blank at start
  out = cadjf(30, out,len(out))
!     size
  j = len_trim(out)
  write(out,15) out(1:j),c3,rpg
  j = len_trim(out)
  call pen(1)
  call symbol (0.2_lrk, 17.2_lrk, .3_lrk, out(1:j), 0, 0._lrk, j)
!     number of modes
  txt = pltmsgf(30, 62)
  j = len_trim(txt)
  write(out,25) txt(1:j),c3,nmd
  j = len_trim(out)
  call symbol (15.2_lrk, 17.2_lrk, .3_lrk, out(1:j), 0, 0._lrk, j)
!
!     eventual speed extrapolation message, see parmanv.f
  call eplwrg(-1,msg)
  j = len_trim(msg)
  if (j .gt. 0) call symbol (1.0_lrk, 0.0_lrk, 0.2_lrk, msg(1:j), 0, 0._lrk, j)
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
15 format(2a,f6.0)
25 format(2a,i3)
!
!
end subroutine tmssh
!
!     ==================================================================
!>    @brief plot orbit shapes for a given speed range and section.
!
!>    @param[in] rang amplitude output angle (degree)
!>    @param[in] rpg speed vector (rpm)
!>    @param[in] rp  displacement speed vector (rpm)
!>    @param[in] py orbit section position vector (m)
!>    @param[in] u orbit x points (m)
!>    @param[in] w orbit z points (m)
!>    @param[in] value amplitude output (m)
!>    @param[in] ns number of speed points
!>    @param[in] nk number of displacement speed points
!>    @param[in] np number of output points
!>    @param[in] npo number of orbit points, orbit dimension
!>    @param[in] nmd number of considered modes
!>    @param[in] mxp orbit position dimension (mts)
!>    @param[in] mte orbit matrix dimension
!>    @param[in] stdio flag for standard input and output
!>    @param[in] di whril direction
!>    @param[in] pfn output file name, used for non standard output
!
subroutine tmors(rang,rpg,rp,py,u,w,value,&
&ns,nk,np,npo,nmd,mxp,mte,stdio,di,pfn)
  use rd_textfun, only: pltmsgf
  use com_opt, only: std
  use rd_kinds, only: lrk
  implicit none
!
  integer :: ns, nk, np, npo, nmd, mxp, mte
  real(lrk) :: rang, rpg, rp, py, value, u, w
  character(len=2) :: di
  character(len=255) :: pfn
  logical :: stdio
  dimension rpg(mte),rp(np,ns),py(mxp),value(np,ns),&
  &u(mxp,mte,npo),w(mxp,mte,npo),di(mxp,mte)
!     local
  character(len=1) :: blank, fw, ddot, sla, c1
  character(len=2) :: c2
  character(len=3) :: c3
  character(len=4) :: pmm, c4
  character(len=5) :: c5
!     fw = forward precession direction, see resp_t.f
  parameter (blank = ' ',fw = 'F',ddot = ':',sla = '-',&
  &c1 = '@',c2 = '(i',c3 = 'a,f',pmm = 'tspt',c4 = '.plt',&
  &c5 = '.0,a)')

  integer :: i, j, k, l
  real(lrk) :: gval
  real(lrk) :: al, lp, mg, mx, my, nx, ny, xp, yp, zp, x, y, as, ss, sc, sd, un, c, v
!     output section name
  character(len=10) :: sec
  character(len=15) :: fmt
!     plot messages
  character(len=30) :: txt
  character(len=50) :: out
!     malhap stuff, mte MUST be grater than npo
  integer :: ft, format, xachse, yachse, color
  real(lrk) :: nul, maxrig, maxtop, minbot, minlef, xwert, ywert
  character(len=8) :: zeit, prname
  character(len=10) :: datum
  character(len=30) :: name, abszis, ordina
  character(len=31) :: titelo, titelu
  character(len=70) :: reke
!
  dimension c(4,4),v(4,4),xwert(mxp+2),ywert(mxp+2)
!     std plot output
!
  data ss/1000._lrk/,as/0.05_lrk/,sd/0.8_lrk/,mx/0/,my/0/,nx/0/,ny/0/,lp/0/
!
  intrinsic :: abs, int, nint, len_trim, log10, real, sqrt
!
!     standard io
  std = stdio
!     set 3d world
  call wset(0._lrk,0._lrk,0._lrk,0._lrk,0._lrk,0._lrk,1._lrk,c)
!     set 3d camera view
  call cset(-0.1_lrk,-0.75_lrk,0.1_lrk,2._lrk,-3._lrk,-3._lrk,1.0_lrk,v)
!     malhap settings
  format = 5
  xachse = 0
  yachse = 0
  nul = 1E-12_lrk
!     titles
  call header(reke,zeit,datum,name,titelu)
  abszis = blank
  ordina = blank
  prname = pmm
!     'Time Displacement Shape' !29
  titelo = pltmsgf(30, 29)
!     standard plot marker
  if (stdio) then
    write(sec,5) pmm,c4
    call marksec(sec,0)
  end if
!     length of the 3d coordinate axis
  al = 0.01_lrk
  un = al*as
!     number of speeds
  mg = 1
  do k = 1,ns
!       number of output points
    do i = 1,np
!         number of orbit points each section
      do j = 1,npo
        xp = u(i,k,j)
        zp = w(i,k,j)
!           maximum and minimum
        if (i .eq. 1 .and. j .eq. 1 .and. k .eq. 1 .and.&
        &(abs(xp) .gt. nul .or. abs(zp) .gt. nul)) then
          mg = xp
          if (zp .gt. mg) mg = zp
        else
          if(xp .gt. mg) mg = xp
          if(zp .gt. mg) mg = zp
        end if
      end do
    end do
  end do
!     recalcultate 3d scale, displacement is speed dependent.
!     shaft length / maximum displacement.
  sc = al/mg/8._lrk
  do k = 1,ns
    yp = al*(k-1)/(ns-1)
!       number of output points
    do i = 1,np
      do j = 1,npo
        xp = u(i,k,j)
        zp = w(i,k,j)
        call get2d(c,v,xp*sc,zp*sc,yp,0._lrk,0._lrk,ss,x,y)
        if (i .eq. 1 .and. j .eq. 1 .and. k .eq. 1) then
          mx = x
          my = y
          nx = x
          ny = y
        else
          if(x .gt. mx) mx = x
          if(y .gt. my) my = y
          if(x .lt. nx) nx = x
          if(y .lt. ny) ny = y
        end if
      end do
    end do
  end do
!     maximum and minimum
  maxrig = mx
  minlef = nx
  maxtop = my
  minbot = ny
!     initialize hpgl plot
  call plots(0,0,pfn)
!     plot page
  ft = 1
!     number of speeds
  do k = 1,ns
!       number of sections
    yp = al*(k-1)/(ns-1)
!       number of output points
    do i = 1,np
!         section points
      do j = 1,npo
        xp = u(i,k,j)
        zp = w(i,k,j)
        call get2d(c,v,xp*sc,zp*sc,yp,0._lrk,0._lrk,ss,x,y)
        xwert(j) = x
        ywert(j) = y
      end do
!         precession direction
      if (di(i,k)(1:1) .eq. fw) then
        color = 5
      else
        color = 2
      end if
!         orbit points
      call malhap(npo, format, ft, mxp, xachse, yachse,&
      &nul, maxrig, maxtop, minbot, minlef,&
      &xwert, ywert,&
      &reke, zeit, datum, abszis, ordina,&
      &name, titelo, titelu, prname,color)
      ft = ft+1
!         sections
    end do
!       speeds
  end do
!
  color = 1
!     number of output points
  do i = 1,np
!       number of speeds
    do k = 1,ns
      yp = al*(k-1)/(ns-1)
!         zero center line
      xp = 0._lrk
      zp = 0._lrk
      call get2d(c,v,xp*sc,zp*sc,yp,0._lrk,0._lrk,ss,x,y)
!         first section point
      xwert(k) = x
      ywert(k) = y
      xp = u(i,k,1)
      zp = w(i,k,1)
      call get2d(c,v,xp*sc,zp*sc,yp,0._lrk,0._lrk,ss,x,y)
!         precession direction
      if (di(i,k)(1:1) .eq. fw) then
        call pen(5)
      else
        call pen(2)
      end if
      call circle(x,y,15._lrk)
    end do
!       zero axis (must call every time)
    call rscale(xwert,ywert,mxp,ns)
    call malhap(ns, format, ft, mxp, xachse, yachse,&
    &nul, maxrig, maxtop, minbot, minlef,&
    &xwert, ywert,&
    &reke, zeit, datum, abszis, ordina,&
    &name, titelo, titelu, prname,color)
    ft = ft+1
!       number of speeds
    do k = 1,ns
      yp = al*(k-1)/(ns-1)
!         line from center to first section point
      xp = 0._lrk
      zp = 0._lrk
      call get2d(c,v,xp*sc,zp*sc,yp,0._lrk,0._lrk,ss,x,y)
      xwert(1) = x
      ywert(1) = y
      xp = u(i,k,1)
      zp = w(i,k,1)
      call get2d(c,v,xp*sc,zp*sc,yp,0._lrk,0._lrk,ss,x,y)
      xwert(2) = x
      ywert(2) = y
      if (k .eq. 1) call rscale(xwert,ywert,mxp,2)
      call malhap(2, format, ft, mxp, xachse, yachse,&
      &nul, maxrig, maxtop, minbot, minlef,&
      &xwert, ywert,&
      &reke, zeit, datum, abszis, ordina,&
      &name, titelo, titelu, prname,color)
      ft = ft+1
!         output position based on maximum orbit value
      xp = 0._lrk*mg
      yp = yp-1.0_lrk*sc*mg
      zp = -1._lrk*mg
      call get2d(c,v,xp*sc,zp*sc,yp,0._lrk,0._lrk,ss,x,y)
!         absolute position for annotation output
      if (k .gt. 1 ) lp = sqrt((gval(x,1)-mx)**2 +&
      &(gval(y,2)-my)**2)
      if (k .eq. 1 .or. lp .gt. sd) then
!           is enough distance for annotation
!           index number of digits
        j = int(log10(real(k, lrk)+1e-9_lrk))+1
!           speed number of digits
        l = int(log10(rpg(k)))+2
!           write out the format
        write(fmt,15) c2,j,c3,l,c5
!           write whole output
        write(out,fmt) k,ddot,rpg(k),di(i,k)(1:1)
        j = len_trim(out)
        call csymbol (x, y, .22_lrk, out(1:j), 0, 0._lrk, j)
        mx = gval(x,1)
        my = gval(y,2)
      end if
!         speed loop
    end do
!       points loop
  end do
!
!     3d coordinate axis
  call axis3d(un,ss,mxp,c,v,format, ft, xachse, yachse,&
  &nul, maxrig, maxtop, minbot, minlef,&
  &reke, zeit, datum, abszis, ordina,&
  &name, titelo, titelu, prname)
!     eventual speed extrapolation message, see parmanv.f
  call eplwrg(-1,out)
  j = len_trim(out)
  if (j .gt. 0) call symbol (1.0_lrk, 0.3_lrk, 0.2_lrk, out(1:j), 0, 0._lrk, j)
!
!     orbit position, speed range
  if (.not. py(1) .lt. 0) then
!       position, not support
    write(out,25) py(1),ddot,rpg(1),sla,rpg(ns)
  else
!       'supp='
    txt = pltmsgf(30, 66)
    j = len_trim(txt)
!       support number
    i = nint(abs(py(1)))
    write(out,55) txt(1:j),i,ddot,rpg(1),sla,rpg(ns)
  end if
  j = len_trim(out)
  call pen(1)
  call symbol (0.2_lrk, 17.2_lrk, .3_lrk, out(1:j), 0, 0._lrk,j)
!     number of modes
  txt = pltmsgf(30, 62)
  j = len_trim(txt)
  write(out,45) txt(1:j),ddot,nmd
  j = len_trim(out)
  call symbol (15.2_lrk, 17.2_lrk, .3_lrk, out(1:j), 0, 0._lrk, j)
!
!     displacement at angle
  xachse = 1
  yachse = 1
!     '      speed (rpm)' !1
  abszis = pltmsgf(30, 1)
!     'Amplitude (m)' !67
  ordina = pltmsgf(30, 67)
!     'Displacement Amp.' !30
  titelo = pltmsgf(30, 30)
!     number of output points
  do i = 1,np
    my = -1/nul
    ny = 1/nul
!       number of speeds
    do k = 1,nk
      if (value(i,k) .gt. my) my = value(i,k)
      if (value(i,k) .lt. ny) ny = value(i,k)
      xwert(k) = rp(i,k)
      ywert(k) = value(i,k)
    end do
    if (nk .gt. 0) then
!         x maximum and minimum
      maxrig = rp(i,nk)
      minlef = rp(i,1)
!         y maximum and minimum
      maxtop = my
      minbot = ny
      ft = 1
      if (i.eq.1) call rscale(xwert,ywert,mxp,ns)
      call malhap(ns, format, ft, mxp, xachse, yachse,&
      &nul, maxrig, maxtop, minbot, minlef,&
      &xwert, ywert,&
      &reke, zeit, datum, abszis, ordina,&
      &name, titelo, titelu, prname,color)
!         position, angle
      if (.not. py(i) .lt. 0) then
!           orbit position
        write(out,35) py(i),c1,rang
      else
!           support number
!           'supp='
        txt = pltmsgf(30, 66)
        l = len_trim(txt)
        j = nint(abs(py(1)))
        write(out,65) txt(1:l),j,c1,rang
      end if
      j = len_trim(out)
      call pen(1)
      call symbol (0.2_lrk, 17.2_lrk,.3_lrk, out(1:j), 0, 0._lrk,j)
!         number of modes
      txt = pltmsgf(30, 62)
      j = len_trim(txt)
      write(out,45) txt(1:j),ddot,nmd
      j = len_trim(out)
      call symbol (15.2_lrk, 17.2_lrk, .3_lrk, out(1:j), 0, 0._lrk, j)
    end if
!       eventual speed extrapolation message, see parmanv.f
    call eplwrg(-1,out)
    j = len_trim(out)
    if (j .gt. 0) call symbol (9.0_lrk, 16.5_lrk, 0.2_lrk, out(1:j), 0, 0._lrk, j)
!
!       output points
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
15 format(a,i1,a,i1,a)
25 format(f7.2,a,f6.0,a,f6.0)
35 format(f7.2,a,f5.1)
45 format(2a,i3)
55 format(a,i2,a,f6.0,a,f6.0)
65 format(a,i2,a,f5.1)

!
end subroutine tmors
!
!     ==================================================================
!>    @brief bearing parameters curves.
!
!>    @param[in] nbrg number of lateral bearings
!>    @param[in] nrpm number of speed points
!>    @param[in] nxm dimension of number of bearings on speed vector
!>     and bearing parameters matrix, same as mxm parameter.
!>    @param[in] sp speed vector
!>    @param[in] pa bearing parameters matrix
!>    @param[in] spdn rated speed (rpm)
!>    @param[in] stdio flag for standard input and output
!>    @param[in] knd kind of bearing code, 1=coefficient, 2=table
!>    @param[in] cbk name of bearing kind, coeff or table
!>    @param[in] pfn output file name, used for non standard output
!
subroutine beap(nbrg,nrpm,nxm,sp,pa,spdn,stdio,&
&knd,cbk,pfn)
  use rd_textfun, only: pltmsgf
  use com_nbc, only: cp, scl, bc
  use com_opt, only: std
  use com_pmk, only: nt, kmc, cmc, rmc
  use rd_kinds, only: lrk
  implicit none
!
  integer :: nbrg, nrpm, nxm
  real(lrk) :: sp, pa, spdn
  dimension pa(nrpm+1,nxm,8),sp(nrpm+1)
  logical :: stdio
  character(len=1) :: knd
  character(len=5) :: cbk
  character(len=255) :: pfn
!
  character(len=1) :: ckd
!     legend names
  character(len=3) :: leg, cds
  character(len=4) :: nmm, pmm, cpl
!     output section name
  character(len=10) :: sec
!     plot messages
  character(len=30) :: buf
  character(len=50) :: txt
!     mkidx,colora -> marker index,pen colors
  integer :: i, j, k, l, m, o, mxb, nmk, mkidx, colora
!     number of markers, equation parameters
  dimension leg(8),colora(4)
  parameter(nmm = 'beap',cds = 'mxp',pmm = 'vsbp',cpl = '.plt',&
!     see sbeapar on saidas.f
  &leg = (/'Khh','Khv','Kvh','Kvv','Chh','Chv','Cvh','Cvv'/),&
  &ckd = '2',colora = (/5,3,6,1/))
!     default Pen colors: 1:Black, 2:Red, 3:Green, 4:Yellow, 5:Blue,
!     6:Magenta
!
  real(lrk) :: nini, nfin, xx, yy, mxrf, mnrf, riff, mkrpm, mxv, mvl, mnv, dm
  parameter (nmk = 8,mvl = 1e18_lrk)
  dimension mkidx(nmk),mxv(2),mnv(2),dm(4)
!
!     malhap stuff
  integer :: ft, format, xachse, yachse, color
  real(lrk) :: nul, maxrig, maxtop, minbot, minlef
  real(lrk) :: xwert(nrpm+2), ywert(nrpm+2)
  character(len=8) :: zeit, prname
  character(len=10) :: datum
  character(len=30) :: name, abszis, ordina
  character(len=31) :: titelo, titelu
  character(len=70) :: reke
!
!     input parameter
  integer :: mxm
  parameter (mxm = 9)
!
!     max number of parameters lines
  integer :: mpm
  parameter (mpm = 99)
!
!     variable speed bearing parameters table
!
!
!     std plot output
!
  intrinsic :: len_trim, nint, real
!
!     standard i/o
  std = stdio
!
!     standard plot marker
  if (std) then
    write(sec,5) pmm,cpl
    call marksec(sec,0)
  end if
!     initialize hpgl plot
  call plots(0,0,pfn)
!
!     malhap settings
  format = 5
  xachse = 1
  yachse = 1
  nul = 1E-12_lrk
!     titles
  call header(reke,zeit,datum,name,titelu)
  prname = pmm
  abszis = pltmsgf(30, 1)
!
  nini = sp(1)
  nfin = sp(nrpm)
!     marker speed increment
  mkrpm = (nfin-nini)/real(nmk, lrk)
!
  o = 0
!     bearing number loop
  do l = 1,nbrg
    do m = 1,cp
      if (nint(bc(m)) .eq. l) then
        o = o+1
!           maximal and minimal
        mnv(1) = mvl
        mxv(1) = -mvl
        mnv(2) = mnv(1)
        mxv(2) = mxv(1)
!           marker
        k = 0
!           speed loop
        do i = 1,nrpm
!             rpm -> rad/s
          xwert(i) = sp(i)
!             max / min stiffnsess
          do j = 1,4
            if (pa(i,o,j) .gt. mxv(1)) mxv(1) = pa(i,o,j)
            if (pa(i,o,j) .lt. mnv(1)) mnv(1) = pa(i,o,j)
          end do
!             max / min damping
          do j = 5,8
            if (pa(i,o,j) .gt. mxv(2)) mxv(2) = pa(i,o,j)
            if (pa(i,o,j) .lt. mnv(2)) mnv(2) = pa(i,o,j)
          end do
          if (sp(i) .ge. (k+1)*mkrpm .and. k .lt. nmk) then
            k = k+1
            mkidx(k) = i
          end if
        end do
!
!           kind of parameters
        if (knd .eq. ckd) then
!             table
          mxb = nt(m)
!             check for bearing parameters table bounds
!             parameters loop
          do j = 1,4
!               speed loop
            do i = 1,mxb
!                 stiffness
              yy = kmc(l,i,j)*scl(l)
              if (yy .gt. mxv(1)) mxv(1) = yy
              if (yy .lt. mnv(1)) mnv(1) = yy
!                 damping
              yy = cmc(l,i,j)*scl(l)
              if (yy .gt. mxv(2)) mxv(2) = yy
              if (yy .lt. mnv(2)) mnv(2) = yy
            end do
          end do
!             last index -> max speed
          j = nt(m)
!             graphic x bounds
          minlef = riff(rmc(l,1) .lt. nini,rmc(l,1),nini)
          maxrig = riff(rmc(l,j) .gt. nfin,rmc(l,j),nfin)
        else
!             graphic x bounds
          minlef = nini
          maxrig = nfin
        end if
!
!           set new scale
        call sreset(0)
!
!           graphic y bounds
        minbot = mnv(1)
        maxtop = mxv(1)
!           page number
        ft = 1
!
        buf = pltmsgf(30, 32)
        j = len_trim(buf)
        write(titelo,15)buf(1:j),l
        ordina = pltmsgf(30, 31)
!
!           stiffness
!
        do j = 1,4
          color = colora(j)
          do i = 1,nrpm
            ywert(i) = pa(i,o,j)
          end do
!
          call malhap(nrpm, format, ft, nrpm, xachse, yachse,&
          &nul, maxrig, maxtop, minbot, minlef,&
          &xwert, ywert,&
          &reke, zeit, datum, abszis, ordina,&
          &name, titelo, titelu, prname,color)
!             plot markers
!             kind of parameters
          if (knd .eq. ckd) then
!               bearing table
            do i = 1,mxb
              xx = rmc(m,i)
              yy = kmc(m,i,j)*scl(m)
              call mark(xx, yy, 50._lrk, j)
            end do
          else
!               equation
            do i = 1,nmk
              xx = xwert(mkidx(i))
              yy = pa(mkidx(i),o,j)
              call mark(xx, yy, 50._lrk, j)
            end do
          end if
!             legends
          call legend(real(-3.5_lrk+j*2.3_lrk, lrk),-1.5_lrk,j,0.28_lrk,color,3,leg(j))
!             page counter
          ft = ft+1
        end do
!
!           rated speed
        if (spdn .gt. 0) then
!             get stiffness at rated speed -> dm
          do j = 1,4
            dm(j) = pa(nrpm+1,l,j)
          end do
!             red
!             minimum stiffness
          xx = mnrf(dm,4,4)
          ywert(1) = xx
!             maximum stiffness
          xx = mxrf(dm,4,4)
          ywert(2) = xx
!             hold speed
          do j = 1,4
            dm(j) = xwert(j)
          end do
          xwert(1) = spdn
          xwert(2) = spdn
!             red
          color = 2
          call rscale(xwert,ywert,nrpm,2)
          call malhap (2, format, ft, nrpm, xachse, yachse,&
          &nul, maxrig, maxtop, minbot, minlef,&
          &xwert, ywert,&
          &reke, zeit, datum, abszis, ordina,&
          &name, titelo, titelu, prname,color)
!             restore speed
          do j = 1,4
            xwert(j) = dm(j)
          end do
!
!             rated speed
          buf = pltmsgf(30, 64)
          k = len_trim(buf)
          write(txt,35) buf(1:k),spdn
          k = len_trim(txt)
          call symbol (15.5_lrk,16.5_lrk,.25_lrk,txt(1:k),0,0._lrk,k)
        end if
!           kind of parameters, coeff/table
        call pen(1)
        call symbol (7.0_lrk,16.5_lrk,.25_lrk,cbk,0,0._lrk,5)
!
!           damping
!
        call sreset(0)
        ft = 1
        buf = pltmsgf(30, 33)
        j = len_trim(buf)
        write(titelo,15) buf(1:j),l
        ordina = pltmsgf(30, 34)
        minbot = mnv(2)
        maxtop = mxv(2)
        do j = 1,4
          color = colora(j)
          do i = 1,nrpm
            ywert(i) = pa(i,o,j+4)
          end do
!
          call malhap(nrpm, format, ft, nrpm, xachse, yachse,&
          &nul, maxrig, maxtop, minbot, minlef,&
          &xwert, ywert,&
          &reke, zeit, datum, abszis, ordina,&
          &name, titelo, titelu, prname,color)
!             plot markers
!             kind of parameters
          if (knd .eq. ckd) then
!               bearing table
            do i = 1,mxb
              xx = rmc(m,i)
              yy = cmc(m,i,j)*scl(m)
              call mark(xx, yy, 50._lrk, j)
            end do
          else
!               equation
            do i = 1,nmk
              xx = xwert(mkidx(i))
              yy = pa(mkidx(i),o,j+4)
              call mark(xx, yy, 50._lrk, j)
            end do
          end if
!             legends
          call legend(real(-3.5_lrk+j*2.3_lrk, lrk),-1.5_lrk,j,0.28_lrk,color,3,leg(j+4))
!             page counter
          ft = ft+1
        end do
!
!           rated speed
        if (spdn .gt. 0) then
!             get damping at rated speed -> dm
          do j = 1,4
            dm(j) = pa(nrpm+1,o,4+j)
          end do
!             red
!             minimum stiffness
          xx = mnrf(dm,4,4)
          ywert(1) = xx
!             maximum stiffness
          xx = mxrf(dm,4,4)
          ywert(2) = xx
!             hold speed
          do j = 1,4
            dm(j) = xwert(j)
          end do
          xwert(1) = spdn
          xwert(2) = spdn
!
!             red
          color = 2
          call rscale(xwert,ywert,nrpm,2)
          call malhap (2, format, ft, nrpm, xachse, yachse,&
          &nul, maxrig, maxtop, minbot, minlef,&
          &xwert, ywert,&
          &reke, zeit, datum, abszis, ordina,&
          &name, titelo, titelu, prname,color)
!             restore speed
          do j = 1,4
            xwert(j) = dm(j)
          end do
!             rated speed
          buf = pltmsgf(30, 64)
          k = len_trim(buf)
          write(txt,35) buf(1:k),spdn
          k = len_trim(txt)
          call symbol (15.5_lrk,16.5_lrk,.25_lrk,txt(1:k),0,0._lrk,k)
        end if
!           kind of parameters, coeff/table
        call pen(1)
        call symbol (7.0_lrk,16.5_lrk,.25_lrk,cbk,0,0._lrk,5)
!
!           parameter bearing if
      end if
!
!         bearing parameters do
    end do
!     end bearing number loop
  end do
!
!     standard plot marker
  if (std) then
    call bflush
    call marksec(sec,1)
  else
    call plots(1,0,pfn)
  end if
!
  return
!
5 format(2a)
15 format(a,i2)
35 format(a,f7.1)
!
end subroutine beap
!
