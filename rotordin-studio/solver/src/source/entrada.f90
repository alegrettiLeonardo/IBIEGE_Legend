!     $Id$
!     ==================================================================
!
!>    @file entrada.f
!>    @author francisco
!>    @brief data input, last changes:<br>
!>    added optionsf, options handling - francisco - 03/12/2008<br>
!>    added beapar, bearing parameters handling - francisco - 03/12/2008
!>    added beatab, bearing parameters table handling  - francisco - 03/
!>    added kind of force in unbalance / deflection calculations - franc
!>    added beasupf bearing options data handling - francisco - 17/11/20
!>    changed beasupf, add bearing support parameters vector - francisco
!>    changed modes quantity check, parameter xmd - francisco - 22/02/20
!>    added speed-shape option, shp - francisco - 15/06/2011<br>
!>    added ori block /unb/ - francisco - 29/06/2011<br>
!>    added standard i/o capabilities - francisco - 14/02/2011<br>
!>    added tpf = (0) default unbalance, unbalance reading - francisco -
!>    added input/output of time response angle TANGLE - francisco 01/10
!>    added time response angle rangle on mdd block - francisco 01/10/20
!>    added missing options handling  in coptf - francisco jul-15<br>
!>    added read for MINAMP and MDRPM data in entrada - francisco - jul-
!>    added kph, kth, block tor and read in entrada - francisco - oct-15
!>    added reading support data on entrada - francisco - oct-15<br>
!>    added sok on entrada, support data on main input file - francisco
!>    added bearing parameter scale sc - francisco - oct-15<br>
!>    added support parameter scale sps - francisco - nov-15<br>
!>    added angular critical speed support - francisco - nov-17<br>
!>    added variable speed bearing parameters plot "j" option - francisc
!>    added disk offset check option text, coptf changed function - fran
!>    changed to central messages,added icoptf function - francisco - ap
!>    added message override call on entrada - francisco - may-19<br>
!>    added torsional input data, input kind = 3 - francisco - nov-19<br
!>    changed optionsf added torsional logical argument- francisco - nov
!>    added beamasf.f bearing mass function, see parmanv.f - francisco -
!>    added harmonic info on unbalance section of entrada.f - francisco
!>    changed concentrated masses and inertias dimension mxic to 15 - fr
!>    changed mxb = 99 francisco - apr-20<br>
!>    added au angle unit for excitation and ru for response - francisco
!>    changed coptf function, chanded data initialization by loop - fran
!>    updated fixed characters to parameters on entrada - francisco - se
!>    added angle unit handling on entrada - francisco - sep-20<br>
!>    moved beamasf to parmanv.f - francisco - oct-20
!>    added isf on entrada, bend stress median filter window length 0 -
!>    added torsion restriction data handling on entrada - fracisco - ma
!>    added setable crossing lines entry on optionsf for Campbell - fran
!>    added setable modes to show on optionsf - francisco - may-21>br>
!>    added flexural-torsion flag parameter on optionsf and entrada - fr
!>    added cs number of section argument on optionsf - francisco - aug-
!>    added cs number of section argument on tentrada - francisco - aug-
!>    added gycscid, gyroscopic factor comp. equivalent diameters - fran
!>    added excitation kind check on entrada, francisco - sep-21<br>
!>    added position check on entrada, francisco - sep-21<br>
!>    added number of modes check on entrada, francisco - oct-21<br>
!>    added chkspps verify unbalance negative values as valid support in
!>    added iex export job index on entrada, optionsf, coptf, francisco
!>    added zero scale check on beatab, beapar and beasupf, francisco -
!>    added distributed force section uniqueness check ckdsfrf, francisc
!>    added fxstf fixed stiffnes for no displacement on entrada - franci
!
!     ==================================================================
!>    @brief set calculation option for valid smallcap letters.
!>    see blockd.f
!
!>    @param[in] bf1 smallcap letter to check and set options.
!>    @param[in,out] lopt logical options with appropriate size
!>    @param[in] mop logical options vector size
!>    @return option index that was set or zero otherwise
!
integer function icoptf(bf1,lopt,mop)
  use com_vldo, only: vopt
  implicit none
!
  character(len=1) :: bf1
  logical :: lopt
  integer :: mop
  dimension lopt(mop)
!
  integer :: i, j, iop
!
!     valid options, see blockd.f
  integer :: jvop
  parameter (jvop = 20)
!     & 'c' !1 cambell
!     &,'m' !2 modes
!     &,'f' !3 unbalance
!     &,'t' !4 time
!     &,'$' !5 input file set
!     &,'e' !6 geometry
!     &,'$' !7 use speed dep. param
!     &,'s' !8 elastic
!     &,'k' !9 map
!     &,'b' !10 bearing support
!     &,'$' !11 std
!     &,'x' !12 none (std export)
!     &,'$' !13 none
!     &,'p' !14 plot
!     &,'a' !15 angular map
!     &,'j' !16 bearing plot
!     &,'$' !17 override messages
!     &,'$' !18 torsion option
!     &,'n' !19 torsion numerical integration
!     &,'r' !20 flexural-torsion
!
  intrinsic :: min
!
  iop = 0
!
  j = min(jvop,mop)
  do i = 1,j
    if (bf1 .eq. vopt(i))then
      if (.not. lopt(i)) then
        iop = i
        lopt(i) = .true.
        exit
      end if
    end if
  end do
!
  icoptf = iop
!
  return
!
end function icoptf
!
!     =================================================================
!>    @brief check for directory name signs on a text.
!>    signs means initial dot, slash or back slash.
!>    @param[in] txt input text to check
!>    @return zero if guess that txt is NOT a directory name
integer function chkdirsf(txt)
  implicit none
!
  character(len=*) :: txt
!
  integer :: iiff
  character(len=1) :: slash, bslash, dot
  parameter (slash = '/',bslash = char(92),dot = '.')
!
  intrinsic :: char, index, len_trim
!     check for slash
  chkdirsf = iiff(index(txt,slash) .gt. 0,1,0)
  if (chkdirsf .gt. 0) return
!     check for backslash
  chkdirsf = iiff(index(txt,bslash) .gt. 0,1,0)
  if (chkdirsf .gt. 0) return
!     check for initial dot
  if (len_trim(txt) .gt. 0) then
    chkdirsf = iiff(txt(1:1) .eq. dot,1,0)
    if (chkdirsf .gt. 0) return
  end if
!     will return zero
  return
!
end function chkdirsf
!
!     ==================================================================
!>    @brief check calculation options text from input, command line arg
!>     overrides it. The text must contain only valid smallcap letters.
!>     should not contain dot or slashes.
!>     see init.f,rotordin.f,saidas.f
!>    @return true if input text contains only valid letters or is blank
!>    false otherwise.
!
!>    @param[in] ops options text, case sensitive<br>
!>    valid options are: a, c, m, f, t, e, l, s, k, b, x, j, n, r
!>    <ul>
!>    <li>a = angular undamped critical speed map</li>
!>    <li>c = calculate Campbell diagram</li>
!>    <li>m = calculate modal shapes</li>
!>    <li>f = calculate frequency response</li>
!>    <li>t = calculate time response</li>
!>    <li>e = export geometry files</li>
!>    <li>l = generate log file</li>
!>    <li>s = static elastic line</li>
!>    <li>k = undamped critical speed map</li>
!>    <li>b = consider support on bearing</li>
!>    <li>x = export modal parameters plain format</li>
!>    <li>j = bearing plot</li>
!>    <li>n = torsion numerical integration</li>
!>    <li>r = flexural-torsion</li>
!>    </ul>
!>    @param[out] iex export kind flag:0=HB,1=mm,2=plain
!
!     changed to logical function - francisco feb-19
logical function coptf(ops,iex)
  use com_ppr, only: opt, ifn
  use com_vldo, only: vopt
  implicit none
!
!     arguments
  integer :: iex
  character(len=10) :: ops
!
!     locals
  integer :: ics, i, k
  character(len=1) :: bf1, blank
  parameter(blank = ' ')
!
!     added - francisco  - feb-19
  integer :: icoptf, chkdirsf
  logical :: ok
!
!     calculation options
  integer :: mop
  parameter(mop = 20)
!
!     valid options, see blockd.f
  integer :: jvop
  parameter (jvop = 20)
!
!     check options
  logical :: lopt(mop)
!
!     intrinsic functions
  intrinsic :: char, len_trim, index
!
!     please see init.f,rotordin.f,saidas.f
!
!     c calculate Campbell diagram
!     e export geometry files
!     f calculate frequency response
!     j variable speed bearing parameters plot
!     l generate log file
!     m calculate modal shapes
!     t calculate time response
!     p output HPGL plot
!     ...
!
!     opt(1) cpb campbell -c
!     opt(2) mds modes -m
!     opt(3) fqr frequency resp. -f
!     opt(4) tmr time resp. -t
!     opt(5) fng input file set
!     opt(6) export geometry to file -e
!     opt(7) speed dependent bearing parameters
!     opt(8) static elastic line -s
!     opt(9) undamped critical speed map -k
!     opt(10) bearing support
!     opt(11) standard i/o
!     opt(12) none (std export)
!     opt(13) none
!     opt(14) output HPGL plot -p
!     opt(15) undamped angular critical speed map -a
!     opt(16) variable speed journal bearing parameters plot -j
!     opt(17) override messages -o
!     opt(18) torsion option
!     opt(19) torsion time transient
!     opt(20) flexural-torsion
!     iex export kind flag = 0 HB, 1=mm,2=plain!
!
  logical :: torf ! Sartori, local var True or False = torf
  torf = .false.
!
  ok = .true.
!     init options check
  do i = 1,mop
    lopt(i)=.false.
  end do
!     ops size
  ics = len_trim(ops)
!     added check invalid option - francisco - feb-19
  do i = 1,ics
    bf1 = ops(i:i)
    do  k = 1,jvop
!         check for known options
      ok = (vopt(k) .eq. bf1) .or. (bf1 .eq. blank)
!         check for duplicated option
! TODO: sartori - reavaliar a linha de codigo comentada.
!       encontrado diferenca de comportamento entre compilador window x
!       possivel erro em icoptf() ou deveria ser .or. no lugar de .and.
!       temporariamente removido a verificação de entrada duplicada
      !   ok = ok .and. (icoptf(bf1,lopt,mop) .ne. 0)
      if (ok) exit
    end do
    if (.not. ok) exit
  end do
!     added check for file name text:slash or dot or back slash - franci
  i = chkdirsf(ops)

  ! coptf = ok .and. (i .eq. 0)
  torf = (i .eq. 0)
  torf = torf .and. ok
!
!     set calculation options
  !if (coptf) then
  if (torf) then
    do i = 1,ics
!         just one letter
      bf1 = ops(i:i)
!         note that opt is passed here, k has no meaning
      k = icoptf(bf1,opt,mop)
    end do
!       check export option
    if (opt(12)) then
      opt(12) = .false.
!         iex export kind flag = 0 HB, 1=mm,2=plain!
      iex = 2
    end if
  end if
!
  coptf = torf  ! sartori, set function return value
  return
!
end function coptf
!
!     ==================================================================
!>    @brief read bearing parameters table.
!
!>    @param[in] nm number of bearing data files
!>    @param[in] bl bearing number
!>    @param[in] opf bearing data file name
!>    @param[in] std standard i/o flag
!>    @param[out] errmsg return a error message
!>    @param[in] mx dimension of bl and opf parameters
!>    @param[out] ok return flag. unsuccessful if <0.
!
subroutine beatab(nm,bl,opf,std,errmsg,mx,ok)
  use rd_textfun, only: fomsgf, fopmsf
  use com_nbc, only: cp, scl, bc
  use com_pmk, only: nt, kmc, cmc, rmc
  use com_pmt, only: tph, tth
  use com_ppr, only: opt, ifn
  use com_pst, only: wrl
  use rd_kinds, only: lrk
  use rd_bearing_contract, only: bearing_table_reset,bearing_read_row
  implicit none
  real(lrk) :: audit_row(11)
  integer :: audit_status
!
!     arguments
  character(len=99) :: errmsg
  integer :: nm, mx, ok
  logical :: std
  real(lrk) :: bl
  character(len=255) :: opf
  dimension bl(mx),opf(mx)
!
!     locals
  integer :: j, k, l, jj, kk, ip, chkdirsf
  logical :: ext
  character(len=1) :: blank
  character(len=3) :: cbd
  character(len=4) :: arb
  character(len=6) :: nmm
  character(len=8) :: per, csn
  character(len=255) :: buf, dt
  dimension csn(3)
  parameter(blank = ' ',cbd = 'mpm',&
  &nmm = 'beatab',csn = (/'1 line  ','np      ','bea.tdat'/))
!
!     max number of parameters lines
  integer :: mpm, mxm
  parameter (mpm = 99,mxm = 9)
!
!
!     additional rotationsl stiffnes - francisco - oct-15
!
!     parameters block
  integer :: mop
  parameter (mop = 20)
!
!     added - francisco - feb-19
!     work folder
!
!     intrinsic functions
  intrinsic :: len_trim
!
!     return init
  ok = -1
!
  if (.not. std) then
!       bearing parameter unit number (file)
    ip = 12
  else
!       standard i/o
    ip = 5
  end if
!
!     bearing parameters count
  cp = 0
  call bearing_table_reset()
  do j = 1,nm
    bc(j) = bl(j)
    buf = opf(j)
!
!       added check for local folder - francisco feb-19
    k = chkdirsf(buf)
    if (k .eq. 0 .and. len_trim(buf) .gt. 0) then
      k = len_trim(wrl)
      buf = wrl(1:k)//buf
    end if
!
!       length bearing data file name space striped
    k = len_trim(buf)
    if (.not. std) then
!         check for the bearing parameters file
      inquire(file = buf(1:k),exist = ext)
!         file not found
      if (.not. ext) then
        errmsg = fomsgf(99, nmm,2,buf(1:k),1)
        return
      end if
      open(ip,file=buf(1:k),err=60)
    end if
!
!EFNLB_11-100�0@27102008102630
!9 1
!500.00 67.98E06 -15.50E06 -20.59E07 37.15E07 90.10E04 -18.28E05 -18.53E
!912.50 68.23E06 -38.40E05 -17.82E07 25.97E07 59.10E04 -94.30E04 -95.50E
!1325.00 69.03E06 83.96E05 -16.16E07 18.33E07 48.90E04 -65.30E04 -66.10E
!1737.50 68.73E06 14.33E06 -15.54E07 15.52E07 40.10E04 -47.70E04 -48.00E
!2150.00 69.62E06 21.15E06 -15.46E07 13.63E07 36.30E04 -39.60E04 -40.40E
!2562.50 70.64E06 26.42E06 -15.49E07 12.38E07 33.10E04 -33.70E04 -34.20E
!2975.00 71.13E06 29.49E06 -15.56E07 11.73E07 29.90E04 -29.00E04 -29.40E
!3387.50 71.68E06 33.37E06 -15.71E07 10.96E07 27.80E04 -25.40E04 -25.60E
!3800.00 72.24E06 36.59E06 -15.86E07 10.39E07 26.00E04 -22.60E04 -22.80E
!EOF
!
!       section id, error report
    per = csn(1)
!       parameters reading
!       jump first header line
    read(ip,*,err=110) arb
!       id
    per = csn(2)
!       number of lines, optional scale
    read(ip,5,err=110,end=110) dt
    read(dt,*,iostat=l) nt(j),scl(j)
    if (l .ne. 0) then
      scl(j) = 1.0_lrk
      read(dt,*,err=110,end=110) nt(j)
    end if
    if (scl(j) .le. 0) then
!         'invalid option' !6
      errmsg = fomsgf(99, nmm,6,csn(3),0)
      return
    end if
!       check vector dimensions
    if (nt(j) .gt. mpm) then
!         close only if not standard io
      close(ip)
      errmsg = fomsgf(99, nmm,4,cbd,0)
      return
    end if
!       check min number of lines (at least 2)
    if (nt(j) .lt. 2) then
!         close only if not standard io
      close(ip)
      errmsg = fomsgf(99, nmm,7,blank,0)
      return
    end if
!       id
    per = csn(3)
!       parameters, checkout the order
    do jj = 1,nt(j)
!         rpm,kxx,kxz,kzx,kzz,cxx,cxz,czx,czz[,kph,kth]
      read(ip,5,err=110,end=110) dt
!         try read all parameters, check iostat
      call bearing_read_row(dt,j,jj,audit_row,errmsg,audit_status)
      if(audit_status/=0) then
        if(.not.std) close(ip)
        return
      end if
      rmc(j,jj)=audit_row(1)
      kmc(j,jj,1:4)=audit_row(2:5)
      cmc(j,jj,1:4)=audit_row(6:9)
      tph(j,jj)=audit_row(10)
      tth(j,jj)=audit_row(11)
    end do
!       may have last line like "eof"
    read(ip,15,end=200)
!       close only if not standard io
200 if (.not. std) close(ip)
!       next bearing
    cp = cp+1
!
  end do
!
!     use speed dep. bearing params, see init.f
  opt(7) = .true.
!
!     return ok
  ok = 0
  return
!
!     error on open bearing file
60 errmsg = fomsgf(99, nmm,1,buf,1)
  return
!
!     error on reading bearing parameters file
110 errmsg = fopmsf(99, nmm,3,per,buf)
  if (.not. std) close(ip)
  return
!
5 format(a)
15 format()
!
end subroutine beatab
!
!     ==================================================================
!>    @brief read bearing parameters equation coefficients.
!
!>    @param[in] nm number of bearing data files
!>    @param[in] bl bearing number
!>    @param[in] opf bearing data file name
!>    @param[in] std standard i/o flag
!>    @param[out] errmsg return a error message
!>    @param[in] mx dimension of parameters bl and opf
!>    @param[out] ok return flag. unsuccessful if <0.
!
subroutine beapar(nm,bl,opf,std,errmsg,mx,ok)
  use rd_textfun, only: fomsgf, fopmsf
  use com_cfm, only: c11, c12, c21, c22, d11, d12, d21, d22, nl
  use com_cft, only: eph, eth
  use com_nbc, only: cp, scl, bc
  use com_ppr, only: opt, ifn
  use com_pst, only: wrl
  use rd_kinds, only: lrk
  implicit none
!
!     locals
  character(len=6) :: nmm
  character(len=8) :: per, csn
  character(len=255) :: buf, dt
  dimension csn(2)
  parameter(nmm = 'beapar',&
  &csn = (/'beafiles','bea.pdat'/))
  integer :: j, k, l, ip, chkdirsf
  logical :: ext
!
!     arguments
  integer :: nm, mx, ok
  real(lrk) :: bl
  logical :: std
  character(len=99) :: errmsg
  character(len=255) :: opf
  dimension bl(mx),opf(mx)
!
!     input parameters
  integer :: mxm
  parameter (mxm = 9)
!
!     bearing parameters coefficients
!     additional rotational parameters - francisco - oct-15
!
!     parameters
  integer :: mop
  parameter (mop = 20)
!
!     added - francisco - feb-19
!     work folder
!
!     intrinsic functions
  intrinsic :: len_trim, index
!
!     init
!     return init
  ok = -1
!
  if (.not. std) then
!       bearing parameter unit number (file)
    ip = 12
  else
!       standard i/o
    ip = 5
  end if
!
!     reading input:
!     section id for error report
  per = csn(1)
!     bearing count
  cp = 0
  do j = 1,nm
    bc(j) = bl(j)
    buf = opf(j)
!
!       added check for local folder - francisco feb-19
    k = chkdirsf(buf)
    if (k .eq. 0 .and. l .gt. 0) then
      k = len_trim(wrl)
      buf = wrl(1:k)//buf
    end if
!
!       length bearing data file name space striped
    k = len_trim(buf)
    if (.not. std) then
!         check for the bearing parameters file
      inquire(file = buf(1:k),exist = ext)
      if (.not. ext) then
!           file not found
        errmsg = fomsgf(99, nmm,2,buf(1:k),1)
        return
      end if
      open(ip,file=buf(1:k),err=60)
    end if
!
!COEF:EFNLB_11-100�0@27102008102630
!8 1E6
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
!       section id, error report
    per = csn(2)
!       parameters reading
    read(ip,*,err=110,end=110)
!       scale
!       number of equations, optional scale
    read(ip,5,err=110,end=110) dt
    read(dt,*,iostat=l) nl(j),scl(j)
    if(l .ne. 0)then
      scl(j) = 1.0_lrk
      read(dt,*,err=110,end=110)nl(j)
    end if
    if (scl(j) .le. 0) then
!         'invalid option' !6
      errmsg = fomsgf(99, nmm,6,per,0)
      return
    end if
!       check number of equations
    if (nl(j) .lt. 8) then
      close(ip)
      errmsg = fomsgf(99, nmm,7,per,0)
      return
    end if
!
    read(ip,*,err=110,end=110) (c11(j,l),l=1,3)
    read(ip,*,err=110,end=110) (c12(j,l),l=1,3)
    read(ip,*,err=110,end=110) (c21(j,l),l=1,3)
    read(ip,*,err=110,end=110) (c22(j,l),l=1,3)
    read(ip,*,err=110,end=110) (d11(j,l),l=1,3)
    read(ip,*,err=110,end=110) (d12(j,l),l=1,3)
    read(ip,*,err=110,end=110) (d21(j,l),l=1,3)
    read(ip,*,err=110,end=110) (d22(j,l),l=1,3)
!       addtional rotational equations
    if (nl(j) .gt. 8) then
      read(ip,*,err=110,end=110) (eph(j,l),l=1,3)
      if (nl(j) .gt. 9) read(ip,*,err=110,end=110)&
      &(eth(j,l),l=1,3)
    else
      do l=1,3
        eph(j,l) = 0._lrk
        eth(j,l) = 0._lrk
      end do
    end if
!       may have last line like "eof"
    read(ip,15,end=200)
!
!       close only if not standard io
200 if (.not. std) close(ip)

    cp = cp+1
!
  end do
!
!     use speed dep. bearing params, see init.f
  opt(7) = .true.
!
!     return ok
  ok = 0
  return
!
!     error on open bearing file
60 errmsg = fomsgf(99, nmm,1,buf,1)
  return
!
!     error on reading bearing parameters file
110 errmsg = fopmsf(99, nmm,1,per,buf)
  if (.not. std) close(ip)
!
  return
!
5 format(a)
15 format()
!
end subroutine beapar
!
!     ==================================================================
!>    @brief check for negtive support unbalance reponse
!
!>    @param[in] mxm dimension of unbalance positions vector
!>    @param[in] mxr dimension of supoort indices vector
!>    @param[in] np number of unbalances
!>    @param[in] ns number of supports
!>    @param[in] bn supoort indices vector
!>    @param[in] pr unbalance positions vector
!>    @param[out] errmsg return a error message
!>    @param[out] ok return flag. unsuccessful if <0.
subroutine chkspps(mxm,mxr,np,ns,bn,pr,ok,errmsg)
  use rd_textfun, only: fomsgf
  use rd_kinds, only: lrk
  implicit none
!
  integer :: mxm, mxr, np, ns, bn, ok
  real(lrk) :: pr
  dimension pr(mxr),bn(mxm)
  character(len=99) :: errmsg
!
  real(lrk) :: prec
  logical :: sok
  integer :: ii, jj, kk
  character(len=7) :: nmm
  character(len=8) :: msg
  parameter (prec = 1e-12_lrk,nmm = 'chkspps',msg = 'supp.dat')
!
  intrinsic :: abs, nint
!
  ok = -1
!     unbalance positions loop
  do jj = 1,np
!       search negative unbalance position
    if (pr(jj) .lt. 0) then
      sok = .false.
!         support loop
      do ii = 1,ns
!           check for same negative index
        kk = abs(nint(pr(jj)))
        if (abs(bn(ii) - kk) .lt. prec) then
!             found ok
          sok = .true.
          exit
        end if
      end do
!         check negative match
      if (.not. sok) then
!           'invalid option' !6
        errmsg = fomsgf(99, nmm,6,msg,0)
        return
      end if
!         search negative unbalance position if
    end if
!       unbalance positions loop
  end do
!
  ok = 0
!
  return

end subroutine chkspps
!
!     ==================================================================
!>    @brief options file processing (opt).
!
!>    @param[in] nomarq options input file name
!>    @param[in] cs number of sections
!>    @param[in] std standard i/o
!>    @param[out] iex export kind flag = 0 HB, 1=mm,2=plain
!>    @param[out] tors true if torsional kind
!>    @param[out] fto true if flexural-torsion optional data is present
!>    @param[out] errmsg return a error message
!>    @param[out] ok return flag. unsuccessful if <0.
!
subroutine optionsf(nomarq,cs,std,iex,tors,fto,errmsg,ok)
  use rd_textfun, only: fomsgf, fopmsf
  use com_bef, only: opf
  use com_cpbd, only: dmp, nit
  use com_knd, only: cknd, supr
  use com_mdd2, only: imd, nsm
  use com_mfa, only: ma, rm, au
  use com_ymc, only: nbrg, rks, pc
  use rd_kinds, only: lrk
  implicit none
!
!     arguments
  character(len=99) :: errmsg
  character(len=255) :: nomarq
  logical :: std, tors, fto
  integer :: iex, cs, ok
!
!     max bearings,max number of modes,search for commons
  integer :: mxm, xmd
  parameter (mxm = 9,xmd = 19)
!
!     locals
!     added logical function coptf - francisco - feb-19
  real(lrk) :: lma, lrm, bc
  logical :: coptf
  character(len=1) :: cop
  character(len=3) :: cbd
  character(len=4) :: cdm, clm
  character(len=5) :: cst
  character(len=8) :: nmm, per, cpr
  character(len=10) :: c10
  character(len=13) :: dsc
  character(len=99) :: buf
  dimension bc(mxm),cop(6),cpr(2),cdm(mxm),dsc(4),clm(xmd)
  parameter (cst = 'stdio',cbd = 'mxm',nmm = 'optionsf',&
  &dsc = (/'bearing index','cpbl.crx.bufv',&
  &'mode.vis.bufv','options      '/),&
  &cop = (/'1','2','3','(',')',','/),&
  &cpr = (/'kind    ','beafiles'/))
  integer :: i, j, iu, nm, oki, splitf
!     calculation options
  character(len=10) :: ops
!
!     kind of bearing parameters
!     table or coefficients
!
!     min amplification factor fator and modal rpm block
!     angle unit r -> radians, default degree
!
!     Campbell crossing lines 1,2,0.5
!
!     bearings
!     positions
!     number of bearings
!
!     bearing files
!
!     modes to show
!
!     intrinsic functions
  intrinsic :: len_trim
!
!     init
!     return init
  ok = -1
!
!     open the file
  if (.not. std) then
!       input unit number (file)
    iu = 12
!       nomarq should hold the bearing information (options) file
!       input file name string length space striped
    i = len_trim(nomarq)
    open(iu,file=nomarq(1:i),err=50)
  else
!       standard i/o
    iu = 5
    nomarq = cst
  end if
!
!     reading input
!     section id for error report
  per = cpr(1)
!     kind of bearing params, optional modes to show, comma separated
  read(iu,5,end=100,err=100) c10,buf
  j = len_trim(c10)
!     just one character
  cknd = c10(j:j)
!     check for user defined modes to show
  j = len_trim(buf)
  if (j .gt. 0) then
!       split values between comma->clm
!       i-> number of buffer values
    i = splitf(buf,cop(6),xmd,clm)
!       check if out of buffer dimension
    if (i .gt. xmd) then
!         more data than buffer dimension xmd
      errmsg = fomsgf(99, nmm,4,dsc(3),0)
      return
    end if
!       count valid mode index value > 0 < xmd
    oki = 0
!       read clm buffer loop
    do j = 1,i
!         read data->nm
      read(clm(j),*,end=100,err=100) nm
!         check if valid
      if (nm .gt. 0 .and. nm .le. xmd) then
        oki = oki+1
        imd(oki) = nm
      end if
    end do
!       number of valid modes to show
    nsm = oki
  else
!       no defined modes to show, show all
    nsm = 0
  end if
!
!     check for torsional kind = 3 nov-19 - f
  if (cknd .eq. cop(3)) then
!       torsional input
    call tentrada(iu,nomarq,cs,iex,fto,errmsg,oki)
    if(oki .lt. 0) return
!       torsional flag
    tors = oki .eq. 0
!
  else
!       torsional flag
    tors = .false.
!
!       section id for error report
    per = cpr(2)
!       nm - number of bearing data files
!       lma - amplitude limit
!       lrm - modes speed
    read(iu,15,end=100,err=100) nm,lma,lrm,buf
!       override if not zero
    if (lma .gt. 0) ma = lma
    if (lrm .gt. 0) rm = lrm
!       check vector max dimension
    if (nm .gt. mxm .or. nm .gt. nbrg) then
      close(iu)
      if (nm .gt. mxm) then
        errmsg = fomsgf(99, nmm,4,cbd,0)
      else
!           number of bearing greather than bearing count
        errmsg = fomsgf(99, nmm,4,dsc(1),0)
      end if
      return
    end if
!       Campbell crossing lines
    j = len_trim(buf)
    if (j .gt. 0) then
!         split values between comma->cdm
!         i-> number of buffer values
      i = splitf(buf,cop(6),mxm,cdm)
!         check if out of buffer dimension
      if (i .gt. mxm) then
!           more data than buffer dimension mxm
        errmsg = fomsgf(99, nmm,4,dsc(2),0)
        return
      end if
!         count valid crossing line value > 0
      oki = 0
!         read cdm buffer loop
      do j = 1,i
!           read data->lrm
        read(cdm(j),*,end=100,err=100) lrm
!           check if valid
        if (lrm .gt. 0) then
          oki = oki+1
          dmp(oki) = lrm
        end if
      end do
!         number of valid crossing lines
      nit = oki
    else
!         no defined Cambell cross lines
!         will set on campbell subroutine
      nit = 0
    end if
!
!       loop over bearings parameters number
!       number of different param data
    do j = 1,nm
!         bc - bearing number
!         opf - bearing data file name
      read(iu,25,end=100,err=100) bc(j),opf(j)
    end do
!
!       calculation options [optional]
    read(iu,35,end=200,err=200) ops
!       call options proccessing
!       added check invalid option - francisco - feb-19
!       iex -> default export
    if (.not. coptf(ops,iex)) then
      errmsg = fomsgf(99, nmm,6,dsc(4),0)
      return
    end if
!
200 continue
!
!       bearing kind options
    if (cknd .eq. cop(1)) then
!         nm - number of bearing data files
!         bc - bearing number
!         opf - bearing data file name
      call beapar(nm,bc,opf,std,errmsg,mxm,oki)
!         return with error
      if(oki .lt. 0) return
    elseif (cknd .eq. cop(2)) then
!         parameters coefficients
      call beatab(nm,bc,opf,std,errmsg,mxm,oki)
      if(oki .lt. 0) return
    else
      errmsg = fomsgf(99, nmm,6,dsc(4),0)
      return
    end if
!
!       torsion kind if
  end if
!
  if (.not. std) close(iu)
!
!     return ok
  ok = 0
  return
!
!     error on open options file
50 errmsg = fomsgf(99, nmm,1,nomarq,1)
  return
!
!     error on reading options file
100 errmsg = fopmsf(99, nmm,3,per,nomarq)
  if (.not. std) close(iu)
  return
!
5 format(/,2a)
15 format(//,i10,2f10.0,a,//)
25 format(f10.0,a)
35 format(//,a)
!
end subroutine optionsf
!
!     ==================================================================
!>    @brief bearing support parameters file.
!
!>    @param[in] nomarq bearing support input file name
!>    @param[in] std standard i/o
!>    @param[out] errmsg return a error message
!>    @param[out] ok return flag. unsuccessful if <0.
!
subroutine beasupf(nomarq,std,errmsg,ok)
  use rd_textfun, only: fomsgf, fopmsf
  use com_knd, only: kind => cknd, supr
  use com_sp1, only: ns, bn
  use com_sp2, only: sup, sps
  use com_sp3, only: kos
  use com_ymc, only: nbrg, rks, pc
  use rd_kinds, only: lrk
  implicit none
!
!     arguments
  character(len=255) :: nomarq
  character(len=99) :: errmsg
  integer :: ok
  logical :: std
!
!     locals
  character(len=1) :: cop
  character(len=3) :: cbn
  character(len=7) :: nmm
  character(len=8) :: per
  character(len=8) :: cpr
  character(len=10) :: dm
  character(len=13) :: dsc
  dimension cop(2),cpr(2)
  parameter(cbn = 'mxm', dsc = 'support index',cop = (/' ','S'/),&
  &nmm = 'beasupf',cpr = (/'sup.numb','sup.data'/))
!     error messages
  integer :: i, j, iu, st, oki
  logical :: ext
!
!     max de linhas de parametros
  integer :: mxm
  parameter (mxm = 9)
!
!     bearing parameters
!
!     bearing suport
!     number of parameters lines
!     bearing number
!     description of support
!     added scale sps - francisco - nov-15
!
!     bearing kind / support parameters
!
!     intrinsic functions
  intrinsic :: len_trim
!
!     init
!     return init
  ok = -1
!
!     check output kind
  if (.not. std) then
!       input file name string length space striped
    i = len_trim(nomarq)
!       nomarq should hold the bearing support information
!       check if the file exist
    inquire(file=nomarq(1:i),exist=ext)
    if(.not. ext) then
!         does not exist
      errmsg = fomsgf(99, nmm,2,nomarq,1)
      return
    end if
!       open the file
!       input unit number (file)
    iu = 12
    open(iu,file=nomarq(1:i),err=50)
  else
!       standard i/o
    iu = 5
  end if
!
!SUPPORT   SCALE
!02        1
!
!BEANBR    KXX       KXZ       KZZ       KZX       CXX       CXZ       C
!01        10.00E+06 00.00E+00 10.00E+06 00.00E+00 10.00E+03 00.00E+00 1
!02        10.00E+06 00.00E+00 10.00E+06 00.00E+00 10.00E+03 00.00E+00 1
!
!     section id for error report
  per = cpr(1)
!     reading input data
!     number of supports and optional scale
  read(iu,5,err=100,end=100) dm
  read(dm,*,iostat=st) ns,sps
  if (st .ne. 0) then
    sps = 1.0_lrk
    read(dm,*,err=100,end=100) ns
  end if
  if (sps .le. 0) then
!       'invalid option' !6
    errmsg = fomsgf(99, nmm,6,cpr(2),0)
    return
  end if
!     check vector max dimmension
  if (ns .gt. mxm) then
    close(iu)
    errmsg = fomsgf(99, nmm,4,cbn,0)
    return
  end if
!
!     support data
  per = cpr(2)
  read(iu,15,end=100,err=100)
  do j = 1,ns
!       #nbr,kxx,kxz,kzz,kzx,cxx,cxz,czz,czx,mass
    read(iu,25,end=100,err=100)&
    &bn(j),sup(j,1),sup(j,2),sup(j,4),sup(j,3),&
    &sup(j,5),sup(j,6),sup(j,8),sup(j,7),&
    &sup(j,9),kos(j)
!
!       check for support number
    if (bn(j) .gt. nbrg .or. bn(j) .le. 0) then
      close(iu)
      errmsg = fomsgf(99, nmm,5,dsc,0)
      return
    end if
!
!       check for mass, must be > 0
    if (sup(j,9) .le. 0._lrk) then
      errmsg = fomsgf(99, nmm,9,cop(1),0)
      return
    end if
!
  end do
!     Each flexible support represents one bearing housing. Multiple
!     SUPPORT rows for the same bearing are ambiguous and were formerly
!     order-dependent in the elastic-line calculation. Reject them.
  call chksupdup(ns,bn,errmsg,oki)
  if (oki .lt. 0) then
    if (.not. std) close(iu)
    return
  end if
!     optional dynamic FOUNDATION block follows SUPPORT in stdio
!     or in a dedicated combined input stream. Legacy EOF is valid.
  call foundation_read_control(iu,nomarq,errmsg,st)
  if (st .lt. 0) return
!
!     support kind of parameter
200 supr = cop(2)
  if (.not. std) close(iu)
!
!     return ok
  ok = 0
  return
!
!     error on open file
50 errmsg = fomsgf(99, nmm,1,nomarq,1)
  return
!
!     error on reading file
100 errmsg = fopmsf(99, nmm,3,per,nomarq)
  if (.not. std) close(iu)
  return
!
5 format(/,a)
15 format(/)
25 format(i10,9f10.0,a)
35 format()
!
end subroutine beasupf
!
!     =================================================================
!>    @brief validate one-to-one SUPPORT-to-BEARING association.
!
!>    Each SUPPORT row models a distinct bearing housing.  More than one
!>    row associated with the same bearing is ambiguous in the current
!>    topology and historically made the elastic-line result depend on
!>    input row order.
!
!>    @param[in] ns number of support rows
!>    @param[in] bn bearing number associated with each support
!>    @param[out] errmsg error message
!>    @param[out] ok zero if valid, negative if duplicated
subroutine chksupdup(ns,bn,errmsg,ok)
  implicit none
!
  integer :: ns, ok
  integer :: ii, jj
  integer :: bn(*)
  character(len=99) :: errmsg
!
  ok = -1
  do ii = 1,ns-1
    do jj = ii+1,ns
      if (bn(ii) .eq. bn(jj)) then
        write(errmsg,5) bn(ii)
        return
      end if
    end do
  end do
!
  ok = 0
  return
!
5 format('more than one SUPPORT associated with BEARING ',i3)
!
end subroutine chksupdup
!
!     =================================================================
!>    @brief copy bearing data to lateral vectors
!
!>    @param[in] dmr input bearing parameters vector
!>    @param[in,out] pc bearing position (m)
!>    @param[in,out] kxx xx stiffness vector (N/m)
!>    @param[in,out] kxz xz stiffness vector (N/m)
!>    @param[in,out] kzz zz stiffness vector (N/m)
!>    @param[in,out] kzx zx stiffness vector (N/m)
!>    @param[in,out] cxx xx damping vector (N/m/s)
!>    @param[in,out] cxz xz damping vector (N/m/s)
!>    @param[in,out] czz zz damping vector (N/m/s)
!>    @param[in,out] czx zx damping vector (N/m/s)
!>    @param[in,out] mm bearing mass >= 0 (kg)
!>    @param[in,out] nbrg current bearing index to copy to
!>    @param[in,out] mxm dimension of bearing parameter vectors
!
subroutine cplbdmr(dmr,pc,kxx,kxz,kzz,kzx,&
&cxx,cxz,czz,czx,mm,nbrg,mxm)
  use rd_kinds, only: lrk
  implicit none
!
  integer :: nbrg, mxm
  real(lrk) :: dmr, pc, kxx, kxz, kzz, kzx, cxx, cxz, czz, czx, mm
  dimension dmr(10),pc(mxm),kxx(mxm),kxz(mxm),kzz(mxm),kzx(mxm),&
  &cxx(mxm),cxz(mxm),czz(mxm),czx(mxm),mm(mxm)
!
!     Historical BEARING-row order is intentionally preserved here:
!       POS,KXX,KXZ,KZZ,KZX,CXX,CXZ,CZZ,CZX,WEIGHT.
!     Convert it explicitly into named storage; downstream routines pack
!     those names into the canonical matrix order xx,xz,zx,zz.
  integer :: ipos, ikxx, ikxz, ikzz, ikzx, icxx, icxz, iczz, iczx, iwgt
  parameter (ipos=1,ikxx=2,ikxz=3,ikzz=4,ikzx=5,&
  &icxx=6,icxz=7,iczz=8,iczx=9,iwgt=10)
!
  pc(nbrg) = dmr(ipos)
  kxx(nbrg) = dmr(ikxx)
  kxz(nbrg) = dmr(ikxz)
  kzz(nbrg) = dmr(ikzz)
  kzx(nbrg) = dmr(ikzx)
  cxx(nbrg) = dmr(icxx)
  cxz(nbrg) = dmr(icxz)
  czz(nbrg) = dmr(iczz)
  czx(nbrg) = dmr(iczx)
  mm(nbrg) = dmr(iwgt)
!
  return
!
end subroutine cplbdmr
!
!     =================================================================
!>    @brief copy torsion restriction bearing data to torsion vectors
!
!>    @param[in] dmr input bearing parameters vector
!>    @param[in,out] tpc torsion restriction position (m)
!>    @param[in,out] tkk torsion stiffness vector (Nm/rad)
!>    @param[in,out] tcc torsion damping vector (Nm/rad/s)
!>    @param[in,out] tjj torsion mass inertia < 0 input >0 output (kgm^2
!>    @param[in,out] ntbr current torsion restriction index to copy to
!>    @param[in,out] mxm dimension of torsion restriction bearing
!>     parameter vectors
subroutine cpltdmr(dmr,tpc,tkk,tcc,tjj,ntbr,mxm)
  use rd_kinds, only: lrk
  implicit none
!
  integer :: ntbr, mxm
  real(lrk) :: dmr, tpc, tkk, tcc, tjj
  dimension dmr(10),tpc(mxm),tkk(mxm),tcc(mxm),tjj(mxm)
!
  tpc(ntbr) = dmr(1)
!     stiffness kxx
  tkk(ntbr) = dmr(2)
!     damping cxx
  tcc(ntbr) = dmr(6)
!     inertia mm
  tjj(ntbr) = abs(dmr(10))
!
  return
!
end subroutine cpltdmr
!
!     ==================================================================
!>    @brief get gyroscopic factor for equivalente diamenter indices
!>     and values.input is a text vector with elements composed by
!>     an integer and the gyroscopic compensation factor both
!>     numbers separated by a slash. section number - gyroscopic
!>     compensation fator, index two digits value ten. index and
!>     value are check for less than zero and greater than maximum.
!
!>    @param[in] cs number of sections
!>    @param[in,out] ngycsed number of text elements at input,
!>     number of valid pairs at output.
!>    @param[in] mxspl dimension of input text vector
!>    @param[in] mxgc dimension of output index and values vectors
!>    @param[in] steln input text vector
!>    @param[out] gycsedid id of the section to apply gyroscopic factor.
!>    @param[out] gycsedvl gyroscopic factor value.
!
subroutine gycscid(cs,ngycsed,mxspl,mxgc,steln,gycsedid,gycsedvl)
  use rd_kinds, only: lrk
  implicit none
!
  real(lrk) :: gycsedvl
  integer :: cs, ngycsed, mxspl, mxgc, gycsedid
  character(len=*) :: steln
!
  real(lrk) :: dm, mx
  logical :: ok
  integer :: ii, jj, kk, ll, ios, splitf
  character(len=1) :: csl
  character(len=7) :: nmm
  character(len=10) :: cval
  dimension cval(2),steln(mxspl),gycsedid(mxgc),gycsedvl(mxgc)
  parameter(csl = '-',nmm = 'gycscid',mx = 5)
!
  intrinsic :: nint
!
  kk = 0
  do ii = 1,ngycsed
!       extract index from index-value -> cval
    jj = splitf(steln(ii),csl,2,cval)
!       check format
    if (jj .eq. 2) then
      do jj = 1,2
!           read index and factor value both as real
        read(cval(jj),5,iostat=ios) dm
!           check read status
        if (ios .ne. 0) exit
!           extract data
        if (jj .eq. 1) then
!             index
          ll = nint(dm)
!             check index bounds
          ok = ll .gt. 0 .and. .not. ll .gt. cs
          if (.not. ok) exit
          gycsedid(kk+1) = ll
        else if (jj .eq. 2) then
!             value
          ok = dm .gt. 0 .and. dm .le. mx
          if (.not. ok) exit
          gycsedvl(kk+1) = dm
        else
!             error, not supposed to be, stop
          call elmsge(1,8,nmm)
        end if
      end do
!         ok, increment index
      if (ios .eq. 0 .and. ok) kk = kk+1
    end if
  end do
!
!     number of valid data values
  ngycsed = kk
!
  return
!
5 format(f10.0)
!
end subroutine gycscid
!
!     =================================================================
!>    @brief check distributed force section id uniqueness.
!
!>    @param[in] icd current id
!>    @param[in] rid current section id
!>    @return true if current section id was previously defined.
!
logical function ckdsfrf(icd,rid)
  use com_unb1, only: ndd, mu, ed, tpf, nb
  use rd_kinds, only: lrk
  implicit none
!
  integer :: icd
  real(lrk) :: rid
!
!     unbalance common
  integer :: mxb
  parameter (mxb = 99)
!
!
  real(lrk) :: rai
  integer :: ii
  logical :: ok, rpeqf
!
  ok = .false.
  do ii = 1,nb
!       tpf = kind of force
!       -1:transient torque,0:unbalance (default),1:concentrated,
!       2:harmonic torque,3:static torque,4:freq. response,5:dist. force
    if (ii .ne. icd .and. tpf(ii) .eq. 5) then
!         section id
      rai = ndd(ii)
!         number inside real precision tolerance
      ok = rpeqf(rid,rai,1e-6_lrk)
      if (ok) exit
    end if
  end do
!
  ckdsfrf = ok
!
  return
!
end function ckdsfrf
!
!     =================================================================
!>    @brief main data input i/o.
!>     see optionsf for options and torsion data
!
!     ***********************
!     * ALL VARIABLES IN SI *
!     ***********************
!
!     l - length of the shaft
!
!     shaft sections
!
!     cs       number of shaft sections
!     d()      diameters
!     ps()     start position
!     e()      Young's module E
!     nu()     Poisson's ratio
!     rho_e()  density
!
!     disks
!     nd      number of disks
!     pd()    start position
!     d_d()   diameter
!     h()     length
!     rho_d() density
!
!     bearings
!     nc    number of bearings
!     pc()  start position
!     kxx() xx stiffness
!     kxz() xz stiffness
!     kzz() zz stiffness
!     kzx() zx stiffness
!     cxx() xx damping
!     cxz() xz damping
!     czz() zz damping
!     czx() zx damping
!     mm()  [mass]
!
!     campbell
!     nini    initial speed
!     nfin    final speed
!     dw      delta
!     v_maior number of div, largest length
!     v_menor number of divisoes minnor length
!
!     unbalance response
!     nini_r initial speed
!     nfin_r final speed
!     dw_r   delta speed
!     ndd()  number of unbalances
!     mu()   unbalance mass
!     e()    excentricidade
!     np     number of response positions
!     pr()   response positions
!     desp() displacement
!
!     modes
!     qtd_modos number of desired modes
!
!     concentrated masses and inertias
!     nmic  number of concentrated masses
!     psic() positions
!     vlmc() mass values
!     ixic() inertia x
!     iyic() inertia y
!     izic() inertia z
!
!     dimension of vectors
!
!     mxs max sections
!     mxd max disks
!     mxm max bearings
!     mxb max unbalances
!     mxr max responses
!
!>    @param[in] shp speed shape flag
!>    @param[out] nomopf return options file name
!>    @param[out] iex export kind flag = 0 HB, 1=mm,2=plain
!>    @param[out] sok return true if support was read in line.
!>    @param[out] frl true if plot vertical scale is logarithmic
!>    @param[out] fto true if flexural-torsion optional data is present
!>    @param[out] errmsg return a error message
!>    @param[out] ok return flag. unsuccessful if <0.
!
subroutine entrada(shp,nomopf,iex,sok,frl,fto,errmsg,ok)
  use rd_textfun, only: fomsgf, fopmsf
  use com_cab, only: name, descript
  use com_conc, only: nmic, psic, vlmc, ixic, iyic, izic
  use com_copcm, only: cop => ops
  use com_cpb, only: nini, nfin, dw, npi, ncc, imt
  use com_cpba, only: nrdc, rgini, rgrpm
  use com_cpbn, only: spdn
  use com_dflexi, only: dmodel, dfdofx, dfdofz, nfdisk, nshaftfd
  use com_dflexr, only: dkr, df1nd
  use com_dis, only: pd, d_d, h_d, rho_d, r2, nd
  use com_disa, only: d_i, i_x, i_y, m_d
  use com_doff, only: off_d
  use com_eix, only: l, d, di, ps, e, nu, rho_e, g_e, r1, cs
  use com_eix2, only: ys
  use com_eixa, only: divs, umps
  use com_eixshape, only: d2, di2, etype
  use com_fixstif, only: fxstf
  use com_gycsedc, only: gycsedid, gycsedvl, ngycsed
  use com_hang, only: hangle, accg, isf, ha
  use com_knd, only: cknd, supr
  use com_man, only: kxx, kxz, kzz, kzx, cxx, cxz, czz, czx, mm, scl => sc
  use com_mdd, only: qtd_modos, porb, rorb, rang
  use com_mdd1, only: ru
  use com_mfa, only: ma, rm, au
  use com_ppr, only: opt, ifn
  use com_pse, only: v_maior, v_menor
  use com_psea, only: ld_r
  use com_pst, only: wrl
  use com_sp1, only: ns, bn
  use com_sp2, only: sup, sps
  use com_sp3, only: kos
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
  character(len=99) :: errmsg
  character(len=255) :: nomopf
  integer :: iex, ok
  logical :: shp, sok, frl, fto
!
!     locals
  character(len=1) :: clb, cdl, ckd, csp
  character(len=2) :: cps
  character(len=4) :: cbn
  character(len=5) :: cst
  character(len=7) :: nmm
  character(len=8) :: csn, per
  character(len=80) :: dm
  character(len=60) :: opg
  character(len=255) :: opf, nomarq
!
  dimension clb(4),cbn(8),csn(15)
  parameter (clb = (/' ','(',')',','/),cdl = '$',ckd = '0',&
  &csp = 'S',cps = 'op', nmm = 'entrada', cst = 'stdio',&
  &cbn = (/'mxs ','mxd ','mxm ','mxc ',&
  &'mxr ','xmd ','mxic','mxgc'/),&
  &csn = (/'descript','section ','disk    ','bearing ','campbell',&
  &'excit.  ','time.out','resp.pos','calc.opt','conc.nrl',&
  &'conc.pos','supp.nrl','supp.dat','supp.idx','options '/))
!
  integer :: i, j, k, iu, oki, st, chkdirsf
!     added coptf logical function - francisco - feb-19
  logical :: ext, tors, pneg, coptf, ckdsfrf, foundation_active_f
!     rkm fixed stiffness, see linhael
  real(lrk) :: sc, dmr, rkm
  dimension dmr(10)
!     comma separated values
  integer :: mxspl, splitf
  parameter (rkm = 5e20_lrk,mxspl = 5)
  character(len=12) :: steln(mxspl)
!
!     input parameters
  integer :: mxs, mxd, mxm
  parameter (mxs = 99,mxd = 99,mxm = 9)
!
!     shaft data
!     native cylindrical/conical and solid/hollow geometry
!     added yield strength - francisco - dec-19
!     see blockd.f
!     added gyroscopic section compensation for equivalent diameters
!     franciscp - aug-21
  integer :: mxgc
  parameter (mxgc = 5)
!
!     umbalanced magnetic pull
!
!     section parameters
!     angle unit r -> radians, default degree
!     bending stress median filter width
!     horizontal angle and gravity
!
!     bearings
!     scale, scl added francisco - oct-15
!     nbrg -> number of bearings
!     rks -> use bearing stiffness on static deflection flag,
!     nominal speed pu, added francisco oct-20
!     added on 13/04/2007
!     pc -> position of bearings
!     added - francisco dec-21
!     fixed stiffnes for no displacement
!     added - francisco - oct-15
!     torsion restriction bearings, added mar-21
!
!     disks
!     added disk offset - francisco - feb-19
!     flexible disk input data
!
!     campbell
!     mxc -> max number of campbell frequencies
  integer :: mxc
  parameter (mxc = 19)
!     nominal speed (rpm), added francisco oct-20
!
!     undamped critical speed map
!
!     unbalance
  integer :: mxr, mxb
  parameter (mxr = 9, mxb = 99)
!
!     ori -> radians
!     kind of exc, see expgeo.f
!     min amplification factor fator, modal rpm, angle unit block
!     angle unit r -> radians, default degree
!     torsion harmonic excitation frequency (rad/s)
!     or force offset for kind = 1 (m)
!
!     modes
  integer :: xmd
!     max number of modes
  parameter (xmd = 19)
!     time orbit position
!     response angle unit 'd' or 'D' for degree, default radian
!     time response angle unit 'r' or 'R' for radian, default degree
!
!     concentrated masses and inertias
  integer :: mxic
  parameter (mxic = 15)
!
!     bearing support
!     number of parameters lines
!     bearing number
!     description of support
!     added scale sps - francisco - nov-15
!
!     header
!
!     calculation options
!
!     bearing calculation kind
!
!     parameters
!     see init.f blockd.f
  integer :: mop
  parameter (mop = 20)
!     please checkout the init loop
!
!     work folder
!
!     intrinsic functions
  intrinsic :: abs, index, len, len_trim, min, nint
!
!     return init
  ok = -1
!     reset optional foundation state
  call foundation_reset()
!     reset optional true-transient input state (CLI request is separate
  call transient_reset()
!
!     support data flags
  supr = clb(1)
  sok = .false.
!     torsion flag
  tors = .false.
!     gyroscopic compensation buffer
  opg = ' '
!
  if (.not. opt(11)) then
!       file i/o
!       input unit number (file)
    iu = 10
!       input file name
    nomarq = ifn
!       check for input file
!       input file name string length space striped
    i = len_trim(nomarq)
    inquire(file = nomarq(1:i),exist=ext)
    if (.not. ext) then
!         file not found
      errmsg = fomsgf(99, nmm,2,nomarq,1)
      return
    end if
!       open input file
    open(iu,file=nomarq(1:i),err=50)
  else
!       standard i/o
    iu = 5
!       open standard input
    nomarq = cst
    open(iu,err=50)
!       override messages
    if (opt(17)) call loadmsg(iu,.true.)
  endif
!
!     reading input data
!     id
  per = csn(1)
  read(iu,15,end=100,err=100) name
  read(iu,300,end=100,err=100) descript
!
!     sections, angle, gravity
  per = csn(2)
  read(iu,25,end=100,err=100)&
  &cs,l,v_menor,v_maior,ld_r,hangle,accg,dm
!     angle unit
  ha = dm(1:10)
!     check beyond
  k = len_trim(dm(11:))
  if(k .gt. 0) then
!       try ti read both values
    read(dm(11:),*,err=60,end=60) isf(1),isf(2)
!       ok, jump
    goto 60
!       try to read only one value
60  read(dm(11:),*,err=70,end=70) isf(1)
!       same value for both
    isf(2) = isf(1)
!       go ahead
70  continue
  end if
!
!     bound check
  if (cs .gt. mxs) then
    close(iu)
!       'out of bounds' !4
    errmsg = fomsgf(99, nmm,4,cbn(1),0)
    return
  end if
!
  read(iu,250,end=100)
!
  do j = 1,cs
!
    read(iu,35,end=100,err=100)&
    &ps(j),d(j),di(j),e(j),nu(j),rho_e(j),divs(j),umps(j),dm
!       Legacy geometry is cylindrical. Optional tail after YSTR adds:
!       ETYPE, final external diameter and final internal diameter.
!       1 solid cylindrical, 2 solid conical,
!       3 hollow cylindrical, 4 hollow conical.
    ys(j) = 0
    d2(j) = d(j)
    di2(j) = di(j)
    if (di(j) .gt. 0._lrk) then
      etype(j) = 3
    else
      etype(j) = 1
    end if
    k = len_trim(dm)
    if (k .gt. 0) read(dm,350,err=80,end=80) ys(j)
    if (k .gt. 10) then
      st = 0
      read(dm(11:),*,iostat=st) etype(j),d2(j),di2(j)
!         Ignore an unrecognized legacy tail and retain cylindrical defa
      if (st .ne. 0) then
        d2(j) = d(j)
        di2(j) = di(j)
        if (di(j) .gt. 0._lrk) then
          etype(j) = 3
        else
          etype(j) = 1
        end if
      end if
    end if
!       Basic geometry validation before data preparation.
    if (etype(j) .lt. 1 .or. etype(j) .gt. 4 .or.d(j) .le. 0._lrk .or. d2(j) .le. 0._lrk .or.di(j) .lt. 0._lrk .or. &
      & di2(j) .lt. 0._lrk .or.di(j) .ge. d(j) .or. di2(j) .ge. d2(j)) then
      errmsg = fomsgf(99, nmm,6,per,0)
      return
    end if
    if ((etype(j) .eq. 1 .or. etype(j) .eq. 2) .and.(di(j) .gt. 0._lrk .or. di2(j) .gt. 0._lrk)) then
      errmsg = fomsgf(99, nmm,6,per,0)
      return
    end if
    if ((etype(j) .eq. 3 .or. etype(j) .eq. 4) .and.(di(j) .le. 0._lrk .or. di2(j) .le. 0._lrk)) then
      errmsg = fomsgf(99, nmm,6,per,0)
      return
    end if
    if ((etype(j) .eq. 1 .or. etype(j) .eq. 3) .and.(abs(d2(j)-d(j)) .gt. 1e-9_lrk .or.abs(di2(j)-di(j)) .gt. &
      & 1e-9_lrk)) then
      errmsg = fomsgf(99, nmm,6,per,0)
      return
    end if
    if ((etype(j) .eq. 2 .or. etype(j) .eq. 4) .and.abs(d2(j)-d(j)) .le. 1e-9_lrk .and.abs(di2(j)-di(j)) .le. &
      & 1e-9_lrk) then
      errmsg = fomsgf(99, nmm,6,per,0)
      return
    end if
!
80 end do
!
!     disks, [rated speed,section-gyroscopic compensations]
  per = csn(3)
  read(iu,195,end=100,err=100) nd,opf
!     buffer size
  k = len_trim(opf)
!     check optional data
  if (k .eq. 0) then
!       could not get the two values, read only number of disks
    spdn = 0
  else
!       try read rated speed and buffer [section-gyroscopic compensation
    read(opf,205,iostat=k) spdn,opg
!       if io status then just rated speed?
    if (k .ne. 0) then
      read(opf,*,err=100,end=100) spdn
    else
!         buffer size
      k = len_trim(opg)
!         check buffer size
      if (k .gt. 0) then
!           get coma separated data -> steln
        j = splitf(opg,clb(4),mxspl,steln)
        if (j .gt. mxgc) then
!             more data than vector bound
          close(iu)
!             'out of bounds' !4
          errmsg = fomsgf(99, nmm,4,cbn(8),0)
          return
        else
!             bound ok, get section index-gyr. factor->gycsedid,gycsedvl
          call gycscid(cs,j,mxspl,mxgc,steln,gycsedid,gycsedvl)
          ngycsed = j
        end if
!           opg buffer check
      end if
!         i/o status check
    end if
!       opf buffer size if
  end if
!
!     check for vector bound
  if (nd .gt. mxd) then
    close(iu)
!       'out of bounds' !4
    errmsg = fomsgf(99, nmm,4,cbn(2),0)
    return
  end if
!
!     blank line
  read(iu,250,end=100)
!     disk loop
  do j = 1,nd
!
    read(iu,45,end=100,err=100)&
    &pd(j),d_d(j),h_d(j),rho_d(j),&
    &d_i(j),i_x(j),i_y(j),m_d(j),dm
!
!       disk offset plus optional flexible-disk definition.
!       Legacy: OFFSET
!       New:    OFFSET RIGID
!               OFFSET FLEXIBLE KR value
!               OFFSET FLEXIBLE F1ND value
    off_d(j) = 0
    dmodel(j) = 0
    dkr(j) = 0._lrk
    df1nd(j) = 0._lrk
    call flexdisk_parse_option(dm,j,off_d(j),errmsg,oki)
    if (oki .lt. 0) return
!       disk prop read loop end
!       check position
    if (pd(j) .lt. 0 .or. pd(j) .gt. l) then
!         'invalid option' !6
      errmsg = fomsgf(99, nmm,6,per,0)
      return
    end if
110 end do
  call flexdisk_recount()
!
!     bearings
  per = csn(4)
!     number of lateral bearings
  nbrg = 0
!     number of torsion restriction bearings
  ntbr = 0
!     number of lines, optional scale
!     bearing stiff on st. deflect - added oct-20 - francisco
  read(iu,300,err=100,end=100) dm
!     try read total number of bearings "i", bearing props scale,rks fla
  read(dm,*,iostat=st) i,sc,rks,fxstf
!     check for read status
  if (st .ne. 0) then
!       assume fixed support stiffness value, see linhael
    fxstf = rkm
!       try read total number of bearings "i", bearing props scale and r
    read(dm,*,iostat=st) i,sc,rks
    if (st .ne. 0) then
!         could not read three values, set rks to zero and try two
      rks = 0
!         try read total number of bearings "i", bearing props scale
      read(dm,*,iostat=st) i,sc
      if (st .ne. 0) then
!           could not read two values, set sc to one
        sc = 1.0_lrk
!           should have at least total number of bearings "i"
        read(dm,85,err=100,end=100) i
      end if
    end if
  end if
!
!     check number of bearings
  if (i .gt. mxm .or. i .lt. 0) then
    close(iu)
!       check total number of bearings "i" for vector bound
!       'out of bounds' !4
    if (i .gt. mxm) errmsg = fomsgf(99, nmm,4,cbn(3),0)
!       check numbet less than zero
!       'invalid option' !6
    if (i .lt. 0) errmsg = fomsgf(99, nmm,6,per,0)
    return
  end if
!     blank line
  read(iu,250,end=100)
!     bearing loop, total number of bearings "i"
!     set scale
  scl = sc
  sct = sc
  do j = 1,i
!       read 10xdmr, last is mass + 1xdm
!       added last char value - francisco 20/10/2015
    read(iu,55,end=100,err=100) (dmr(k),k=1,10),dm
!       check position
    if (dmr(1) .lt. 0 .or. dmr(1) .gt. l) then
!         'invalid option' !6
      errmsg = fomsgf(99, nmm,6,per,0)
      return
    end if
!       check for bearing mass dmr(10):
!       if >= 0 -> lateral standard bearing
!       if < 0 -> torsion restriction
    if (dmr(10) .ge. 0) then
!         lateral standard bearing -> mm>=0
      nbrg = nbrg+1
      call cplbdmr(dmr,pc,kxx,kxz,kzz,kzx,&
      &cxx,cxz,czz,czx,mm,nbrg,mxm)
!         added rotational data kphi and ktheta - francisco 20/10/2015
      kph(j) = 0._lrk
      kth(j) = 0._lrk
      k = len_trim(dm)
!         try to read as 2 x f10.0, skip if not
      if (k .gt. 0) read(dm,65,end=120,err=120) kph(j),kth(j)
    else
!         torsion restriction bearing -> mm<0
      ntbr = ntbr+1
      call cpltdmr(dmr,tpc,tkk,tcc,tjj,ntbr,mxm)
    end if
!       bearing prop loop end
120 end do
!
!     campbell
!     added dm, get mode track flag
  per = csn(5)
  read(iu,75,end=100,err=100)&
  &nini,nfin,dw,ncc,npi,rgini,nrdc,rgrpm,dm
!     check dm for mode track flag
  imt = 0
  j = len_trim(dm)
  if (j .gt. 0) read(dm,85,end=130,err=130) imt
!     anything else zero
  if (imt .ne. 0) imt = 1
!
!     check the max number of campbell frequencies
130 if (ncc .gt. mxc) then
    close(iu)
!       'out of bounds' !4
    errmsg = fomsgf(99, nmm,4,cbn(4),0)
    return
  end if
!
!     unbalance / force
  per = csn(6)
  read(iu,95,end=100,err=100) nb,nini_r,nfin_r,dw_r,j,dm
!     unbalance response amplitude log plot => number of modes < 0
  frl = j .lt. 0
!     check number of modes =0
  if (j .eq. 0) then
!       'invalid option' !6
    errmsg = fomsgf(99, nmm,6,per,0)
    return
  end if
!     number of modes (complex -> 2x)
  nm = 2*abs(j)
!
!     min amplification factor fator and modal rpm block
!     additional MINAMP and MDRPM read, optional
!     au = excitations angle unit r -> radian, default degree
  j = len_trim(dm)
!     intial values set on blockd.f
  if (j .gt. 0) read(dm,105,end=150,err=150) ma,rm,au
!     number of modes (complex -> 2x)
150 if (nb .gt. mxb) then
    close(iu)
!       'out of bounds' !4
    errmsg = fomsgf(99, nmm,4,cbn(5),0)
    return
  end if
!
  read(iu,250,end=100)
!
  do j = 1,nb
!       unbalance (default)
    tpf(j) = 0
!       ndd = position, mu = unbalance, ed = phase
!       tpf = kind of force
!       -1:transient torque,0:unbalance (default),1:concentrated,
!       2:harmonic torque,3:static torque,4:freq. response,5:dist. force
    read(iu,115,end=100,err=100) ndd(j),mu(j),ed(j),tpf(j),dm
!       check kind
    if (tpf(j) .lt. -1 .or. tpf(j) .gt. 5) then
!         'invalid option' !6
      errmsg = fomsgf(99, nmm,6,per,0)
      return
    end if
!
!       check for distributed force section uniqueness
    if (ckdsfrf(j,ndd(j))) then
!         'already defined' !35
      errmsg = fomsgf(99, nmm,35,per,0)
      return
    end if
!
!       possible section index
    k = nint(ndd(j))
    if ( (tpf(j) .eq. 5 .and.&
!         check index for dist. force
    &(k .lt. 1 .or. k .gt. cs)) .or.&
    &(tpf(j) .ne. 5 .and.&
!         check position not dist. force
    &(ndd(j) .lt. 0 .or.ndd(j) .gt. l)) ) then
!         'invalid option' !6
      errmsg = fomsgf(99, nmm,6,per,0)
      return
    end if
!       added torsion harmonic exc.freq. (rad/s) - francisco feb-20
    k = len_trim(dm)
    if (k .gt. 0) read(dm,350,end=175,err=175) thfr(j)
175 end do
!
!     responses
  per = csn(7)
  read(iu,125,end=100,err=100)&
  &np,qtd_modos,porb,rorb,rang,dm
!     orient response angle unit 'r' or 'R' for radian, default degree
!     time response angle unit 'r' or 'R' for radian, default degree
  k = len_trim(dm)
  if (k .gt. 0) then
!       try read two "a10"
    read(dm,135,iostat=st) ru(1),ru(2)
!       if not, read one "a"
    if (st .ne. 0) read(dm,5,err=185) ru(1)
  end if
!
185 read(iu,250,end=100)
!
!     check porb
  if (porb .gt. l) then
!       'invalid option' !6
    errmsg = fomsgf(99, nmm,6,per,0)
    return
  end if
!     check if porb = 0 -> speed/shape
  shp = porb .eq. 0
!     check number of modes <0
  if (qtd_modos .lt. 1) then
!       'invalid option' !6
    errmsg = fomsgf(99, nmm,6,per,0)
    return
  end if
!     check bounds
  if (qtd_modos .gt. xmd .or. np .gt. mxr) then
    close(iu)
    if (qtd_modos .gt. xmd) then
!         check max number of modes
!         'out of bounds' !4
      errmsg = fomsgf(99, nmm,4,cbn(6),0)
    else
!         'out of bounds' !4
      errmsg = fomsgf(99, nmm,4,cbn(5),0)
    end if
    return
  end if
!     unbalance positions
  pneg = .false.
  per = csn(8)
  do j = 1,np
    read(iu,145,end=100,err=100) pr(j),desp(j),ori(j)
!       check suport position later
    if (.not. pneg) pneg = pr(j) .lt. 0
!       check position larger than length
    if (pr(j) .gt. l) then
!         'invalid option' !6
      errmsg = fomsgf(99, nmm,6,per,0)
      return
    end if
  end do
!
!     calculation options
!     kind of calculation [optional]
  cknd = ckd
!
!     check for the options file
  opf = clb(1)
  per = csn(9)
  read(iu,300,end=200,err=100,iostat=st) opf
!
!     added check for local folder - francisco feb-19
  j = chkdirsf(opf)
  k = len_trim(opf)
!     should not include directory characters and not contain
!     only valid calculation options
  if ((j .eq. 0).and.(k .gt. 0).and.(.not. coptf(opf,iex))) then
    j = len_trim(wrl)
!       option file name
    opf = wrl(1:j)//opf
  end if
!
!     concentrated masses and inertias.
!     for compatibility with old input
!     masks EOF should not be an error
  per = csn(10)
  read(iu,450,end=200,err=100) nmic
  if (nmic .gt. mxic) then
    close(iu)
!       'out of bounds' !4
    errmsg = fomsgf(99, nmm,4,cbn(7),0)
    return
  end if
!
  read(iu,250,end=100,err=100)
  per = csn(11)
  do j = 1,nmic
!       mass position, ix iy iz
    read(iu,155,end=100,err=100,iostat=st)&
    &psic(j),vlmc(j),ixic(j),iyic(j),izic(j)
!       check position
    if (psic(j) .lt. 0 .or. psic(j) .gt. l) then
!         'invalid option' !6
      errmsg = fomsgf(99, nmm,6,per,0)
      return
    end if
  end do
!
!     initialize io status
  st = 0
!     jump to standard input else try read support data
!     for standard input support file should come at last.
!     opt(11) standard i/o
  if (opt(11)) goto 200
!
!     support data. may be read again.
!     for compatibility with old input
!     masks EOF should not be an error.
  per = csn(12)
  read(iu,300,err=100,end=200) dm
  read(dm,*,iostat=st) ns,sps
  if (st .ne. 0) then
    sps = 1.0_lrk
    read(dm,*,err=100,end=100) ns
  end if
  if (ns .gt. mxm .or. ns .gt. nbrg) then
    close(iu)
    if (ns .gt. mxm) then
      !       'out of bounds' !4
      errmsg = fomsgf(99, nmm,4,cbn(3),0)
    else
!         number of supports greather than bearing count
      errmsg = fomsgf(99, nmm,5,csn(14),0)
    end if
    return
  end if
  per = csn(13)
  read(iu,250,end=100)
!
  do j = 1,ns
!       #nbr,kxx,kxz,kzz,kzx,cxx,cxz,czz,czx,mass,desc
    read(iu,165,end=100,err=100,iostat=st)&
    &bn(j),sup(j,1),sup(j,2),sup(j,4),sup(j,3),&
    &sup(j,5),sup(j,6),sup(j,8),sup(j,7),&
    &sup(j,9),kos(j)
!       check for mass, must be > 0
    if (sup(j,9) .le. 0._lrk) then
      close(iu)
!         'support without mass' !9
      errmsg = fomsgf(99, nmm,9,clb(1),0)
      return
    end if
!       check for bearing number range
    if (bn(j) .le. 0 .or. bn(j) .gt. nbrg) then
      close(iu)
!         'invalid data ' !8
      errmsg = fomsgf(99, nmm,8,csn(14),0)
      return
    end if
!       end do input data
  end do
!
!     Reject ambiguous duplicate support-to-bearing associations.
  call chksupdup(ns,bn,errmsg,oki)
  if (oki .lt. 0) then
    close(iu)
    return
  end if
!
!     optional dynamic FOUNDATION block after inline SUPPORT data.
  call foundation_read_control(iu,nomarq,errmsg,oki)
  if (oki .lt. 0) return
!
!     check for negative support positions
  if (pneg) then
    call chkspps(mxm,mxr,np,ns,bn,pr,oki,errmsg)
    if (oki .lt. 0) return
  end if
!
!     will consider supports, see parmanv.f, prpkc and matrizes.f
  if (opt(10)) supr = csp
!
!     support data ok, do not try to read later.
  sok = .true.
!
!     end of file bypass (this is fortran stuff)
200 continue
!
!     check iostat from options line
  if (st .ne. 0) goto 100
!
!     options file
  if (.not. opt(11)) then
!       means not standard i/o
    i = len_trim(opf)
!       check if opf is a file, existing file name
    inquire(file = opf(1:i),exist = ext)
    if (.not. ext) then
!         normal calculation options
      k = min(len(cop),i)
      cop = opf(1:k)
!         added check for invalid options
!         iex -> default export
      if (.not. coptf(cop,iex)) then
!           'invalid option' !6
        errmsg = fomsgf(99, nmm,6,csn(15),0)
        return
      end if
      nomopf = clb(1)
    else
!         options file
      call optionsf(opf,cs,opt(11),iex,tors,fto,errmsg,oki)
      if (oki .lt. 0) return
!         return option file name
      nomopf = opf
    end if
  else
!       standard i/o opt(11) = true
!       this should indicate an option file
    i = index(opf,cps)
    if (i .gt. 0) then
      nomopf = cst
      call optionsf(opf,cs,opt(11),iex,tors,fto,errmsg,oki)
      if (oki .lt. 0) return
    else
      k = min(len(cop),i)
!         normal calculation options
      cop = opf(1:k)
!         added check for invalid options
!         iex -> default export
      if (.not. coptf(cop,iex)) then
        errmsg = fomsgf(99, nmm,6,csn(15),0)
        return
      end if
      nomopf(1:1) = clb(1)
    end if
  end if
!
!     torsional options file
!     check init loop - nov-19 - francisco
  opt(18) = tors
!
!     support data not ok
  if (opt(10) .and. .not. sok) then
!       means not standard i/o
    if (.not. opt(11)) then
      call mntfnm(10,i,opf)
    else
!         stdio filename
      opf = cst
    end if
    call beasupf(opf,opt(11),errmsg,oki)
    if (oki .lt. 0) return
!       check for negative support positions
    if (pneg) then
      call chkspps(mxm,mxr,np,ns,bn,pr,oki,errmsg)
      if (oki .lt. 0) return
    end if
!
  end if
!
!     FOUNDATION can also connect directly to BEARING when no flexible
!     SUPPORT is present.  In that case no beasupf() call occurs, so
!     scan the remaining input stream here for the optional block.
  if (.not. foundation_active_f()) then
    call foundation_read_control(iu,nomarq,errmsg,oki)
    if (oki .lt. 0) return
  end if
!     TRANSIENT is an optional tail block and is deliberately independen
!     from the historical time/orbit option -t.
  call transient_read_control(iu,errmsg,oki)
  if (oki .lt. 0) return
!     The GUI/user chooses the response domain in input.txt through
!     LATERAL_RESPONSE_MODE.  If absent, preserve all legacy option/CLI
!     semantics exactly.
  call transient_apply_analysis_mode(opt,mop,errmsg,oki)
  if (oki .lt. 0) return
!
!     closing input file
  close(iu)
!
!     return ok
  ok = 0
  return
!
!     file open error
50 errmsg = fomsgf(99, nmm,1,nomarq,1)
  return
!
!     reading error
100 errmsg = fopmsf(99, nmm,3,per,nomarq)
  if (.not. opt(11)) close(iu)
  return
!
5 format(a)
15 format(/,a)
25 format(//,i10,f10.0,2i10,3f10.0,a)
35 format(6f10.0,i10,1f10.0,a)
45 format(8f10.0,a)
55 format(10f10.0,a)
65 format(2f10.0)
75 format(//,3f10.0,2i10,f10.0,i10,f10.0,a)
85 format(i10)
95 format(//,i10,3f10.0,i10,a)
105 format(2f10.0,a)
115 format(3f10.0,i10,a)
125 format(//,2i10,3f10.0,a)
135 format(2a10)
145 format(3f10.0)
155 format(5f10.0)
165 format(i10,9f10.0,a)
195 format(//,i10,a)
205 format(f10.0,a)
250 format(/)
300 format(//,a)
350 format(f10.0)
450 format(//,i10)
!
end subroutine entrada
!
