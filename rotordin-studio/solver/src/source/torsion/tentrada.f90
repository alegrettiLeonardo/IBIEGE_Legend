!     $Id$
!     ==================================================================
!
!>    @file tentrada.f
!>    @brief torsional data input.
!>    last changes:<br>
!>    new file nov-19 - f
!>    added equivalent stiffness for static torque - francisco - oct-20
!>    moved splitf to tmatfun - franciso - apr-21<br>
!>    added torsion and bend stress factors on input - francisco - jul-2
!>    added cs number of section argument on tentrada - francisco - aug-
!>    added cs number of section argument on teqstid - francisco - aug-2
!>    added iex export job index on entrada, optionsf, francisco - nov-2
!
!     ==================================================================
!>    @brief get static equivalent stiffness definition indices.
!>     input is a text vector with elements composed by two integers
!>     numbers separated by a slash. torsion bearing number - torque
!>     excitation number. Each element can have up to two digits.
!
!>    @param[in] cs number of sections
!>    @param[in,out] ntest number of text elements at input,
!>     number of valid pairs at output.
!>    @param[in] mxspl dimension of input text vector
!>    @param[in] mxtstf dimension of output pairs matrix
!>    @param[in] steln input text vector
!>    @param[out] eqstid output pairs matrix.
!
subroutine teqstid(cs,ntest,mxspl,mxtstf,steln,eqstid)
  implicit none
!
  integer :: cs, ntest, mxspl, mxtstf, eqstid
  character(len=*) :: steln
!
  logical :: ok
  integer :: ii, jj, kk, ll, ios, splitf
  character(len=1) :: csl
  character(len=2) :: cind
  dimension steln(mxspl),cind(2),eqstid(mxtstf,2)
  parameter(csl = '-')
!
  kk = 0
  ll = 0
  do ii = 1,ntest
!       extract index from i1-i2 -> cind
    jj = splitf(steln(ii),csl,2,cind)
!       check format i1-i2
    if (jj .eq. 2) then
      do jj = 1,2
        read(cind(jj),5,iostat=ios) ll
!           check index bounds
        ok = ll .gt. 0 .and. .not. ll .gt. cs .and. ios .eq. 0
        if (.not. ok) exit
        eqstid(kk+1,jj) = ll
      end do
!         ok, increment index
      if (ok) kk = kk+1
    end if
  end do
!     number of valid data values
  ntest = kk
!
  return
!
5 format(i10)
!
end subroutine teqstid
!
!     ==================================================================
!>    @brief torsion-flexural optional data
!
!>    @param[in] ip input file handle
!>    @param[in] nomarq input file name
!>    @param[out] to optional flexural-torsion data is present
!>    @param[out] errmsg return a error message
!>    @param[out] ok return flag. unsuccessful if <0.
!
subroutine topfltr(ip,nomarq,to,errmsg,ok)
  use rd_textfun, only: fomsgf, fopmsf
  use com_tflxtor, only: siv, nsi, nki, sfv, ftv, scy, tvl, kvl, nsc, nks, nnt
  use rd_kinds, only: lrk
  implicit none
!
!     arguments
  logical :: to
  character(len=99) :: errmsg
  character(len=255) :: nomarq
  integer :: ip, ok
!
!     splitf = split function
  integer :: i, j, nl, ios, splitf
  real(lrk) :: dm
  character(len=1) :: blank, ccm
  character(len=6) :: cpn, csn, per
  character(len=7) :: nmm
!     message functions
  character(len=125) :: lbf
  character(len=255) :: buf
  dimension csn(5)
  parameter(blank = ' ',ccm = ',',nmm = 'topfltr',&
  &cpn = 'mxscls',&
  &csn = (/'fatig.','s.utls','s.fnsh','kytqnt','keyvls'/))
!
!     coma separated values
  integer :: mxspl
  parameter (mxspl = 25)
  character(len=10) :: steln(mxspl)
!
!     flexural-trosion optional data
!     see tblockd.f
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
  intrinsic :: len_trim
!
  ok = -1
!
!     empty line and field descriptions
  read(ip,5,iostat=ios) buf
!     check for optional data
  if (ios .eq. 0) then
!       check for optional data field descriptions
    if (len_trim(buf) .eq. 0) then
!         no option data, ok
      ok = 0
      return
    else
!
!         ALL optional data is considered present
!
!         fatigue values and section list
!
      per = csn(1)
!     FTCNP     FTNBCY    FTSTPK    FTASM     SCLST
      read(buf,15,err=100,end=100) (ftv(i),i=1,4),lbf
!         split sections list -> steln
      nl = splitf(lbf,ccm,mxspl,steln)
!         check maximal number of sections
      if (nl .gt. mxscls) then
        close(ip)
        errmsg = fomsgf(99, nmm,4,cpn,0)
        return
      end if
!         convert to integer
      do i = 1,nl
        read(steln(i),*,end=100,err=100) j
        siv(i) = j
      end do
      nsc = nl
!
    end if
!
  else
!       no option data, ok
    ok = 0
    return
  end if
!
!     now it is supposed that all optional data is present and correct
!
!     ultimate strenght list
!
  per = csn(2)
!     SCUSTL
  read(ip,25,err=100,end=100) lbf
!     split sections ult strength list -> steln
  nl = splitf(lbf,ccm,mxspl,steln)
!     check maximal number of sections
  if (nl .gt. mxscls) then
    close(ip)
    errmsg = fomsgf(99, nmm,4,cpn,0)
    return
  end if
!     should have same number of sections to check
  if (nl .ne. nsc) then
    errmsg = fomsgf(99, nmm,7,blank,0)
    return
  end if
!     convert to real
  do i = 1,nl
    read(steln(i),*,end=100,err=100) dm
    scy(i) = dm
  end do
!
!     surface finishings
!
  per = csn(3)
!     FTSCSF
  read(ip,25,err=100,end=100) lbf
!     split sections finishing list -> steln
  nl = splitf(lbf,ccm,mxspl,steln)
!     check maximal number of sections
  if (nl .gt. mxscls) then
    close(ip)
    errmsg = fomsgf(99, nmm,4,cpn,0)
    return
  end if
!     should have same number of sections to check
  if (nl .ne. nsc) then
    errmsg = fomsgf(99, nmm,7,blank,0)
    return
  end if
!     convert to integer
  do i = 1,nl
    read(steln(i),*,end=100,err=100) j
    sfv(i) = j
  end do
!
!     keys,torque,notches
!
  per = csn(4)
!     KEYS      ALTQP     MXTQP     TQSTF     BDSTF     NTIND
  read(ip,35,err=100,end=100) nks,(tvl(i),i=1,4),lbf
!     check maximal number of sections
  if (nks .gt. mxscls) then
    close(ip)
    errmsg = fomsgf(99, nmm,4,cpn,0)
    return
  end if
!     split notches list -> steln
  nl = splitf(lbf,ccm,mxspl,steln)
!     check maximal number of sections
  if (nl .gt. mxscls) then
    close(ip)
    errmsg = fomsgf(99, nmm,4,cpn,0)
    return
  end if
!     convert to integer
  do i = 1,nl
    read(steln(i),*,end=100,err=100) j
    nsi(i) = j
  end do
  nnt = nl
!
!     key geometry values
!
  per = csn(5)
!     blank and names
  read(ip,5,err=100,end=100) lbf
!     KIDX      DIST      HEIGHT    LENGTH    RF        WIDTH     RG
  do i = 1,nks
    read(ip,45,err=100,end=100) nki(i),(kvl(i,j),j=1,6)
  end do
!
  to = .true.
  ok = 0
!
  return
!
!     composed error message
100 errmsg = fopmsf(99, nmm,3,per,nomarq)
  return
!
5 format(/,a)
15 format(4f10.0,a)
25 format(//,a)
35 format(//,i10,4f10.0,a)
45 format(i10,6f10.0)
!
end subroutine topfltr
!
!     ==================================================================
!>    @brief torsional data file input.
!>     see topfltr for flexural-torsion optional data
!
!>    @param[in] ip input file handle
!>    @param[in] nomarq input file name
!>    @param[in] cs number of sections
!>    @param[out] iex export kind flag = 0 HB, 1=mm,2=plain
!>    @param[out] to optional flexural-torsion data is present
!>    @param[out] errmsg return a error message
!>    @param[out] ok return flag. unsuccessful if <0.
!
subroutine tentrada(ip,nomarq,cs,iex,to,errmsg,ok)
  use rd_textfun, only: femsgf, fomsgf, fopmsf
  use com_copcm, only: ops
  use com_couplings, only: cplgid, cplgst, cplgdp, cplgin, cplgsr, ncplg
  use com_fillets, only: ntfil, tfilid, tfilrd
  use com_mdamps, only: tdmrt, tdmmd, ntdmp
  use com_mdampso, only: mdoff
  use com_tcpbl, only: tcpn, tsmg, nteln
  use com_tcpbl1, only: teln
  use com_tmdn, only: tmdnb, ntmdn
  use com_touopt, only: telm, tlopt
  use com_tpfrt, only: fprt, fprts
  use com_tshyi, only: tshyipu
  use com_tstest, only: eqstid, ntest
  use com_ttraneq, only: tequt
  use com_ttransc, only: ttris, ttrrs, ttrit, ttret
  use com_ttransd, only: ttrmd, ttrdtt, ttrdta, nttra, ttrdsz, iseqt
  use rd_kinds, only: lrk
  implicit none
!
!     arguments
  logical :: to
  character(len=99) :: errmsg
  character(len=255) :: nomarq
  integer :: ip, cs, iex, ok
!
!     locals
  integer :: ist
  character(len=1) :: ccm
  character(len=6) :: csn, cpn
  character(len=8) :: nmm, per
  dimension csn(6),cpn(8)
  parameter (ccm = ',',nmm = 'tentrada',&
  &csn = (/'t.camp','t.coup','t.damp','t.fill','t.tran','t.opts'/),&
  &cpn = (/'mxteln','mxcplg','mxtmdn','mxtdmp','mxtfil','mxttra',&
  &'mxttrd','mxtstf'/))
!     message functions
  character(len=255) :: buf, bf1
  real(lrk) :: dm
!
!     comma separated values
  integer :: mxspl
  parameter (mxspl = 15)
  character(len=5) :: steln(mxspl)
!
!     splitf = split function
  integer :: i, j, k, splitf
!     options processing function, see entrada.f
  logical :: coptf
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
!     logical output options lof and radian
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
!     equivalent stiffness for static
  integer :: mxtstf
  parameter (mxtstf = 5)
!
!     transient
  integer :: mxttra, mxttrd
  parameter (mxttra = 5,mxttrd = 10000)
!     number of transients, number of data points
!     integration step, result step, integration init,integration end
!     damping,time,torque amplitude
!     torque equation
!
!     calculation options
!
  intrinsic :: abs, len_trim, nint
!
!     return init
  ok = -1
  to = .false.
!
!     campbell
!     section id for error report
  per = csn(1)
!     CPNOM  CPSM [CPSP1,CPSP2,...]
  read(ip,5,end=100,err=100) tcpn,tsmg,buf
  i = len_trim(buf)
  if (i .gt. 0) then
!       split excitation lines -> steln
    nteln = splitf(buf,ccm,mxspl,steln)
!       check maximal Campbell lines
    if (nteln .gt. mxteln) then
      close(ip)
      errmsg = fomsgf(99, nmm,4,cpn(1),0)
      return
    end if
!       convert to real
    do j = 1,nteln
      read(steln(j),*,end=100,err=100) dm
      teln(j) = dm
    end do
  else
!       default 1xrpm line
    nteln = 1
    teln(1) = 1
  end if
!
!     couplings, harmonic time, time step, mode list
  per = csn(2)
!     COUPLINGS FPRT FPRTS [MODES1,MODES2,...]
  read(ip,15,end=100,err=100)&
  &ncplg,fprt,fprts,buf
!     check maximum number of couplings
  if (ncplg .gt. mxcplg) then
    close(ip)
    errmsg = fomsgf(99, nmm,4,cpn(2),0)
    return
  end if
!     optional mode numbers overrides NBRMOD on main onput
!     zeros on tblock.f
  i = len_trim(buf)
  if (i .gt. 0) then
    ntmdn = splitf(buf,ccm,mxspl,steln)
    if (ntmdn .gt. mxtmdn) then
      close(ip)
      errmsg = fomsgf(99, nmm,4,cpn(3),0)
      return
    end if
    do j = 1,ntmdn
      read(steln(j),*,end=100,err=100) dm
      tmdnb(j) = nint(dm)
    end do
  end if
!
!     INDEX     KC        DC        IC        RATIO
  read(ip,250,end=100,err=100)
  do j = 1,ncplg
    read(ip,25,end=100,err=100)&
    &cplgid(j),cplgst(j),cplgdp(j),cplgin(j),cplgsr(j)
  end do
!
!     torsional modal damping, mode offset, output options
  per = csn(3)
!     DAMPING  [MD_OFF] [ETYPE,LOG,RAD/S]
  read(ip,300,end=100,err=100) ntdmp,buf
  if (ntdmp .gt. mxtdmp) then
    close(ip)
    errmsg = fomsgf(99, nmm,4,cpn(4),0)
    return
  end if
!     optional mode offset, element type, log and radian output options
!     see tblockd.f for initializations
  i = len_trim(buf)
  if (i .gt. 0) then
!       try read mode offset, element type, log and radian output option
    read(buf,*,err=120,iostat=ist) mdoff,telm,tlopt(1),tlopt(2)
!       check for mode offset, element type, log  output option
120 if (ist .ne. 0) read(buf,*,err=125,iostat=ist)&
    &mdoff,telm,tlopt(1)
!       check for mode offset, element type
125 if (ist .ne. 0) read(buf,*,err=130,iostat=ist) mdoff,telm
!       check for mode offset
130 if (ist .ne. 0) read(buf,*,end=100,err=100) mdoff
  else
    mdoff = 0
  end if
!     MODE      RATIO
  read(ip,250,end=100,err=100)
  do j = 1,ntdmp
    read(ip,35,end=100,err=100)&
    &tdmmd(j),tdmrt(j)
  end do
!
!     fillets,shear yield pu,equivalent stiffness definition
  per = csn(4)
!     FILLET [SHSTR] [STEQSF]
  read(ip,300,end=100,err=100) ntfil,buf
  if (ntfil .gt. mxtfil) then
    close(ip)
    errmsg = fomsgf(99, nmm,4,cpn(5),0)
    return
  end if
!     optional torsion shear yield pu -> tshyipu
  i = len_trim(buf)
  if (i .gt. 0) then
!       try read optional equivalent stiffness definition -> bf1
    read(buf,85,end=100,err=100) tshyipu,bf1
!       check for data on bf1
    i = len_trim(bf1)
    if (i .gt. 0) then
!         get coma separated data -> steln
      j = splitf(bf1,ccm,mxspl,steln)
!         check bound
      if (j .gt. mxtstf) then
!           more data than vector bound
        close(ip)
        errmsg = fomsgf(99, nmm,4,cpn(8),0)
        return
      else
!           bound ok, get brg-torque indices -> eqstid, ntest
        call teqstid(cs,j,mxspl,mxtstf,steln,eqstid)
        ntest = j
      end if
    end if
  end if
!     INDEX     RADIUS
  read(ip,250,end=100,err=100)
  do j = 1,ntfil
    read(ip,35,end=100,err=100) tfilid(j),tfilrd(j)
  end do
  per = csn(5)
!     TRANS     ITSTP     RSSTP     SIZE      INIT      FINT
  read(ip,45,end=100,err=100)&
  &nttra,ttris,ttrrs,ttrdsz,ttrit,ttret
!     check for torque equation
  iseqt = nttra .lt. 0
  nttra = abs(nttra)
!     check dimension
  if (nttra .gt. mxttra .or. ttrdsz .gt. mxttrd) then
    close(ip)
    if (nttra .gt. mxttra) then
      errmsg = fomsgf(99, nmm,4,cpn(6),0)
    else
      errmsg = fomsgf(99, nmm,4,cpn(7),0)
    end if
    return
  end if
!     TRANSIENT DAMPINGS
  read(ip,250,end=100,err=100)
  do j = 1,nttra
    read(ip,55,end=100,err=100) ttrmd(j)
  end do
!     TRANSIENT DATA
  read(ip,250,end=100,err=100)
!     check time equation
  if (.not. iseqt .and. nttra .gt. 0) then
!       time vector
    read(ip,*,end=100,err=100) (ttrdtt(k),k = 1,ttrdsz)
  end if
  if (nttra .eq. 0) read(ip,75,end=100,err=100)
!     excitations matrix
  do j = 1,nttra
!       check time equation
    if (.not. iseqt) then
!         torque values
      read(ip,*,end=100,err=100) (ttrdta(j,k),k = 1,ttrdsz)
    else
!         torque time equation
      read(ip,95,end=100,err=100) tequt(j)
    end if
  end do
  if (nttra .gt. 0) read(ip,75,end=100,err=100)
!
!     calculation options [optional]
  per = csn(6)
  read(ip,65,end=200,err=200) ops
!     call options proccessing
!     added check invalid option - francisco - feb-19
!     iex -> default export
  if (.not. coptf(ops,iex)) then
    errmsg = femsgf(99, nmm,6,10,0)
    return
  end if
  read(ip,75,end=200,err=100)
!
!     flexural - torsion optional data
  call topfltr(ip,nomarq,to,errmsg,k)
  if (k .ne. 0) return
!
!     return ok
200 ok = 0
!
  return
!
!     reading error
!     composed error message
100 errmsg = fopmsf(99, nmm,3,per,nomarq)
  return
!
5 format(//,2f10.0,a)
15 format(//,i10,2f10.0,a)
25 format(i10,4f10.0)
35 format(i10,f10.0)
45 format(//,i10,2f10.0,i10,2f10.0)
55 format(f10.0)
65 format(/,a)
75 format()
85 format(f10.0,a)
95 format(a)
250 format(/)
300 format(//,i10,a)
!
end subroutine tentrada
!
