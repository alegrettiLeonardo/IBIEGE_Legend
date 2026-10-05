!
!     $Id$
!     ==================================================================
!
!>    @file rotordin.f
!>    @author Carlos Alberto Bavastri UFPR - UTFPR - CEFET/PR,<br>
!>     Hideraldo Luis Vasconcelos dos Santos - R&D WMO,<br>
!>     Original language Matlab, current language Fortran 77/90
!>     (gfortran), coded mostly by FJDF.
!>    @brief RotorDin - rotordynamics calculation program.
!>    Last changes:<br>
!>    changed calculation options to vector opt - francisco - 03/12/2008
!>    added option elastic line opt(8) - francisco - 31/03/2009<br>
!>    added option undamped critical speed map opt(9) - francisco - 08/0
!>    added option beaing support opt(10) - francisco - 23/11/2009<br>
!>    changed options management (bug) - francisco - 19/02/2010<br>
!>    added center of mass - francisco - 11/10/2010<br>
!>    added center of mass in saidas.f - francisco - 11/10/2010<br>
!>    added pass of pp in espmod, resp_t.f - francisco - 05/11/2010<br>
!>    removed equivalent bearing support calculation - francisco - 05/11
!>    added parameter shp in entrada.f and resp_t.f - francisco - 15/06/
!>    bearing support moved to entrada - francisco - 14/02/2012<br>
!>    english translation - francisco - 20/02/2012<br>
!>    add oplg call moved from init - francisco - 20/02/2012<br>
!>    added options export 12 HB and 13 matrix market - francisco - 19/1
!>    added center of mass on geometry export - francisco - jul-15<br>
!>    added sok on entrada and saidas, support data on main input file -
!>    added mapriga, angular undamped critical speed map - francisco nov
!>    added disk offset support - francisco feb-19<br>
!>    added variable speed bearing parameters plot - francisco - feb-19<
!>    changed to central messages - francisco - apr-19<br>
!>    added application info on lof - francisco - oct-19<br>
!>    added torsion option - francisco - nov-19<br>
!>    changed geometry export call from wgeo to expgeom - francisco - ja
!>    added torsion calculations - francisco - mar-20<br>
!>    added frl, frequency response log plot - francisco - feb-21<br>
!>    added flexural-torsion flag parameter on optionsf,entrada and said
!>    added lateral get export speed function - francisco sep-21.
!>    added iex kind export flag on init, pinit, export and saidas - fra
!>    added iex export job index on entrada - francisco nov-21<br>
!>    added torsion element kind on saidas - francisco - nov-21<br>
!>    <p>WEG Equipamentos Eletricos S.A.<br>
!>    Critical speed calculation,
!>    solves a complex adjoint eigenvalues problem
!>    stated on mass, stiffness, damping and gyroscopic matrices</p>
!>    <p>References:<br>
!>    Rotordynamics prediction in engineering, <br>
!>    Lalanne, Michel and Ferraris, Guy,J. Wiley and Sons,<br>
!>    Chichester, ISBN 0-470-86037-5, 1990.<br>
!>    Dinamica e vibrazioni dei sistemi meccanici<br>
!>    Giorgio Diana, UTET Universita, 1993, ISBN-10: 8877502290.</p>
!>    <p>Torsion static and dynamic calculations, one and two nodes<br>
!>    References:<br>
!>    Torsional Vibration of Turbo-Machinery. Duncan N. Walker.</p>
!>    <p>Last known change DEC-2021</p>
!
!     ==================================================================
!>    @brief Main rotordynamics calculation program.
!>    @see init.f, saidas.f, entrada.f
!
program rotordin
  use rd_textfun, only: msizef, sinfo
  use com_mfa, only: ma, rm, au
  use com_nmi, only: inm, sbv
  use com_ppr, only: opt, ifn
  use com_touopt, only: telm, tlopt
  use rd_kinds, only: lrk
  implicit none
!
!     locals
  character(len=1) :: blank
  parameter(blank = ' ')
!
  character(len=99) :: msg
  character(len=255) :: opf
  integer :: pdm, pp, lgn, iex, ok, iop, im, iiff
  logical :: ard, lgp, prv, shp, sok, frl, fto, std, plt, tors, transient_requested_f, foundation_dynamic_f, fdyn
  real(lrk) :: vl, riff, expspdf
!     messages
  character(len=40) :: lm
!
!     min AF, modal rpm and angle unit
!     angle unit r -> radian, default degree
!
!     options
  integer :: mop
  parameter(mop = 20)
!
!
!     internal name, see init.f
!
!     torsion element type
!     logical output options lof and radian
!
!     clear options file name
  opf = blank
!
!     init
  call init(std,lgn,iex,msg,ok)
!     Unified progress telemetry is available during input/component set
  call progress_init(std)
!     screen/log message after init
  call scexems(lm,std,lgn,0,29)
  if (ok .ne. 0) call emsg(lgn,1,msg)
  call lmsg(0,sinfo(99, .true.,0))
!     init ok screen/log message
  call scexems(lm,std,lgn,1,0)
!
!     opt(11) standard i/o
  std = opt(11)
  call progress_init(std)
!
  call scexems(lm,std,lgn,0,1)
!     input
  call entrada(shp,opf,iex,sok,frl,fto,msg,ok)
  if (ok .ne. 0) call emsg(lgn,1,msg)
!     Chapter 6 modelling safety: validate raw physical input before
!     discretization.  This gate is intentionally always active.
  call model_input_audit(opt(18),msg,ok)
  if (ok .ne. 0) call emsg(lgn,1,msg)
  call scexems(lm,std,lgn,1,0)
!
!     calculation options
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
!     opt(12) none (export)
!     opt(13) none
!     opt(14) HPGL plot output
!     opt(15) undamped angular critical speed map
!     opt(16) variable speed bearing parameters plot
!     opt(17) override messages -o
!     opt(18) torsion option
!     opt(19) torsion time transient
!     opt(20) flexural-torsion
!     iex export kind flag = 0 HB, 1=mm,2=plain!
!
!     bearing speed variable parameters
!     should check AFTER input
  prv = opt(7)
!
!     opt(14) HPGL plot output
  plt = opt(14)
!
!     torsional option - francisco - nov-19
  tors = opt(18)
!
!     check options, see init.f
  call pinit(lgn,mop,iex,opt,inm)
!
  call scexems(lm,std,lgn,0,2)
!     input data processing
!     added torsional option  - francisco-nov-19
  call predad(tors,msg,ok)
  if (ok .ne. 0) call emsg(lgn,1,msg)
!     Catch zero-total-mass and invalid generated element lengths before
!     center-of-mass calculations or any dynamic solution.
  if (.not. tors) then
    call model_preassembly_audit(msg,ok)
    if (ok .ne. 0) call emsg(lgn,1,msg)
  endif
  call scexems(lm,std,lgn,1,0)
  fdyn=foundation_dynamic_f()
!
!     center of mass calculation -> vl
  call calccm(vl)
!
  call scexems(lm,std,lgn,0,3)
!     basic data output
!     added tors - francisco - mar-20
!     added telm - francisco - nov-21
  call saidas(vl,opf,tors,fto,sok,iex,telm,msg,ok)
  if (ok .ne. 0) call emsg(lgn,1,msg)
  call scexems(lm,std,lgn,1,0)
!
  if (opt(6)) then
    call scexems(lm,std,lgn,0,16)
!       geometry data
    call expgeom(std,plt,vl,msg,ok)
    if (ok .ne. 0) call emsg(lgn,1,msg)
    call scexems(lm,std,lgn,1,0)
  end if
!
!     lateral or torsion
  if (tors) then
!
!       torsion
!
!       see tentrada.f, tblockd.f and tsaidas.f
!       index model 1:two nodes, 2:three nodes
    im = telm
!       plot output angle in radian
    ard = tlopt(2)
!       frequency response plot output log scale
    lgp = tlopt(1)
!
    call scexems(lm,std,lgn,0,21)
!       matrix assemble
    call tmatrices(im,pdm,msg,ok)
    if (ok .ne. 0) call emsg(lgn,1,msg)
    call scexems(lm,std,lgn,1,0)
!
!       show size of the matrices, see messages.f
    msg = msizef(99, pdm,.false.)
    call emsg(lgn,0,msg)
    lm = msg(2:)
    call sprint(std,lm,blank)
!       need of modal space
    if (opt(1) .or. opt(2) .or.  opt(3) .or. opt(4) .or.&
    &opt(12) .or. opt(13) .or. opt(19)) then
!
      call scexems(lm,std,lgn,0,22)
!         torsion modal space
      call tespmod(msg,ok)
      if (ok .ne. 0) call emsg(lgn,1,msg)
      call scexems(lm,std,lgn,1,0)
!
!         torsion modal damping matrix
      call tmdamp()
!
      if (opt(1)) then
        call scexems(lm,std,lgn,0,23)
!           torsion campbell
        call tcmpbl(std,plt,msg,ok)
        if (ok .ne. 0) call emsg(lgn,1,msg)
        call scexems(lm,std,lgn,1,0)
      end if
!
      if (opt(2)) then
        call scexems(lm,std,lgn,0,24)
!           torsion modes
        call tmodos(im,std,plt,msg,ok)
        if (ok .ne. 0) call emsg(lgn,1,msg)
        call scexems(lm,std,lgn,1,0)
      end if
!
      if (opt(3)) then
        call scexems(lm,std,lgn,0,25)
!           torsion frequency response
        call tresp_f(im,ard,lgp,std,plt,msg,ok)
        if (ok .ne. 0) call emsg(lgn,1,msg)
        call scexems(lm,std,lgn,1,0)
      end if
!
      if (opt(4)) then
        call scexems(lm,std,lgn,0,26)
!           torsion time harmonic response
        call tpefors(im,ard,std,plt,msg,ok)
        if (ok .ne. 0) call emsg(lgn,1,msg)
        call scexems(lm,std,lgn,1,0)
      end if
!
      if (.not. iex .lt. 0) then
        call scexems(lm,std,lgn,0,19)
!           export torsion modal parameters matrices, FI and LEMDA
!           iex export kind flag = 0 HB, 1=mm,2=plain!
        call texport(iex,std,msg,ok)
        if (ok .ne. 0) call emsg(lgn,1,msg)
        call scexems(lm,std,lgn,1,0)
      end if
!
      if (opt(19)) then
        call scexems(lm,std,lgn,0,27)
!           torsion time transient
        call tprptran(im,ard,std,plt,msg,ok)
        if (ok .ne. 0) call emsg(lgn,1,msg)
        call scexems(lm,std,lgn,1,0)
      end if
!
!         torsion modal space if
    end if
!
    if (opt(8)) then
      call scexems(lm,std,lgn,0,28)
!         torsion static
      call tstatic(im,std,plt,msg,ok)
      if (ok .ne. 0) call emsg(lgn,1,msg)
      call scexems(lm,std,lgn,1,0)
    end if
!
    if (opt(20)) then
      call scexems(lm,std,lgn,0,30)
!         flexural-torsion
      call tflextor(im,pdm,pp,prv,std,plt,msg,ok)
      if (ok .ne. 0) call emsg(lgn,1,msg)
      call scexems(lm,std,lgn,1,0)
    end if
!
!       else lateral/torsion if
  else
!
!       lateral
!
    call scexems(lm,std,lgn,0,4)
!       matrices assembly
    call matrizes(pp,pdm,msg,ok)
!       pp -> number of effective bearings with support
!       pp = 0 if consider no support
    if (ok .ne. 0) call emsg(lgn,1,msg)
!       Global matrix safety gate: finite values, M symmetry/PD,
!       structural-K symmetry and gyroscopic skew-symmetry.
    call model_assembled_audit(pdm,std,msg,ok)
    if (ok .ne. 0) call emsg(lgn,1,msg)
    call flexdisk_write_audit(std)
    call scexems(lm,std,lgn,1,0)
!
!       size of the matrices, opt(10) => has support
    msg = msizef(99, pdm,opt(10))
    call emsg(lgn,0,msg)
    lm = msg(2:)
    call sprint(std,lm,blank)
!
!       A measured/computed dynamic-stiffness table is a frequency-
!       domain impedance. V1 intentionally permits only direct lateral
!       frequency response; modal/Campbell/static/time use PHYSICAL_MCK.
    if(fdyn .and. (opt(1) .or. opt(2) .or. opt(4) .or.&
    &opt(8) .or. opt(9) .or. opt(15) .or.&
    &transient_requested_f())) then
      msg='foundation: DYNAMIC_STIFFNESS_TABLE supports -f only'
      call emsg(lgn,1,msg)
    endif
!
!       The true transient is a separate physical-space integrator.
!       The historical -t path remains resp_t below.
    if (transient_requested_f()) then
      call transient_solver(pp,std,msg,ok)
      if (ok .ne. 0) call emsg(lgn,1,msg)
    end if
!
    if (opt(1)) then
      iop = iiff(prv,5,6)
      call scexems(lm,std,lgn,0,iop)
!         campbell
      call campbell(pp,prv,std,plt,msg,ok)
      if (ok .ne. 0) call emsg(lgn,1,msg)
      call scexems(lm,std,lgn,1,0)
    end if
!
    if (opt(9)) then
      call scexems(lm,std,lgn,0,7)
!         undamped critical speed map
      call foundation_warn_legacy_analysis('CRITICAL SPEED MAP')
      call maprigs(pp,std,plt,prv,msg,ok)
      if (ok .ne. 0) call emsg(lgn,1,msg)
      call scexems(lm,std,lgn,1,0)
    end if
!
    if (opt(15)) then
      call scexems(lm,std,lgn,0,8)
!         undamped angular critical speed map
      call foundation_warn_legacy_analysis(&
      &'ANGULAR CRITICAL SPEED MAP')
      call mapriga(pp,prv,std,plt,msg,ok)
      if (ok .ne. 0) call emsg(lgn,1,msg)
      call scexems(lm,std,lgn,1,0)
    end if
!       Modes/MDRPM is explicitly the prescribed-spin problem:
!       M*qdd+(C+Omega*G)*qd+K*q=0.  Bearing K/C selection remains
!       independent from the physical spin speed.
    if (opt(2)) then
      iop = 1
      if (prv) iop = 0
      call scexems(lm,std,lgn,0,10)
      call espmod_speed(pp,rm,iop,msg,ok)
      if (ok .ne. 0) call emsg(lgn,1,msg)
      call foundation_mode_participation(pp)
      call scexems(lm,std,lgn,1,0)
!
      call scexems(lm,std,lgn,0,11)
      call modos(rm,pp,std,plt,msg,ok)
      if (ok .ne. 0) call emsg(lgn,1,msg)
      call scexems(lm,std,lgn,1,0)
    end if
!
!       Direct frequency response is a physical-space dynamic-stiffness
!       solve and must never be gated by, or consume, modal space.
    if (opt(3)) then
      iop = 12
      if (prv .or. fdyn) iop = 14
      call scexems(lm,std,lgn,0,iop)
      iop = 1
      if (prv) iop = 0
      call resp_fv(pdm,pp,iop,std,frl,plt,msg,ok)
      if (ok .ne. 0) call emsg(lgn,1,msg)
      call scexems(lm,std,lgn,1,0)
    end if
!
!       Historical time-harmonic response retains the synchronous modal
!       basis used by its legacy transfer formula.  Fixed K/C needs one
!       solve here; variable K/C recomputes it inside resp_t per speed.
    if (opt(4) .and. .not. prv) then
      call scexems(lm,std,lgn,0,9)
      call espmod_sync(pp,-1.0_lrk,msg,ok)
      if (ok .ne. 0) call emsg(lgn,1,msg)
      call foundation_mode_participation(pp)
      call scexems(lm,std,lgn,1,0)
!
      call scexems(lm,std,lgn,0,13)
      call resp_t(pdm,pp,0,shp,std,plt,msg,ok)
      if (ok .ne. 0) call emsg(lgn,1,msg)
      call scexems(lm,std,lgn,1,0)
    end if
!
    if (prv .and. opt(4)) then
      call scexems(lm,std,lgn,0,15)
      call resp_t(pdm,pp,1,shp,std,plt,msg,ok)
      if (ok .ne. 0) call emsg(lgn,1,msg)
      call scexems(lm,std,lgn,1,0)
    end if
!
    if (opt(8)) then
      call scexems(lm,std,lgn,0,17)
!         static elastic line
      call foundation_warn_legacy_analysis('STATIC ELASTIC LINE')
      call linhael(prv,std,plt,msg,ok)
      if (ok .ne. 0) call emsg(lgn,1,msg)
      call scexems(lm,std,lgn,1,0)
    end if
!
    if (.not. iex .lt. 0) then
      iop = iiff(prv,18,19)
      call scexems(lm,std,lgn,0,iop)
!         export modal parameters matrices, PHI, PSI and LAMBDA
!         prv => fixed modal space
!                speed dependent modal space
!         expspdf() => get export speed, see expmat.f
      vl = riff(prv,expspdf(),-1.0_lrk)
      call espmod(pp,vl,msg,ok)
      if (ok .ne. 0) call emsg(lgn,1,msg)
!         iex export kind flag = 0 HB, 1=mm,2=plain
      call export(iex,vl,std,msg,ok)
      if (ok .ne. 0) call emsg(lgn,1,msg)
      call scexems(lm,std,lgn,1,0)
    end if
!
    if (opt(16)) then
      call scexems(lm,std,lgn,0,20)
!         bearing variable speed parameters
      call sbeapar(prv,std,plt,msg,ok)
      if (ok .ne. 0) call emsg(lgn,1,msg)
      call scexems(lm,std,lgn,1,0)
    end if
!
!       lateral/torsion if
  end if
!
!     time consumption
  call sfinms(std,lgn)
!
!     log output
  if (lgn .ne. 0) then
!       output device handle number
    iop = iiff(std,6,2)
    call oplg(iop,msg,ok)
    if (ok .ne. 0) call emsg(lgn,1,msg)
  end if
!
!     ok end
  stop
!
end program rotordin
!
