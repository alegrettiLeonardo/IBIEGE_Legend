!
!     Chapter 6 modelling safety gate.
!     Keeps the historical governing equations unchanged and adds
!     input/model validation plus auditable model-quality diagnostics.
!
logical function model_finite_r4(x)
  use rd_kinds, only: lrk
  implicit none
  real(lrk) :: x
  model_finite_r4=(x.eq.x).and.(abs(x).le.huge(x))
  return
end function model_finite_r4
!
subroutine model_upper(s)
  implicit none
  character(len=*) :: s
  integer :: i, k
  do i=1,len(s)
    k=ichar(s(i:i))
    if(k.ge.97.and.k.le.122) s(i:i)=char(k-32)
  enddo
  return
end subroutine model_upper
!
!     Structured, user-oriented diagnostic for fatal model errors.
!     The historical EMSG path remains the final stop mechanism; this
!     routine adds engineering context before EMSG terminates the run.
!
subroutine model_guided_error(errmsg,code,summary,location,&
&received,expected,cause,fix)
  implicit none
  character(len=*) :: errmsg, code, summary, location, received, expected
  character(len=*) :: cause, fix
  character(len=99) :: short
  integer :: ios
  short='MODEL_AUDIT ['//trim(code)//']: '//trim(summary)
  errmsg=short
  call model_guidance_unit(0,code,summary,location,received,&
  &expected,cause,fix)
  open(98,file='model_error.out',status='unknown',iostat=ios)
  if(ios.eq.0) then
    rewind(98)
    call model_guidance_unit(98,code,summary,location,received,&
    &expected,cause,fix)
    close(98)
  endif
  return
end subroutine model_guided_error
!
subroutine model_guidance_unit(iu,code,summary,location,&
&received,expected,cause,fix)
  implicit none
  integer :: iu
  character(len=*) :: code, summary, location, received, expected, cause, fix
  write(iu,'(a)') '#BEGIN MODEL_ERROR'
  write(iu,'(a,a)') 'ERROR CODE          : ',trim(code)
  write(iu,'(a,a)') 'SUMMARY             : ',trim(summary)
  write(iu,'(a,a)') 'LOCATION            : ',trim(location)
  write(iu,'(a,a)') 'RECEIVED            : ',trim(received)
  write(iu,'(a,a)') 'EXPECTED            : ',trim(expected)
  write(iu,'(a,a)') 'PHYSICAL CAUSE      : ',trim(cause)
  write(iu,'(a,a)') 'HOW TO FIX          : ',trim(fix)
  write(iu,'(a)') 'ACTION              : analysis aborted before '//&
  &'invalid results could be generated'
  write(iu,'(a)') '#END MODEL_ERROR'
  return
end subroutine model_guidance_unit
!
!     Optional tail controls.  MODEL_AUDIT is deliberately non-disableable.
!       MODEL_AUDIT ON
!       MESH_AUDIT ON
!       MESH_MODES 4
!       MESH_TOL 0.005
!       MESH_LEVELS 3
!       REFERENCE_MASS 1865.0
!       REFERENCE_CG 1.143
!       UNIT_SYSTEM SI
!
subroutine model_config_line(line,errmsg,ok,handled)
  use com_maudi, only: mesh_modes, mesh_levels
  use com_maudl, only: mesh_en, refm_set, refcg_set
  use com_maudr, only: mesh_tol, ref_mass, ref_cg
  use rd_kinds, only: lrk
  implicit none
  character(len=*) :: line, errmsg
  integer :: ok, ios, ival
  logical :: handled
  real(lrk) :: rval
  character(len=512) :: uline
  character(len=32) :: key, val
  character(len=240) :: rec, loc, exp, cause, fix
  ok=0
  ival=0
  rval=0.0_lrk
  handled=.false.
  uline=adjustl(line)
  call model_upper(uline)
  if(len_trim(uline).eq.0) return
  read(uline,*,iostat=ios) key
  if(ios.ne.0) return
  call model_upper(key)
  if(trim(key).eq.'MODEL_AUDIT') then
    handled=.true.
    val='ON'
    read(uline,*,iostat=ios) key,val
    call model_upper(val)
    if(trim(val).eq.'OFF'.or.trim(val).eq.'NO'.or.&
    &trim(val).eq.'0') then
      loc='MODEL_AUDIT configuration'
      rec='MODEL_AUDIT='//trim(val)
      exp='MODEL_AUDIT ON; the Chapter-6 safety gate is mandatory'
      cause='the input attempts to disable mandatory model validation'
      fix='remove MODEL_AUDIT OFF or change the value to ON'
      call model_guided_error(errmsg,'MODEL_CONFIG_001',&
      &'model safety gate cannot be disabled',loc,rec,exp,cause,fix)
      ok=-1
    endif
    return
  else if(trim(key).eq.'MESH_AUDIT') then
    handled=.true.
    val='ON'
    read(uline,*,iostat=ios) key,val
    call model_upper(val)
    if(trim(val).eq.'OFF'.or.trim(val).eq.'NO'.or.&
    &trim(val).eq.'0') then
      mesh_en=.false.
    else if(trim(val).eq.'ON'.or.trim(val).eq.'YES'.or.&
    &trim(val).eq.'1') then
      mesh_en=.true.
    else
      loc='MESH_AUDIT configuration'
      rec='MESH_AUDIT='//trim(val)
      exp='ON/YES/1 or OFF/NO/0'
      cause='unsupported logical value in the mesh-audit control'
      fix='use MESH_AUDIT ON or MESH_AUDIT OFF'
      call model_guided_error(errmsg,'MODEL_CONFIG_002',&
      &'invalid MESH_AUDIT value',loc,rec,exp,cause,fix)
      ok=-1
    endif
    return
  else if(trim(key).eq.'MESH_MODES') then
    handled=.true.
    read(uline,*,iostat=ios) key,ival
    if(ios.ne.0.or.ival.lt.1.or.ival.gt.19) then
      loc='MESH_MODES configuration'
      write(rec,'(a,i0,a,i0)') 'value=',ival,'; iostat=',ios
      exp='integer MESH_MODES in [1,19]'
      cause='requested mode count is outside supported audit range'
      fix='choose between 1 and 19 modes for the convergence audit'
      call model_guided_error(errmsg,'MODEL_CONFIG_003',&
      &'invalid MESH_MODES value',loc,rec,exp,cause,fix)
      ok=-1
    else
      mesh_modes=ival
    endif
    return
  else if(trim(key).eq.'MESH_LEVELS') then
    handled=.true.
    read(uline,*,iostat=ios) key,ival
    if(ios.ne.0.or.ival.lt.2.or.ival.gt.4) then
      loc='MESH_LEVELS configuration'
      write(rec,'(a,i0,a,i0)') 'value=',ival,'; iostat=',ios
      exp='integer MESH_LEVELS in [2,4]'
      cause='too few/many automatic refinement levels requested'
      fix='use 2, 3, or 4 mesh refinement levels'
      call model_guided_error(errmsg,'MODEL_CONFIG_004',&
      &'invalid MESH_LEVELS value',loc,rec,exp,cause,fix)
      ok=-1
    else
      mesh_levels=ival
    endif
    return
  else if(trim(key).eq.'MESH_TOL') then
    handled=.true.
    read(uline,*,iostat=ios) key,rval
    if(ios.ne.0.or.rval.le.0.0_lrk.or.rval.ge.1.0_lrk) then
      loc='MESH_TOL configuration'
      write(rec,'(a,es14.6,a,i0)') 'value=',rval,'; iostat=',ios
      exp='dimensionless tolerance 0 < MESH_TOL < 1'
      cause='invalid relative convergence tolerance'
      fix='for example, use 0.005 for a 0.5 percent criterion'
      call model_guided_error(errmsg,'MODEL_CONFIG_005',&
      &'invalid MESH_TOL value',loc,rec,exp,cause,fix)
      ok=-1
    else
      mesh_tol=rval
    endif
    return
  else if(trim(key).eq.'REFERENCE_MASS') then
    handled=.true.
    read(uline,*,iostat=ios) key,rval
    if(ios.ne.0.or.rval.le.0.0_lrk) then
      loc='REFERENCE_MASS configuration'
      write(rec,'(a,es14.6,a,i0)') 'value=',rval,'; iostat=',ios
      exp='finite positive reference mass in kg'
      cause='reference mass is missing, non-numeric, zero, or negative'
      fix='enter the known measured/reference rotor mass in kg'
      call model_guided_error(errmsg,'MODEL_CONFIG_006',&
      &'invalid REFERENCE_MASS',loc,rec,exp,cause,fix)
      ok=-1
    else
      ref_mass=rval
      refm_set=.true.
    endif
    return
  else if(trim(key).eq.'REFERENCE_CG') then
    handled=.true.
    read(uline,*,iostat=ios) key,rval
    if(ios.ne.0) then
      loc='REFERENCE_CG configuration'
      write(rec,'(a,es14.6,a,i0)') 'value=',rval,'; iostat=',ios
      exp='finite reference center-of-gravity position in m'
      cause='reference CG could not be parsed as a numeric value'
      fix='enter the known/reference CG axial position in metres'
      call model_guided_error(errmsg,'MODEL_CONFIG_007',&
      &'invalid REFERENCE_CG',loc,rec,exp,cause,fix)
      ok=-1
    else
      ref_cg=rval
      refcg_set=.true.
    endif
    return
  else if(trim(key).eq.'UNIT_SYSTEM') then
    handled=.true.
    val=' '
    read(uline,*,iostat=ios) key,val
    call model_upper(val)
    if(ios.ne.0.or.trim(val).ne.'SI') then
      loc='UNIT_SYSTEM configuration'
      rec='UNIT_SYSTEM='//trim(val)
      exp='UNIT_SYSTEM SI'
      cause='this RotorDin build accepts only the documented SI system'
      fix='convert input data to SI and set UNIT_SYSTEM SI'
      call model_guided_error(errmsg,'MODEL_CONFIG_008',&
      &'unsupported unit system',loc,rec,exp,cause,fix)
      ok=-1
    endif
    return
  endif
  return
end subroutine model_config_line
!
subroutine model_input_audit(tors,errmsg,ok)
  use com_conc, only: nmic, psic, vlmc, ixic, iyic, izic
  use com_dis, only: pd, dd => d_d, hd => h_d, rhod => rho_d, r2, nd
  use com_disa, only: did => d_i, ix => i_x, iy => i_y, mdin => m_d
  use com_doff, only: offd => off_d
  use com_eix, only: l, d, di, ps, e, nu, rho => rho_e, ge => g_e, r1, cs
  use com_eixa, only: divs, umps
  use com_man, only: kxx, kxz, kzz, kzx, cxx, cxz, czz, czx, bmass => mm, scl => sc
  use com_unb0, only: pr, desp, ori, np, nm
  use com_ymc, only: nbrg, rks, pc
  use rd_kinds, only: lrk
  implicit none
  logical :: tors, model_finite_r4
  character(len=*) :: errmsg
  integer :: ok, i, j
  integer :: mxs, mxd, mxm, mxr, mxic
  parameter(mxs=99,mxd=99,mxm=9,mxr=9,mxic=15)
  character(len=240) :: rec, loc, exp, cause, fix
  real(lrk) :: prev
  ok=-1
  if(.not.model_finite_r4(l).or.l.le.0.0_lrk) then
    write(rec,'(a,es14.6)') 'shaft length L = ',l
    loc='global shaft definition'
    exp='finite shaft length L > 0 m'
    cause='missing/invalid shaft length or inconsistent SI input'
    fix='set the total shaft length to a positive finite value in m'
    call model_guided_error(errmsg,'MODEL_GEOMETRY_001',&
    &'invalid shaft length',loc,rec,exp,cause,fix)
    return
  endif
  if(cs.lt.1.or.cs.gt.mxs) then
    write(rec,'(a,i0)') 'number of shaft sections = ',cs
    loc='SECTIONS control block'
    write(exp,'(a,i0,a)') 'integer in [1,',mxs,']'
    cause='section count is zero, negative, or exceeds solver limit'
    fix='correct the SECTIONS count to match the listed shaft rows'
    call model_guided_error(errmsg,'MODEL_GEOMETRY_002',&
    &'invalid shaft section count',loc,rec,exp,cause,fix)
    return
  endif
  prev=0.0_lrk
  do i=1,cs
    if(.not.model_finite_r4(ps(i)).or.ps(i).le.prev.or.ps(i).gt.l+1.0e-6_lrk) then
      write(loc,'(a,i0)') 'shaft section ',i
      write(rec,'(a,es14.6,a,es14.6,a,es14.6)')&
      &'position = ',ps(i),'; previous = ',prev,&
      &'; shaft L = ',l
      exp='previous position < current position <= shaft length'
      cause='duplicate, unsorted, non-finite, or out-of-range station'
      fix='order section end positions increasingly and remove overlaps'
      call model_guided_error(errmsg,'MODEL_GEOMETRY_003',&
      &'invalid/non-increasing shaft section',loc,rec,exp,&
      &cause,fix)
      return
    endif
    if(.not.model_finite_r4(d(i)).or.d(i).le.0.0_lrk.or..not.model_finite_r4(di(i)).or.di(i).lt.0.0_lrk.or.di(i) &
      & .ge.d(i)) then
      write(loc,'(a,i0)') 'shaft section ',i
      write(rec,'(a,es14.6,a,es14.6)')&
      &'outer diameter = ',d(i),'; inner diameter = ',di(i)
      exp='outer diameter > 0; 0 <= inner diameter < outer diameter'
      cause='diameters may be swapped, negative, or entered in wrong SI'
      fix='correct De and Di in metres; ensure Di is smaller than De'
      call model_guided_error(errmsg,'MODEL_GEOMETRY_004',&
      &'invalid shaft diameters',loc,rec,exp,cause,fix)
      return
    endif
    if(.not.model_finite_r4(e(i)).or.e(i).le.0.0_lrk) then
      write(loc,'(a,i0)') 'shaft section ',i
      write(rec,'(a,es14.6,a)') 'Young modulus E = ',e(i),' Pa'
      exp='finite Young modulus E > 0 Pa'
      cause='material stiffness is missing, zero, negative, or non-finite'
      fix='enter the material Young modulus in Pa for this section'
      call model_guided_error(errmsg,'MODEL_MATERIAL_001',&
      &'invalid Young modulus',loc,rec,exp,cause,fix)
      return
    endif
    if(.not.model_finite_r4(nu(i)).or.nu(i).le.-0.999_lrk.or.nu(i).ge.0.5_lrk) then
      write(loc,'(a,i0)') 'shaft section ',i
      write(rec,'(a,es14.6)') 'Poisson ratio nu = ',nu(i)
      exp='finite -0.999 < nu < 0.5 for the isotropic shaft model'
      cause='invalid material property or incorrect decimal/unit entry'
      fix='enter a physically admissible dimensionless Poisson ratio'
      call model_guided_error(errmsg,'MODEL_MATERIAL_002',&
      &'invalid Poisson ratio',loc,rec,exp,cause,fix)
      return
    endif
    if(.not.model_finite_r4(rho(i)).or.rho(i).lt.0.0_lrk) then
      write(loc,'(a,i0)') 'shaft section ',i
      write(rec,'(a,es14.6,a)') 'density rho = ',rho(i),' kg/m3'
      exp='finite density rho >= 0 kg/m3; zero is stiffness-only'
      cause='negative/non-finite density or invalid numeric input'
      fix='enter density in kg/m3; use zero only intentionally'
      call model_guided_error(errmsg,'MODEL_MATERIAL_003',&
      &'invalid shaft density',loc,rec,exp,cause,fix)
      return
    endif
    if(.not.model_finite_r4(umps(i)).or.umps(i).lt.0.0_lrk) then
      write(loc,'(a,i0)') 'shaft section ',i
      write(rec,'(a,es14.6)') 'UMP parameter = ',umps(i)
      exp='finite UMP parameter >= 0'
      cause='invalid electromagnetic UMP parameter in shaft definition'
      fix='correct the UMP value or set it to zero when not used'
      call model_guided_error(errmsg,'MODEL_UMP_001',&
      &'invalid UMP parameter',loc,rec,exp,cause,fix)
      return
    endif
    if(umps(i).gt.0.0_lrk.and.rho(i).le.0.0_lrk) then
      write(loc,'(a,i0)') 'shaft section ',i
      write(rec,'(a,es14.6,a,es14.6)')&
      &'UMP = ',umps(i),'; density = ',rho(i)
      exp='density > 0 kg/m3 whenever UMP is active'
      cause='UMP is active on a stiffness-only section with no mass'
      fix='assign physical density or disable UMP on this section'
      call model_guided_error(errmsg,'MODEL_UMP_002',&
      &'UMP requires positive density',loc,rec,exp,cause,fix)
      return
    endif
    if(divs(i).lt.0) then
      write(loc,'(a,i0)') 'shaft section ',i
      write(rec,'(a,i0)') 'DIV = ',divs(i)
      exp='DIV >= 0; zero means use the normal automatic subdivision'
      cause='negative finite-element subdivision request'
      fix='set DIV to zero or to a positive number of subdivisions'
      call model_guided_error(errmsg,'MODEL_MESH_001',&
      &'negative section subdivision',loc,rec,exp,cause,fix)
      return
    endif
    prev=ps(i)
  enddo
  do i=1,nd
    if(.not.model_finite_r4(pd(i)).or.pd(i).lt.0.0_lrk.or.pd(i).gt.l) then
      write(loc,'(a,i0)') 'disk ',i
      write(rec,'(a,es14.6,a,es14.6)')&
      &'disk position = ',pd(i),'; shaft L = ',l
      exp='finite disk position in 0 <= x <= shaft length'
      cause='disk is outside the shaft or has an invalid position'
      fix='move the disk station inside the modeled shaft span'
      call model_guided_error(errmsg,'MODEL_DISK_001',&
      &'invalid disk position',loc,rec,exp,cause,fix)
      return
    endif
    if(.not.model_finite_r4(dd(i)).or.dd(i).lt.0.0_lrk.or..not.model_finite_r4(hd(i)).or.hd(i).lt.0.0_lrk &
      & .or..not.model_finite_r4(did(i)).or.did(i).lt.0.0_lrk.or.(dd(i).le.0.0_lrk.and.did(i).gt.0.0_lrk).or.(dd(i) &
      & .gt.0.0_lrk.and.did(i).ge.dd(i)).or..not.model_finite_r4(rhod(i)).or.rhod(i).lt.0.0_lrk &
      & .or..not.model_finite_r4(mdin(i)).or.mdin(i).lt.0.0_lrk.or..not.model_finite_r4(ix(i)).or.ix(i).lt.0.0_lrk &
      & .or..not.model_finite_r4(iy(i)).or.iy(i).lt.0.0_lrk.or..not.model_finite_r4(offd(i))) then
      write(loc,'(a,i0)') 'disk ',i
      write(rec,'(a,8(es11.3,1x))') 'De Di h rho m Ix Iy off = ',&
      &dd(i),did(i),hd(i),rhod(i),mdin(i),ix(i),iy(i),offd(i)
      exp='finite non-negative disk data; if De>0 then 0<=Di<De'
      cause='invalid geometry, density, mass/inertia, or disk offset'
      fix='review disk SI data and avoid negative/non-finite properties'
      call model_guided_error(errmsg,'MODEL_DISK_002',&
      &'invalid disk data',loc,rec,exp,cause,fix)
      return
    endif
  enddo
  do i=1,nmic
    if(.not.model_finite_r4(psic(i)).or.psic(i).lt.0.0_lrk.or.psic(i).gt.l.or..not.model_finite_r4(vlmc(i)).or.vlmc(i) &
      & .lt.0.0_lrk.or..not.model_finite_r4(ixic(i)).or.ixic(i).lt.0.0_lrk.or..not.model_finite_r4(iyic(i)).or.iyic(i) &
      & .lt.0.0_lrk.or..not.model_finite_r4(izic(i)).or.izic(i).lt.0.0_lrk) then
      write(loc,'(a,i0)') 'concentrated mass ',i
      write(rec,'(a,5(es11.3,1x))') 'x m Ix Iy Iz = ',psic(i),&
      &vlmc(i),ixic(i),iyic(i),izic(i)
      exp='0<=x<=L and finite mass/inertias >= 0 in SI units'
      cause='invalid concentrated mass position, mass, or inertia'
      fix='correct the concentrated mass and inertia values in SI units'
      call model_guided_error(errmsg,'MODEL_MASS_001',&
      &'invalid concentrated mass/inertia',loc,rec,exp,cause,fix)
      return
    endif
  enddo
  if(.not.tors) then
    do i=1,nbrg
      if(.not.model_finite_r4(pc(i)).or.pc(i).lt.0.0_lrk.or.pc(i).gt.l.or..not.model_finite_r4(kxx(i)) &
        & .or..not.model_finite_r4(kxz(i)).or..not.model_finite_r4(kzz(i)).or..not.model_finite_r4(kzx(i)) &
        & .or..not.model_finite_r4(cxx(i)).or..not.model_finite_r4(cxz(i)).or..not.model_finite_r4(czz(i)) &
        & .or..not.model_finite_r4(czx(i)).or..not.model_finite_r4(bmass(i))) then
        write(loc,'(a,i0)') 'lateral bearing ',i
        write(rec,'(a,3(es11.3,1x))') 'x Kxx mass = ',pc(i),&
        &kxx(i),bmass(i)
        exp='finite bearing position in [0,L] and finite K,C,m data'
        cause='invalid/interrupted bearing coefficient input or position'
        fix='review bearing K/C coefficients, mass, position, and units'
        call model_guided_error(errmsg,'MODEL_BEARING_001',&
        &'non-finite/invalid bearing data',loc,rec,exp,cause,fix)
        return
      endif
      if(bmass(i).lt.0.0_lrk) then
        write(loc,'(a,i0)') 'lateral bearing ',i
        write(rec,'(a,es14.6,a)')&
        &'bearing/support mass = ',bmass(i),' kg'
        exp='bearing/support mass >= 0 kg'
        cause='negative support inertia gives non-physical kinetic energy'
        fix='correct the bearing/support mass; use zero only if intended'
        call model_guided_error(errmsg,'MODEL_BEARING_002',&
        &'negative lateral bearing mass',loc,rec,exp,cause,fix)
        return
      endif
    enddo
    do i=1,np
      if(.not.model_finite_r4(pr(i)).or.(pr(i).ge.0.0_lrk.and.pr(i).gt.l).or..not.model_finite_r4(desp(i)) &
        & .or..not.model_finite_r4(ori(i))) then
        write(loc,'(a,i0)') 'response definition ',i
        write(rec,'(a,3(es12.4,1x))') 'position disp ori = ',&
        &pr(i),desp(i),ori(i)
        exp='finite response data; positive position must satisfy x<=L'
        cause='response/probe position or amplitude/orientation is invalid'
        fix='correct FRESPPOS/response data and keep probes inside shaft'
        call model_guided_error(errmsg,'MODEL_RESPONSE_001',&
        &'invalid response definition',loc,rec,exp,cause,fix)
        return
      endif
    enddo
  endif
  ok=0
  return
end subroutine model_input_audit
!
!     Runs after PREDAD, before center-of-mass output.  This gate prevents
!     a zero-total-mass model from reaching calccm(), while rho=0 stiffness-
!     only regions remain legal until the global M positive-definite test.
!
subroutine model_preassembly_audit(errmsg,ok)
  use com_conc, only: nmic, psic, vlmc, ixic, iyic, izic
  use com_dis, only: pd, dd => d_d, hd => h_d, rhod => rho_d, r2, nd
  use com_doff, only: offd => off_d
  use com_dsc, only: md, idx, idy
  use com_sea, only: rs, es, gs, dely, s, ie, ump
  use com_sec, only: n, y, nt, nn
  use rd_kinds, only: lrk, wp
  implicit none
  character(len=*) :: errmsg
  character(len=240) :: rec, loc, exp, cause, fix
  integer :: ok, i, mxd, mxic, mts
  parameter(mxd=99,mxic=15,mts=999)
  real(wp) :: mt, dx
  mt=0._wp
  if(nt.lt.2) then
    write(rec,'(a,i0)') 'generated stations = ',nt
    loc='generated lateral finite-element mesh'
    exp='at least 2 stations and 1 finite element'
    cause='input geometry collapsed or discretization produced no span'
    fix='check shaft positions, section count, and mesh subdivisions'
    call model_guided_error(errmsg,'MODEL_MESH_002',&
    &'too few lateral stations',loc,rec,exp,cause,fix)
    ok=-1
    return
  endif
  do i=1,nt-1
    dx=real(y(i+1)-y(i), wp)
    if(dx.le.0._wp) then
      write(loc,'(a,i0)') 'finite element ',i
      write(rec,'(a,es14.6,a,es14.6,a,es14.6)')&
      &'x1 = ',y(i),'; x2 = ',y(i+1),'; length = ',dx
      exp='strictly positive element length x2-x1 > 0 m'
      cause='duplicate/coincident or incorrectly ordered mesh stations'
      fix='remove duplicate positions or correct the originating input'
      call model_guided_error(errmsg,'MODEL_MESH_003',&
      &'non-positive finite-element length',loc,rec,exp,&
      &cause,fix)
      ok=-1
      return
    endif
    mt=mt+dx*real(s(i), wp)*real(rs(i), wp)
  enddo
  do i=1,nd
    mt=mt+real(md(i), wp)
  enddo
  do i=1,nmic
    mt=mt+real(vlmc(i), wp)
  enddo
  if(mt.le.0._wp) then
    write(rec,'(a,es14.6,a)') 'total physical rotor mass = ',&
    &mt,' kg'
    loc='assembled shaft + disks + concentrated masses'
    exp='total physical rotor mass > 0 kg'
    cause='all mass sources are zero, missing, or stiffness-only'
    fix='define positive shaft density and/or physical disk/mass data'
    call model_guided_error(errmsg,'MODEL_MASS_002',&
    &'rotor total physical mass is zero',loc,rec,exp,cause,fix)
    ok=-1
    return
  endif
  ok=0
  return
end subroutine model_preassembly_audit
!
subroutine model_mass_pd(a,n,pd,suspect,minpivot)
  use rd_kinds, only: lrk, wp
  implicit none
  integer :: n, suspect, i, j, k
  real(lrk) :: a(500,500)
  logical :: pd
  real(wp) :: l(500,500), diag(500), s, minpivot, tol
  pd=.false.
  suspect=0
  minpivot=1e99_wp
  do i=1,n
    if(.not.(a(i,i).eq.a(i,i)).or.a(i,i).le.0.0_lrk) then
      suspect=i
      minpivot=real(a(i,i), wp)
      return
    endif
    diag(i)=sqrt(real(a(i,i), wp))
    do j=1,n
      l(i,j)=0._wp
    enddo
  enddo
  tol=1e-9_wp
  do i=1,n
    do j=1,i
      s=real(a(i,j), wp)/(diag(i)*diag(j))
      do k=1,j-1
        s=s-l(i,k)*l(j,k)
      enddo
      if(i.eq.j) then
        minpivot=min(minpivot,s)
        if(s.le.tol) then
          suspect=i
          return
        endif
        l(i,j)=sqrt(s)
      else
        if(abs(l(j,j)).le.1e-30_wp) then
          suspect=j
          minpivot=0._wp
          return
        endif
        l(i,j)=s/l(j,j)
      endif
    enddo
  enddo
  pd=.true.
  return
end subroutine model_mass_pd
!
subroutine model_assembled_audit(pdm,std,errmsg,ok)
  use com_mat, only: mm, mg, dm, smn
  use com_maudf, only: audit_suspect, audit_pivot
  use com_mbk, only: mkb
  use com_mtk, only: mk1
  use com_sec, only: n, y, nt, nn
  use rd_kinds, only: lrk, wp
  implicit none
  integer :: pdm, ok, i, j, suspect, mtg, mpd, nf, foundation_ndof_f
  integer :: mts, sstation, kdof
  parameter(mtg=500,mts=999)
  logical :: std, pd, model_finite_r4, foundation_dynamic_f
  character(len=*) :: errmsg
  character(len=240) :: rec, loc, exp, cause, fix
  character(len=12) :: dofname
  real(wp) :: mnorm, msym, gnorm, gskew, knorm, ksym, minpivot
  real(wp) :: eratio
  real(wp) :: tiny
  parameter(tiny=1e-30_wp)
  ok=-1
  audit_suspect=0
  audit_pivot=0.0_lrk
  if(dm.lt.1.or.dm.gt.mtg.or.pdm.ne.2*dm) then
    write(rec,'(a,i0,a,i0)') 'assembled dm = ',dm,&
    &'; state dimension pdm = ',pdm
    loc='global lateral matrix assembly'
    write(exp,'(a,i0,a)') '1 <= dm <= ',mtg,&
    &' and pdm = 2*dm'
    cause='inconsistent DOF bookkeeping during model assembly'
    fix='check appended support/foundation DOFs and matrix dimensions'
    call model_guided_error(errmsg,'MODEL_MATRIX_001',&
    &'inconsistent assembled matrix dimension',loc,rec,exp,&
    &cause,fix)
    call model_write_report(std,pdm,.false.,errmsg)
    return
  endif
  mnorm=0._wp
  msym=0._wp
  gnorm=0._wp
  gskew=0._wp
  knorm=0._wp
  ksym=0._wp
  do i=1,dm
    do j=1,dm
      if(.not.model_finite_r4(mm(i,j)).or.&
      &.not.model_finite_r4(mg(i,j)).or.&
      &.not.model_finite_r4(mk1(i,j)).or.&
      &.not.model_finite_r4(mkb(i,j))) then
        write(loc,'(a,i0,a,i0,a)') 'matrix entry (',i,',',j,')'
        write(rec,'(a,4(es11.3,1x))') 'M G Ktotal Kstruct = ',&
        &mm(i,j),mg(i,j),mk1(i,j),mkb(i,j)
        exp='all assembled matrix coefficients must be finite'
        cause='invalid input/assembly produced NaN or infinite coefficient'
        fix='review the component at this DOF pair and its SI properties'
        call model_guided_error(errmsg,'MODEL_MATRIX_002',&
        &'non-finite assembled matrix entry',loc,rec,exp,cause,fix)
        call model_write_report(std,pdm,.false.,errmsg)
        return
      endif
      mnorm=max(mnorm,abs(real(mm(i,j), wp)))
      msym=max(msym,abs(real(mm(i,j), wp)-real(mm(j,i), wp)))
      gnorm=max(gnorm,abs(real(mg(i,j), wp)))
      gskew=max(gskew,abs(real(mg(i,j), wp)+real(mg(j,i), wp)))
      knorm=max(knorm,abs(real(mkb(i,j), wp)))
      ksym=max(ksym,abs(real(mkb(i,j), wp)-real(mkb(j,i), wp)))
    enddo
  enddo
  eratio=msym/max(mnorm,tiny)
  if(eratio.gt.1e-5_wp) then
    write(rec,'(a,es14.6)') 'relative symmetry error = ',eratio
    loc='global physical mass matrix M'
    exp='||M-M^T||/||M|| <= 1.0E-5'
    cause='mass assembly/indexing is inconsistent or input is corrupted'
    fix='check shaft/disk/support mass assembly near modified components'
    call model_guided_error(errmsg,'MODEL_MATRIX_003',&
    &'global mass matrix is not symmetric',loc,rec,exp,cause,fix)
    call model_write_report(std,pdm,.false.,errmsg)
    return
  endif
  eratio=gskew/max(gnorm,1._wp)
  if(eratio.gt.1e-5_wp) then
    write(rec,'(a,es14.6)') 'relative skew error = ',eratio
    loc='global gyroscopic matrix G'
    exp='||G+G^T||/max(||G||,1) <= 1.0E-5'
    cause='gyroscopic sign/index convention is inconsistent in assembly'
    fix='review disk/shaft gyroscopic terms and coordinate conventions'
    call model_guided_error(errmsg,'MODEL_MATRIX_004',&
    &'gyroscopic matrix is not skew-symmetric',loc,rec,exp,&
    &cause,fix)
    call model_write_report(std,pdm,.false.,errmsg)
    return
  endif
  eratio=ksym/max(knorm,1._wp)
  if(eratio.gt.1e-5_wp) then
    write(rec,'(a,es14.6)') 'relative symmetry error = ',eratio
    loc='conservative structural stiffness matrix'
    exp='structural shaft stiffness must be symmetric within 1.0E-5'
    cause='non-conservative/cross-coupled terms entered structural block'
    fix='keep bearing/UMP cross-coupling separate from shaft stiffness'
    call model_guided_error(errmsg,'MODEL_MATRIX_005',&
    &'structural stiffness matrix is not symmetric',loc,rec,exp,&
    &cause,fix)
    call model_write_report(std,pdm,.false.,errmsg)
    return
  endif
!     A measured dynamic-stiffness FOUNDATION is an impedance Z(f); its
!     appended interface coordinates intentionally do not carry a separate
!     physical mass matrix.  Test positive definiteness on the rotor/support
!     physical block in that case, and on the full M for PHYSICAL_MCK.
  mpd=dm
  if(foundation_dynamic_f()) then
    nf=foundation_ndof_f()
    mpd=dm-nf
  endif
  if(mpd.lt.1) then
    write(rec,'(a,i0,a,i0)') 'mass-audit dimension = ',mpd,&
    &'; assembled dm = ',dm
    loc='mass positive-definiteness audit block'
    exp='at least one physical rotor/support mass DOF'
    cause='foundation/appended DOF bookkeeping removed all physical DOFs'
    fix='check foundation NDOF mapping and rotor/support connectivity'
    call model_guided_error(errmsg,'MODEL_MATRIX_006',&
    &'invalid mass-audit matrix dimension',loc,rec,exp,cause,fix)
    call model_write_report(std,pdm,.false.,errmsg)
    return
  endif
  call model_mass_pd(mm,mpd,pd,suspect,minpivot)
  if(.not.pd) then
    audit_suspect=suspect
    audit_pivot=real(minpivot, lrk)
    dofname='unknown'
    if(suspect.gt.0.and.suspect.le.4*nt) then
      sstation=(suspect-1)/4+1
      kdof=mod(suspect-1,4)+1
      if(kdof.eq.1) dofname='x'
      if(kdof.eq.2) dofname='z'
      if(kdof.eq.3) dofname='phi'
      if(kdof.eq.4) dofname='theta'
      write(loc,'(a,i0,a,i0,a,es12.4,a,a)')&
      &'DOF ',suspect,'; station ',sstation,'; x=',y(sstation),&
      &' m; type=',trim(dofname)
    else
      write(loc,'(a,i0,a)') 'DOF ',suspect,&
      &'; appended support/foundation coordinate'
    endif
    write(rec,'(a,es14.6)')&
    &'scaled Cholesky pivot = ',minpivot
    exp='positive-definite M; every physical DOF has kinetic energy'
    cause='zero/missing mass or inertia near this DOF, or disconnected DOF'
    fix='review nearby shaft density, disk inertia, and support/foundation mass'
    call model_guided_error(errmsg,'MODEL_MASS_003',&
    &'mass matrix singular/non-PD',loc,rec,exp,&
    &cause,fix)
    call model_write_report(std,pdm,.false.,errmsg)
    return
  endif
  call model_write_report(std,pdm,.true.,'PASS')
  ok=0
  return
end subroutine model_assembled_audit
!
subroutine model_write_report(std,pdm,pass,reason)
  use com_maudi, only: mesh_modes, mesh_levels
  use com_maudl, only: mesh_en, refm_set, refcg_set
  use com_maudr, only: mesh_tol, ref_mass, ref_cg
  use rd_kinds, only: lrk
  implicit none
  logical :: std, pass
  integer :: pdm
  character(len=*) :: reason
  integer :: ios
  open(97,file='model_audit.out',status='unknown',iostat=ios)
  if(ios.eq.0) then
    rewind(97)
    call model_report_unit(97,pdm,pass,reason)
    close(97)
  endif
  if(std) call model_report_unit(6,pdm,pass,reason)
  return
end subroutine model_write_report
!
subroutine model_report_unit(iu,pdm,pass,reason)
  use com_conc, only: nmic, psic, vlmc, ixic, iyic, izic
  use com_dis, only: pdisk => pd, dd => d_d, hd => h_d, rhod => rho_d, r2, nd
  use com_doff, only: offd => off_d
  use com_dsc, only: md, idx, idy
  use com_mat, only: mm, mg, dm, smn
  use com_maudf, only: audit_suspect, audit_pivot
  use com_maudi, only: mesh_modes, mesh_levels
  use com_maudl, only: mesh_en, refm_set, refcg_set
  use com_maudr, only: mesh_tol, ref_mass, ref_cg
  use com_sea, only: rs, es, gs, dely, s, ie, ump
  use com_sec, only: n, y, nt, nn
  use com_sed, only: ds, didiv => di_d, disec => di_s
  use com_sp1, only: ns, bn
  use com_sp2, only: sup, sps
  use com_unb0, only: pr, desp, ori, np, nm
  use com_ymc, only: nbrg, rks, pc
  use rd_kinds, only: lrk, wp
  implicit none
  integer :: iu, pdm, mtg, mts, mxr, mxm, mxd, mxic
  parameter(mtg=500,mts=999,mxr=9,mxm=9,mxd=99,mxic=15)
  logical :: pass, foundation_dynamic_f
  character(len=*) :: reason
  character(len=12) :: dofname
  integer :: sstation, kdof
  integer :: i, j, near, zero_rho
  real(wp) :: dx, minl, maxl, suml, ratio, mind, maxld, ld
  real(wp) :: smass, dmass, cmass, total, prod, cg, delta
  minl=1e99_wp
  maxl=0._wp
  suml=0._wp
  mind=1e99_wp
  maxld=0._wp
  smass=0._wp
  prod=0._wp
  zero_rho=0
  do i=1,max(0,nt-1)
    dx=real(y(i+1)-y(i), wp)
    minl=min(minl,dx)
    maxl=max(maxl,dx)
    suml=suml+dx
    if(ds(i).gt.0.0_lrk) then
      ld=dx/real(ds(i), wp)
      maxld=max(maxld,ld)
    endif
    if(rs(i).eq.0.0_lrk) zero_rho=zero_rho+1
    delta=dx*real(s(i), wp)*real(rs(i), wp)
    smass=smass+delta
    prod=prod+delta*0.5_wp*real(y(i)+y(i+1), wp)
  enddo
  dmass=0._wp
  do i=1,nd
    dmass=dmass+real(md(i), wp)
    prod=prod+real(md(i), wp)*real(pdisk(i)+offd(i), wp)
  enddo
  cmass=0._wp
  do i=1,nmic
    cmass=cmass+real(vlmc(i), wp)
    prod=prod+real(vlmc(i), wp)*real(psic(i), wp)
  enddo
  total=smass+dmass+cmass
  if(total.gt.0._wp) then
    cg=prod/total
  else
    cg=0._wp
  endif
  if(nt.gt.1.and.minl.gt.0._wp) then
    ratio=maxl/minl
  else
    ratio=0._wp
    minl=0._wp
  endif
  write(iu,'(a)') '#BEGIN MODEL_AUDIT'
  if(pass) then
    write(iu,'(a)') 'STATUS              : PASS'
  else
    write(iu,'(a)') 'STATUS              : FAIL'
  endif
  write(iu,'(a,a)') 'DETAIL              : ',trim(reason)
  if(.not.pass.and.audit_suspect.gt.0) then
    write(iu,'(a,i0)') 'SUSPECT DOF         : ',audit_suspect
    write(iu,'(a,es14.6)') 'CHOLESKY PIVOT      : ',audit_pivot
    if(audit_suspect.le.4*nt) then
      sstation=(audit_suspect-1)/4+1
      kdof=mod(audit_suspect-1,4)+1
      dofname='unknown'
      if(kdof.eq.1) dofname='x'
      if(kdof.eq.2) dofname='z'
      if(kdof.eq.3) dofname='phi'
      if(kdof.eq.4) dofname='theta'
      write(iu,'(a,i0)') 'SUSPECT STATION     : ',sstation
      write(iu,'(a,es14.6)') 'SUSPECT AXIAL (m)   : ',y(sstation)
      write(iu,'(a,a)') 'SUSPECT KIND        : ',trim(dofname)
    else
      write(iu,'(a)') 'SUSPECT KIND        : appended model DOF'
    endif
  endif
  write(iu,'(a,i0)') 'STATIONS            : ',nt
  write(iu,'(a,i0)') 'ELEMENTS            : ',max(0,nt-1)
  write(iu,'(a,i0)') 'LATERAL DOF         : ',dm
  write(iu,'(a,i0)') 'STATE MATRIX DIM    : ',pdm
  if(foundation_dynamic_f()) then
    write(iu,'(a)') 'MASS PD SCOPE       : rotor/support '//&
    &'(foundation supplied as Z(f))'
  else
    write(iu,'(a)') 'MASS PD SCOPE       : full physical M'
  endif
  write(iu,'(a,es14.6)') 'SHAFT MASS (kg)     : ',smass
  write(iu,'(a,es14.6)') 'DISK MASS (kg)      : ',dmass
  write(iu,'(a,es14.6)') 'CONC MASS (kg)      : ',cmass
  write(iu,'(a,es14.6)') 'ROTOR MASS (kg)     : ',total
  write(iu,'(a,es14.6)') 'ROTOR CG (m)        : ',cg
  if(refm_set) then
    delta=(total-real(ref_mass, wp))/real(ref_mass, wp)
    write(iu,'(a,es14.6)') 'REF MASS (kg)       : ',ref_mass
    write(iu,'(a,f10.4,a)') 'MASS DELTA          : ',100._wp*delta,' %'
  endif
  if(refcg_set) then
    write(iu,'(a,es14.6)') 'REF CG (m)          : ',ref_cg
    write(iu,'(a,es14.6)') 'CG DELTA (m)        : ',cg-ref_cg
  endif
  write(iu,'(a,es14.6)') 'MIN ELEMENT (m)     : ',minl
  write(iu,'(a,es14.6)') 'MAX ELEMENT (m)     : ',maxl
  if(nt.gt.1) then
    write(iu,'(a,es14.6)') 'MEAN ELEMENT (m)    : ',suml/real(nt-1, wp)
  endif
  write(iu,'(a,f10.3)') 'MAX/MIN LENGTH      : ',ratio
  write(iu,'(a,f10.3)') 'MAX L/D             : ',maxld
  if(ratio.gt.5._wp) then
    write(iu,'(a)') 'MESH WARNING        : max/min element '//&
    &'length > 5'
  endif
  write(iu,'(a,i0)') 'ZERO-RHO ELEMENTS   : ',zero_rho
  write(iu,'(a,i0)') 'BEARINGS            : ',nbrg
  write(iu,'(a,i0)') 'FLEX SUPPORTS       : ',ns
  if(nbrg.eq.0) then
    write(iu,'(a)')&
    &'SUPPORT WARNING     : no lateral bearings (free-free?)'
  endif
  if(mesh_en) then
    write(iu,'(a)') 'MESH AUDIT          : REQUESTED'
    write(iu,'(a,i0)') 'MESH MODES          : ',mesh_modes
    write(iu,'(a,i0)') 'MESH LEVELS         : ',mesh_levels
    write(iu,'(a,f10.6)') 'MESH TOL            : ',mesh_tol
    write(iu,'(a)')&
    &'MESH CONVERGENCE   : run scripts/run_mesh_audit.py'
  else
    write(iu,'(a)') 'MESH AUDIT          : OFF'
  endif
  write(iu,'(a)') 'RESPONSE POSITIONS'
  do i=1,np
    if(pr(i).lt.0.0_lrk) then
      write(iu,'(i4,a,f12.6,a)') i,' requested=',pr(i),&
      &' support-reference'
    else
      near=1
      do j=2,nt
        if(abs(y(j)-pr(i)).lt.abs(y(near)-pr(i))) near=j
      enddo
      write(iu,'(i4,a,f12.6,a,f12.6,a,i0)') i,&
      &' requested=',pr(i),' actual=',y(near),' station=',near
    endif
  enddo
  write(iu,'(a)') '#END MODEL_AUDIT'
  return
end subroutine model_report_unit
