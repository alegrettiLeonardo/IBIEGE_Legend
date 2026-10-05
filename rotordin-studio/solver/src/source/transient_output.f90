!
!     Single, structured output stream for the true transient solver.
!
subroutine transient_remove_obsolete_outputs(errmsg,ok)
  implicit none
  integer :: ok, i, ios
  logical :: exists
  character(len=*) :: errmsg
  character(len=40) :: names(6)
  data names/&
  &'transient_response.csv',&
  &'transient_orbits.csv',&
  &'transient_bearing_forces.csv',&
  &'transient_foundation_response.csv',&
  &'transient_timestep_convergence.csv',&
  &'transient_matrix_dump.csv'/
  do i=1,6
    inquire(file=trim(names(i)),exist=exists)
    if(exists) then
      open(88,file=trim(names(i)),status='old',iostat=ios)
      if(ios.ne.0) goto 900
      close(88,status='delete',iostat=ios)
      if(ios.ne.0) goto 900
    endif
  enddo
  ok=0
  return
900 errmsg='transient: cannot remove obsolete output file '&
  &//trim(names(i))
  ok=-1
  return
end subroutine transient_remove_obsolete_outputs
!
subroutine transient_output_begin(std,iu,errmsg,ok)
  implicit none
  logical :: std
  integer :: iu, ios, ok, oki
  character(len=*) :: errmsg
  character(len=9) :: section
  section='transient'
  ok=-1
  call transient_remove_obsolete_outputs(errmsg,oki)
  if(oki.ne.0) return
  if(std) then
    iu=6
    call marksec(section,0)
  else
    iu=86
    open(iu,file='transient.out',status='replace',iostat=ios)
    if(ios.ne.0) then
      errmsg='transient: cannot create transient.out'
      return
    endif
    write(iu,'(A)',iostat=ios) '#BEGIN transient.out'
    if(ios.ne.0) then
      close(iu)
      errmsg='transient: cannot begin transient.out'
      return
    endif
  endif
  ok=0
  return
end subroutine transient_output_begin
!
subroutine transient_output_header(iu,errmsg,ok)
  use com_knd, only: cknd, supr
  use com_trnctl, only: trn_enabled, trn_gravity, trn_unbalance, trn_convergence, trn_model, trn_speed_kind, &
    & trn_init_kind, trn_stride, trn_nprof, trn_nic, trn_nforce, trn_out_policy, trn_monitor_dof, trn_analysis_mode
  use com_trnforce, only: trn_fdof, trn_fnpt, trn_ftime, trn_fvalue
  use com_trnic, only: trn_icdof, trn_icq, trn_icv
  use com_trnprof, only: trn_ptime, trn_prpm, trn_pphase
  use com_trntime, only: trn_tstart, trn_tend, trn_dt, trn_beta, trn_gamma, trn_phase0, trn_audit_hz
  use com_ymc, only: nbr => nbrg, rks, pc
  use rd_kinds, only: lrk, wp
  implicit none
  integer :: trn_mprof, trn_mic, trn_mforce, trn_mfpt
  parameter(trn_mprof=999,trn_mic=500,trn_mforce=20,&
  &trn_mfpt=999)
  integer :: mxm
  parameter(mxm=9)
  integer :: iu, ok, i, nstep
  character(len=*) :: errmsg
  character(len=24) :: model, profile, initial
  character(len=16) :: enabled
  real(wp) :: rpm0, rpm1, rpmmin, rpmmax, slope
  logical :: foundation_active_f
  call transient_model_text(trn_model,model)
  if(trn_speed_kind.eq.1) then
    profile='CONSTANT'
  else if(trn_speed_kind.eq.2) then
    profile='LINEAR'
  else
    profile='TABLE'
  endif
  if(trn_init_kind.eq.1) then
    initial='ZERO'
  else if(trn_init_kind.eq.2) then
    initial='USER'
  else
    initial='STATIC_EQUILIBRIUM'
  endif
  rpm0=trn_prpm(1)
  rpm1=trn_prpm(trn_nprof)
  rpmmin=rpm0
  rpmmax=rpm0
  do i=2,trn_nprof
    rpmmin=min(rpmmin,trn_prpm(i))
    rpmmax=max(rpmmax,trn_prpm(i))
  enddo
  nstep=nint((trn_tend-trn_tstart)/trn_dt)
  write(iu,'(A)',err=900)&
  &'ROTORDIN - TRUE LATERAL TRANSIENT ANALYSIS'
  write(iu,'(A)',err=900) 'ANALYSIS_TYPE TRUE_TRANSIENT'
  write(iu,'(A,1X,A)',err=900) 'MODEL',trim(model)
  write(iu,'(A)',err=900) 'INTEGRATOR NEWMARK_BETA'
  write(iu,'(A,1X,ES24.16)',err=900) 'BETA',trn_beta
  write(iu,'(A,1X,ES24.16)',err=900) 'GAMMA',trn_gamma
  write(iu,'(A,1X,ES24.16)',err=900)&
  &'START_TIME_S',trn_tstart
  write(iu,'(A,1X,ES24.16)',err=900) 'END_TIME_S',trn_tend
  write(iu,'(A,1X,ES24.16)',err=900) 'TIME_STEP_S',trn_dt
  write(iu,'(A,1X,I0)',err=900) 'NUMBER_OF_STEPS',nstep
  write(iu,'(A,1X,I0)',err=900) 'OUTPUT_STRIDE',trn_stride
  write(iu,'(A,1X,A)',err=900) 'SPEED_PROFILE',trim(profile)
  write(iu,'(A,1X,ES24.16)',err=900) 'INITIAL_RPM',rpm0
  write(iu,'(A,1X,ES24.16)',err=900) 'FINAL_RPM',rpm1
  write(iu,'(A,1X,ES24.16)',err=900) 'MINIMUM_RPM',rpmmin
  write(iu,'(A,1X,ES24.16)',err=900) 'MAXIMUM_RPM',rpmmax
  write(iu,'(A,1X,ES24.16)',err=900)&
  &'INITIAL_PHASE_RAD',trn_phase0
  if(trn_speed_kind.eq.2) then
    slope=(rpm1-rpm0)/(trn_tend-trn_tstart)
    if(trn_model.eq.3) then
      write(iu,'(A,1X,ES24.16)',err=900)&
      &'DECELERATION_RPM_S',slope
    else
      write(iu,'(A,1X,ES24.16)',err=900)&
      &'ACCELERATION_RPM_S',slope
    endif
  endif
  write(iu,'(A,1X,I0)',err=900) 'PROFILE_POINTS',trn_nprof
  write(iu,'(A,1X,A)',err=900)&
  &'INITIAL_CONDITION',trim(initial)
  enabled='NO'
  if(trn_gravity) enabled='YES'
  write(iu,'(A,1X,A)',err=900) 'GRAVITY',trim(enabled)
  enabled='NO'
  if(trn_unbalance) enabled='YES'
  write(iu,'(A,1X,A)',err=900) 'UNBALANCE',trim(enabled)
  enabled='NO'
  if(trn_nforce.gt.0) enabled='YES'
  write(iu,'(A,1X,A)',err=900) 'GENERAL_FORCE',trim(enabled)
  enabled='NO'
  if(nbr.gt.0) enabled='YES'
  write(iu,'(A,1X,A)',err=900)&
  &'BEARINGS_ACTIVE',trim(enabled)
  enabled='NO'
  if(nbr.gt.0 .and. cknd.ne.'0') enabled='YES'
  write(iu,'(A,1X,A)',err=900)&
  &'SPEED_DEPENDENT_BEARINGS',trim(enabled)
  enabled='CONSTANT'
  if(nbr.eq.0) enabled='NONE'
  if(nbr.gt.0 .and. cknd.ne.'0') enabled='SPEED_DEPENDENT'
  write(iu,'(A,1X,A)',err=900)&
  &'BEARING_COEFFICIENTS',trim(enabled)
  enabled='INACTIVE'
  if(foundation_active_f()) enabled='ACTIVE'
  write(iu,'(A,1X,A)',err=900) 'FOUNDATION',trim(enabled)
  enabled='DISABLED'
  if(trn_convergence) enabled='ENABLED'
  write(iu,'(A,1X,A)',err=900)&
  &'TIMESTEP_CONVERGENCE',trim(enabled)
  write(iu,'(A)',err=900) 'SPEED_PROFILE_BEGIN'
  write(iu,'(A)',err=900) 'SPEED_PROFILE_COLUMNS TIME_S RPM'
  if(trn_nprof.eq.1) then
    write(iu,'(A,1X,2(ES24.16,1X))',err=900)&
    &'SPEED_POINT',trn_tstart,trn_prpm(1)
    write(iu,'(A,1X,2(ES24.16,1X))',err=900)&
    &'SPEED_POINT',trn_tend,trn_prpm(1)
  else
    do i=1,trn_nprof
      write(iu,'(A,1X,2(ES24.16,1X))',err=900)&
      &'SPEED_POINT',trn_ptime(i),trn_prpm(i)
    enddo
  endif
  write(iu,'(A)',err=900) 'SPEED_PROFILE_END'
  ok=0
  return
900 errmsg='transient: failed while writing output header'
  ok=-1
  return
end subroutine transient_output_header
!
subroutine transient_output_time_history_begin(iu,has_bearing,&
&has_foundation,errmsg,ok)
  implicit none
  integer :: iu, ok
  logical :: has_bearing, has_foundation
  character(len=*) :: errmsg
  write(iu,'(A)',err=900) 'TIME_HISTORY_BEGIN'
  write(iu,'(A)',err=900) 'STATE_COLUMNS TIME_S RPM '&
  &//'OMEGA_RAD_S PHASE_RAD POSITION_M DOF DISPLACEMENT_M '&
  &//'VELOCITY_M_S ACCELERATION_M_S2'
  write(iu,'(A)',err=900)&
  &'ORBIT_COLUMNS TIME_S RPM POSITION_M X_M Z_M'
  write(iu,'(A)',err=900)&
  &'FLEXDISK_COLUMNS TIME_S RPM DISK POSITION_M THETA_X_RAD '&
  &//'THETA_Z_RAD REL_THETA_X_RAD REL_THETA_Z_RAD'
  if(has_bearing) write(iu,'(A)',err=900)&
  &'BEARING_COLUMNS TIME_S RPM BEARING POSITION_M FORCE_X_N '&
  &//'FORCE_Z_N'
  if(has_foundation) write(iu,'(A)',err=900)&
  &'FOUNDATION_COLUMNS TIME_S RPM FOUNDATION_DOF DISPLACEMENT '&
  &//'VELOCITY ACCELERATION'
  ok=0
  return
900 errmsg='transient: cannot begin time history'
  ok=-1
  return
end subroutine transient_output_time_history_begin
!
subroutine transient_output_time_history_end(iu,errmsg,ok)
  implicit none
  integer :: iu, ok
  character(len=*) :: errmsg
  write(iu,'(A)',err=900) 'TIME_HISTORY_END'
  ok=0
  return
900 errmsg='transient: cannot end time history'
  ok=-1
  return
end subroutine transient_output_time_history_end
!
subroutine transient_dump_matrices(iu,dm,mass,cphys,kphys,&
&errmsg,ok)
  use rd_kinds, only: wp
  implicit none
  integer :: mtg
  parameter(mtg=500)
  integer :: iu, dm, ok, i, j, envstat
  real(wp) :: mass(mtg,mtg), cphys(mtg,mtg)
  real(wp) :: kphys(mtg,mtg)
  character(len=*) :: errmsg
  character(len=8) :: enabled
  enabled=' '
  call get_environment_variable('ROTORDIN_TRANSIENT_MATRIX_DUMP',&
  &enabled,status=envstat)
  if(envstat.ne.0 .or. len_trim(enabled).eq.0) then
    ok=0
    return
  endif
  write(iu,'(A)',err=900) 'MATRIX_DUMP_BEGIN'
  write(iu,'(A)',err=900) 'MATRIX_COLUMNS ROW COLUMN MASS '&
  &//'STIFFNESS DAMPING'
  do j=1,dm
    do i=1,dm
      write(iu,'(A,1X,2(I0,1X),3(ES24.16,1X))',err=900)&
      &'MATRIX',i,j,mass(i,j),kphys(i,j),cphys(i,j)
    enddo
  enddo
  write(iu,'(A)',err=900) 'MATRIX_DUMP_END'
  ok=0
  return
900 errmsg='transient: cannot write diagnostic matrix dump'
  ok=-1
  return
end subroutine transient_dump_matrices
!
subroutine transient_bearing_output(iu,dm,pp,t,rpm,omega,q,v,&
&counts,errmsg,ok)
  use com_sp1, only: ns, bn
  use com_sp2, only: sup, sps
  use com_ymc, only: nbr => nbrg, rks, pc
  use rd_kinds, only: lrk, wp
  implicit none
  integer :: mtg, mxm
  parameter(mtg=500,mxm=9)
  integer :: iu, dm, pp, ok, oki, i, j, k, rx, gx, gz, xd, zd
  integer :: inddis, ifidxof, support_dof_x
  integer :: foundation_global_dof, counts(4)
  real(lrk) :: par(mxm,10)
  real(wp) :: t, rpm, omega, q(mtg), v(mtg), dx, dz, dvx, dvz
  real(wp) :: fx, fz
  logical :: found
  character(len=*) :: errmsg
  ok=-1
  call parman(real(omega, lrk),.true.,mxm,nbr,par,errmsg,oki)
  if(oki.ne.0) return
  k=0
  do i=1,nbr
    rx=inddis(pc(i),errmsg,oki)
    if(oki.lt.0) return
    rx=ifidxof(rx,1)
    gx=0
    gz=0
    do j=1,ns
      if(bn(j).eq.i) then
        k=k+1
        gx=support_dof_x(dm,pp,k)
        gz=gx+1
        exit
      endif
    enddo
    if(gx.eq.0) then
      call foundation_interface(2,i,xd,zd,found)
      if(found) then
        gx=foundation_global_dof(dm,xd)
        gz=foundation_global_dof(dm,zd)
      endif
    endif
    dx=q(rx)
    dz=q(rx+1)
    dvx=v(rx)
    dvz=v(rx+1)
    if(gx.gt.0) then
      dx=dx-q(gx)
      dz=dz-q(gz)
      dvx=dvx-v(gx)
      dvz=dvz-v(gz)
    endif
    fx=-(real(par(i,1), wp)*dx+real(par(i,2), wp)*dz+real(par(i,5), wp)*dvx+real(par(i,6), wp)*dvz)
    fz=-(real(par(i,3), wp)*dx+real(par(i,4), wp)*dz+real(par(i,7), wp)*dvx+real(par(i,8), wp)*dvz)
    write(iu,'(A,1X,2(ES24.16,1X),I0,1X,3(ES24.16,1X))',err=900) 'BEARING',t,rpm,i,real(pc(i), wp),fx,fz
    counts(3)=counts(3)+1
  enddo
  ok=0
  return
900 errmsg='transient: cannot write bearing history'
  ok=-1
  return
end subroutine transient_bearing_output
!
subroutine transient_output_state(iu,dm,pp,t,rpm,omega,phase,&
&q,v,a,counts,errmsg,ok)
  use com_dflexi, only: dmodel, dfdofx, dfdofz, nfdisk, nshaftfd
  use com_dis, only: pd, dd => d_d, hd => h_d, rho => rho_d, r2, nd
  use com_sec, only: n, y, nt, nn
  use com_ymc, only: nbr => nbrg, rks, pc
  use rd_kinds, only: lrk, wp
  implicit none
  integer :: mtg, mts, mxm
  parameter(mtg=500,mts=999,mxm=9)
  integer :: iu, dm, pp, ok, i, j, node, idof, nrot, nshaft, nf
  integer :: counts(4), foundation_ndof_f, shaft_ndof_f
  integer :: rotating_ndof_f, inddis, ifidxof
  integer :: mxd
  parameter(mxd=99)
  real(lrk) :: pos
  integer :: oki, station
  real(wp) :: rtx, rtz
  real(wp) :: t, rpm, omega, phase, q(mtg), v(mtg), a(mtg)
  character(len=*) :: errmsg
  nf=foundation_ndof_f()
  nshaft=shaft_ndof_f()
  nrot=rotating_ndof_f()
  do i=1,dm
    if(i.le.nshaft) then
      node=(i-1)/4+1
      idof=mod(i-1,4)+1
      if(node.le.nt) then
        pos=y(node)
      else
        pos=0._lrk
      endif
    else if(i.le.nrot) then
      node=0
      idof=i
      pos=0._lrk
    else if(i.le.nrot+2*pp) then
      node=(i-nrot-1)/2+1
      idof=i
      pos=-real(node, lrk)
    else
      idof=i
      pos=0._lrk
    endif
    write(iu,'(A,1X,5(ES24.16,1X),I0,1X,3(ES24.16,1X))',err=900) 'STATE',t,rpm,omega,phase,real(pos, wp),idof,q(i), &
      & v(i),a(i)
    counts(1)=counts(1)+1
  enddo
  do i=1,nshaft,4
    node=(i-1)/4+1
    if(node.le.nt) then
      pos=y(node)
    else
      pos=0._lrk
    endif
    write(iu,'(A,1X,5(ES24.16,1X))',err=900)'ORBIT',t,rpm,real(pos, wp),q(i),q(i+1)
    counts(2)=counts(2)+1
  enddo
!     Flexible-disk rotations and relative rotations to shaft station.
  do j=1,nd
    if(dmodel(j).eq.1) then
      station=inddis(pd(j),errmsg,oki)
      if(oki.lt.0) return
      i=ifidxof(station,0)
      rtx=q(dfdofx(j))-q(i+3)
      rtz=q(dfdofz(j))-q(i+4)
      write(iu,'(A,1X,2(ES24.16,1X),I0,1X,5(ES24.16,1X))',err=900) 'FLEXDISK',t,rpm,j,real(pd(j), wp),q(dfdofx(j)), &
        & q(dfdofz(j)),rtx,rtz
    endif
  enddo
  if(nbr.gt.0) then
    call transient_bearing_output(iu,dm,pp,t,rpm,omega,q,v,&
    &counts,errmsg,ok)
    if(ok.ne.0) return
  endif
  if(nf.gt.0) then
    do i=1,nf
      node=dm-nf+i
      write(iu,'(A,1X,2(ES24.16,1X),I0,1X,'&
      &//'3(ES24.16,1X))',err=900)&
      &'FOUNDATION',t,rpm,i,q(node),v(node),a(node)
      counts(4)=counts(4)+1
    enddo
  endif
  ok=0
  return
900 errmsg='transient: cannot write time history'
  ok=-1
  return
end subroutine transient_output_state
!
subroutine transient_write_convergence(iu,dt,metrics,errmsg,ok)
  use rd_kinds, only: wp
  implicit none
  real(wp) :: dt, metrics(8,3), den, rel
  integer :: iu, i, j, ok
  character(len=*) :: errmsg
  write(iu,'(A)',err=900) 'TIMESTEP_CONVERGENCE_BEGIN'
  write(iu,'(A)',err=900) 'CONVERGENCE_COLUMNS DT_S '&
  &//'PEAK_DISPLACEMENT_M PHASE_RAD CROSSING_RESPONSE_M RMS_M '&
  &//'FINAL_STATE_NORM PEAK_RPM PEAK_TIME_S PEAK_POSITION_M '&
  &//'RELATIVE_PEAK_TO_FINEST'
  den=max(abs(metrics(1,3)),1e-30_wp)
  do j=1,3
    rel=abs(metrics(1,j)-metrics(1,3))/den
    write(iu,'(A,1X,10(ES24.16,1X))',err=900)'CONVERGENCE',dt/(2._wp**(j-1)),(metrics(i,j),i=1,8),rel
  enddo
  write(iu,'(A)',err=900) 'TIMESTEP_CONVERGENCE_END'
  ok=0
  return
900 errmsg='transient: cannot write timestep convergence'
  ok=-1
  return
end subroutine transient_write_convergence
!
subroutine transient_output_summary(iu,metrics,counts,errmsg,ok)
  use rd_kinds, only: wp
  implicit none
  integer :: iu, counts(4), ok
  real(wp) :: metrics(8)
  character(len=*) :: errmsg
  write(iu,'(A)',err=900) 'TRANSIENT_SUMMARY_BEGIN'
  write(iu,'(A,1X,ES24.16)',err=900)&
  &'MAXIMUM_RESPONSE_M',metrics(1)
  write(iu,'(A,1X,ES24.16)',err=900)&
  &'MAXIMUM_RESPONSE_POSITION_M',metrics(8)
  write(iu,'(A,1X,ES24.16)',err=900)&
  &'MAXIMUM_RESPONSE_TIME_S',metrics(7)
  write(iu,'(A,1X,ES24.16)',err=900)&
  &'MAXIMUM_RESPONSE_RPM',metrics(6)
  write(iu,'(A,1X,I0)',err=900) 'STATE_RECORDS',counts(1)
  write(iu,'(A,1X,I0)',err=900) 'ORBIT_RECORDS',counts(2)
  write(iu,'(A,1X,I0)',err=900) 'BEARING_RECORDS',counts(3)
  write(iu,'(A,1X,I0)',err=900) 'FOUNDATION_RECORDS',counts(4)
  write(iu,'(A)',err=900) 'TRANSIENT_SUMMARY_END'
  ok=0
  return
900 errmsg='transient: cannot write transient summary'
  ok=-1
  return
end subroutine transient_output_summary
!
subroutine transient_output_end(std,iu,success,message)
  implicit none
  logical :: std, success
  integer :: iu
  character(len=*) :: message
  character(len=9) :: section
  section='transient'
  if(success) then
    write(iu,'(A)') 'STATUS SUCCESS'
  else
    write(iu,'(A)') 'STATUS ERROR'
    write(iu,'(A,1X,A)') 'ERROR_MESSAGE',trim(message)
  endif
  if(std) then
    call marksec(section,1)
  else
    write(iu,'(A)') '#END transient.out'
    close(iu)
  endif
  return
end subroutine transient_output_end
