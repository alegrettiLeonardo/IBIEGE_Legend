!
!     True lateral time-transient solver in physical coordinates.
!
subroutine transient_time_step_audit(iu,errmsg,ok)
  use com_trnctl, only: trn_enabled, trn_gravity, trn_unbalance, trn_convergence, trn_model, trn_speed_kind, &
    & trn_init_kind, trn_stride, trn_nprof, trn_nic, trn_nforce, trn_out_policy, trn_monitor_dof, trn_analysis_mode
  use com_trnforce, only: trn_fdof, trn_fnpt, trn_ftime, trn_fvalue
  use com_trnic, only: trn_icdof, trn_icq, trn_icv
  use com_trnprof, only: trn_ptime, trn_prpm, trn_pphase
  use com_trntime, only: trn_tstart, trn_tend, trn_dt, trn_beta, trn_gamma, trn_phase0, trn_audit_hz
  use rd_kinds, only: wp
  implicit none
  integer :: trn_mprof, trn_mic, trn_mforce, trn_mfpt
  parameter(trn_mprof=999,trn_mic=500,trn_mforce=20,&
  &trn_mfpt=999)
  integer :: iu, ok, i
  real(wp) :: freq, period, limit, steps, rmax
  character(len=*) :: errmsg
  character(len=7) :: status
  freq=trn_audit_hz
  if(freq.le.0._wp) then
    rmax=0._wp
    do i=1,trn_nprof
      rmax=max(rmax,abs(trn_prpm(i)))
    enddo
    freq=rmax/60._wp
  endif
  write(iu,'(A)') 'TIME_STEP_AUDIT_BEGIN'
  write(iu,'(A,1X,ES24.16)') 'USER_DT_S',trn_dt
  if(freq.le.0._wp) then
    write(iu,'(A)')&
    &'HIGHEST_RELEVANT_FREQUENCY_HZ NOT_AVAILABLE'
    write(iu,'(A)') 'AUDIT_STATUS WARNING'
    write(iu,'(A)') 'TIME_STEP_AUDIT_END'
    ok=0
    return
  endif
  period=1._wp/freq
  limit=period/20._wp
  steps=period/trn_dt
  if(steps.ge.20._wp) then
    status='PASS'
    ok=0
  else if(steps.ge.5._wp) then
    status='WARNING'
    ok=0
  else
    status='ERROR'
    ok=-1
    errmsg='transient: fewer than five steps per relevant period'
  endif
  write(iu,'(A,1X,ES24.16)')&
  &'HIGHEST_RELEVANT_FREQUENCY_HZ',freq
  write(iu,'(A,1X,ES24.16)') 'REFERENCE_PERIOD_S',period
  write(iu,'(A,1X,ES24.16)')&
  &'CHEN_T20_GUIDELINE_S',limit
  write(iu,'(A,1X,ES24.16)') 'STEPS_PER_PERIOD',steps
  write(iu,'(A,1X,A)') 'AUDIT_STATUS',status
  write(iu,'(A)') 'TIME_STEP_AUDIT_END'
  return
end subroutine transient_time_step_audit
!
subroutine transient_measure(dm,pp,q,amplitude,position)
  use com_sec, only: n, y, nt, nn
  use rd_kinds, only: lrk, wp
  implicit none
  integer :: mtg, mts
  parameter(mtg=500,mts=999)
  integer :: dm, pp, nshaft, i, node, shaft_ndof_f
  real(wp) :: q(mtg), amplitude, value, position
  nshaft=shaft_ndof_f()
  amplitude=0._wp
  position=0._wp
  do i=1,nshaft,4
    value=sqrt(q(i)*q(i)+q(i+1)*q(i+1))
    if(value.gt.amplitude) then
      amplitude=value
      node=(i-1)/4+1
      if(node.le.nt) position=real(y(node), wp)
    endif
  enddo
  return
end subroutine transient_measure
!
subroutine transient_run(iu,pp,run_dt,write_output,metrics,&
&counts,errmsg,ok)
  use com_mat, only: mm, mg, dm, smn
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
  integer :: mtg, mxm
  parameter(mtg=500,mxm=9)
  integer :: iu, pp, ok, oki, i, j, istep, nstep, ipiv(mtg)
  integer :: counts(4)
  real(wp) :: run_dt, metrics(8)
  logical :: write_output, matrix_constant, has_foundation
  logical :: transient_finite_vector, foundation_active_f
  real(wp) :: mass(mtg,mtg), gyro(mtg,mtg)
  real(wp) :: kbase(mtg,mtg), cbase(mtg,mtg)
  real(wp) :: kphys(mtg,mtg), cphys(mtg,mtg)
  real(wp) :: kn(mtg,mtg), work(mtg,mtg)
  real(wp) :: q(mtg), v(mtg), a(mtg), force(mtg), rhs(mtg)
  real(wp) :: qn(mtg), vn(mtg), an(mtg)
  real(wp) :: t, rpm, omega, alpha, phase, amp, pos
  real(wp) :: sumsq, finalnorm, crossing, finalphase
  character(len=*) :: errmsg
  character(len=32) :: pan
  ok=-1
  nstep=nint((trn_tend-trn_tstart)/run_dt)
  if(write_output) then
    pan='TRUE_TRANSIENT'
  else if(run_dt.gt.0.375_wp*trn_dt) then
    pan='TRANSIENT_DT2'
  else
    pan='TRANSIENT_DT4'
  endif
  call progress_begin(pan,nstep,&
  &'Newmark time integration in physical coordinates')
  call progress_stage(pan,'initialize speed state and matrices')
  matrix_constant=trn_speed_kind.eq.1
  do i=1,mtg
    q(i)=0._wp
    v(i)=0._wp
    a(i)=0._wp
  enddo
  call transient_speed_state(trn_tstart,rpm,omega,alpha,phase,&
  &errmsg,oki)
  if(oki.ne.0) return
  call transient_matrix_update(dm,pp,rpm,omega,alpha,mass,gyro,&
  &kbase,cbase,kphys,cphys,errmsg,oki)
  if(oki.ne.0) return
  if(write_output) then
    call transient_dump_matrices(iu,dm,mass,cphys,kphys,&
    &errmsg,oki)
    if(oki.ne.0) return
  endif
  if(trn_init_kind.eq.2) then
    do i=1,trn_nic
      q(trn_icdof(i))=trn_icq(i)
      v(trn_icdof(i))=trn_icv(i)
    enddo
  else if(trn_init_kind.eq.3) then
    call transient_force(dm,pp,mass,trn_tstart,omega,alpha,phase,&
    &q,v,.true.,force,errmsg,oki)
    if(oki.ne.0) return
    call transient_solve_system(dm,mtg,kbase,force,q,work,ipiv,&
    &errmsg,oki)
    if(oki.ne.0) then
      errmsg='transient: STATIC_EQUILIBRIUM failed: '//trim(errmsg)
      return
    endif
  endif
  call transient_force(dm,pp,mass,trn_tstart,omega,alpha,phase,&
  &q,v,.false.,force,errmsg,oki)
  if(oki.ne.0) return
  call transient_initial_acceleration(dm,mtg,mass,cphys,kphys,&
  &force,q,v,a,work,rhs,ipiv,errmsg,oki)
  if(oki.ne.0) return
  if(matrix_constant) then
    call progress_stage(pan,'factor effective Newmark matrix')
    call transient_newmark_factor(dm,mtg,mass,cphys,kphys,run_dt,&
    &trn_beta,trn_gamma,kn,ipiv,errmsg,oki)
    if(oki.ne.0) return
  endif
  has_foundation=foundation_active_f()
  if(write_output) then
    call transient_output_time_history_begin(iu,nbr.gt.0,&
    &has_foundation,errmsg,oki)
    if(oki.ne.0) return
    call transient_output_state(iu,dm,pp,trn_tstart,rpm,omega,&
    &phase,q,v,a,counts,errmsg,oki)
    if(oki.ne.0) return
  endif
  call transient_measure(dm,pp,q,amp,pos)
  metrics(1)=amp
  metrics(6)=rpm
  metrics(7)=trn_tstart
  metrics(8)=pos
  sumsq=q(trn_monitor_dof)*q(trn_monitor_dof)
  crossing=0._wp
  do istep=1,nstep
    t=trn_tstart+real(istep, wp)*run_dt
    call transient_speed_state(t,rpm,omega,alpha,phase,errmsg,oki)
    if(oki.ne.0) return
    call progress_update2(pan,istep,nstep,real(t, lrk),'TIME_S',real(rpm, lrk),'RPM')
    if(.not.matrix_constant) then
      call transient_matrix_update(dm,pp,rpm,omega,alpha,mass,gyro,&
      &kbase,cbase,kphys,cphys,errmsg,oki)
      if(oki.ne.0) return
      call transient_newmark_factor(dm,mtg,mass,cphys,kphys,run_dt,&
      &trn_beta,trn_gamma,kn,ipiv,errmsg,oki)
      if(oki.ne.0) return
    endif
    call transient_force(dm,pp,mass,t,omega,alpha,phase,q,v,.false.,&
    &force,errmsg,oki)
    if(oki.ne.0) return
    call transient_newmark_step(dm,mtg,mass,cphys,run_dt,&
    &trn_beta,trn_gamma,kn,ipiv,force,q,v,a,qn,vn,an,rhs,&
    &errmsg,oki)
    if(oki.ne.0) return
    if(.not.transient_finite_vector(dm,qn) .or.&
    &.not.transient_finite_vector(dm,vn) .or.&
    &.not.transient_finite_vector(dm,an)) then
      errmsg='transient: NaN or Inf detected in integrated state'
      return
    endif
    do i=1,dm
      q(i)=qn(i)
      v(i)=vn(i)
      a(i)=an(i)
    enddo
    call transient_measure(dm,pp,q,amp,pos)
    if(amp.gt.metrics(1)) then
      metrics(1)=amp
      metrics(6)=rpm
      metrics(7)=t
      metrics(8)=pos
    endif
    sumsq=sumsq+q(trn_monitor_dof)*q(trn_monitor_dof)
    if(istep.eq.nstep/2) crossing=amp
    if(write_output .and.&
    &(mod(istep,trn_stride).eq.0 .or. istep.eq.nstep)) then
      call transient_output_state(iu,dm,pp,t,rpm,omega,phase,&
      &q,v,a,counts,errmsg,oki)
      if(oki.ne.0) return
    endif
  enddo
  finalnorm=0._wp
  do i=1,dm
    finalnorm=finalnorm+q(i)*q(i)
  enddo
  finalnorm=sqrt(finalnorm)
  if(dm.ge.2) then
    finalphase=atan2(q(2),q(1))
  else
    finalphase=0._wp
  endif
  metrics(2)=finalphase
  metrics(3)=crossing
  metrics(4)=sqrt(sumsq/real(nstep+1, wp))
  metrics(5)=finalnorm
  if(write_output) then
    call transient_output_time_history_end(iu,errmsg,oki)
    if(oki.ne.0) return
  endif
  ok=0
  call progress_end(pan,'OK')
  return
end subroutine transient_run
!
subroutine transient_model_text(model,text)
  implicit none
  integer :: model
  character(len=*) :: text
  if(model.eq.1) then
    text='CONSTANT_SPEED'
  else if(model.eq.2) then
    text='STARTUP'
  else if(model.eq.3) then
    text='COASTDOWN'
  else
    text='USER_PROFILE'
  endif
  return
end subroutine transient_model_text
!
subroutine transient_solver(pp,std,errmsg,ok)
  use com_mat, only: mm, mg, dm, smn
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
  integer :: mtg, mxm
  parameter(mtg=500,mxm=9)
  integer :: pp, ok, oki, nstep, j, iu, counts(4)
  logical :: std, foundation_active_f, opened
  real(wp) :: metrics(8,3), rpm0, om0, al0, ph0
  real(wp) :: rpm1, om1, al1, ph1
  character(len=*) :: errmsg
  character(len=24) :: model
  opened=.false.
  if(.not.trn_enabled) then
    errmsg='transient: true transient requires enabled '//&
    &'TRANSIENT input block'
    ok=-1
    return
  endif
  call transient_validate_input(dm,errmsg,oki)
  if(oki.ne.0) then
    ok=-1
    return
  endif
  call transient_output_begin(std,iu,errmsg,oki)
  if(oki.ne.0) then
    ok=-1
    return
  endif
  opened=.true.
  call transient_output_header(iu,errmsg,oki)
  if(oki.ne.0) goto 900
  call transient_time_step_audit(iu,errmsg,oki)
  if(oki.ne.0) then
    goto 900
  endif
  call transient_speed_state(trn_tstart,rpm0,om0,al0,ph0,&
  &errmsg,oki)
  if(oki.ne.0) then
    goto 900
  endif
  call transient_speed_state(trn_tend,rpm1,om1,al1,ph1,&
  &errmsg,oki)
  if(oki.ne.0) then
    goto 900
  endif
  call transient_model_text(trn_model,model)
  nstep=nint((trn_tend-trn_tstart)/trn_dt)
  do j=1,4
    counts(j)=0
  enddo
  call transient_run(iu,pp,trn_dt,.true.,metrics(1,1),counts,&
  &errmsg,oki)
  if(oki.ne.0) then
    goto 900
  endif
  if(trn_convergence) then
    call transient_run(iu,pp,trn_dt/2._wp,.false.,metrics(1,2),counts,errmsg,oki)
    if(oki.ne.0) then
      goto 900
    endif
    call transient_run(iu,pp,trn_dt/4._wp,.false.,metrics(1,3),counts,errmsg,oki)
    if(oki.ne.0) then
      goto 900
    endif
    call transient_write_convergence(iu,trn_dt,metrics,errmsg,oki)
    if(oki.ne.0) then
      goto 900
    endif
  endif
  call transient_output_summary(iu,metrics(1,1),counts,errmsg,oki)
  if(oki.ne.0) then
    goto 900
  endif
  call transient_output_end(std,iu,.true.,errmsg)
  ok=0
  return
900 if(opened) call transient_output_end(std,iu,.false.,errmsg)
  ok=-1
  return
end subroutine transient_solver
