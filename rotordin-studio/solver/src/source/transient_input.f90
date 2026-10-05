!
!     Explicit tabular input for the true lateral transient solver.
!
subroutine transient_set_cli(value)
  use com_trncli, only: trn_cli
  implicit none
  logical :: value
  trn_cli=value
  return
end subroutine transient_set_cli
!
logical function transient_cli_requested_f()
  use com_trncli, only: trn_cli
  implicit none
  transient_cli_requested_f=trn_cli
  return
end function transient_cli_requested_f
!
logical function transient_requested_f()
  use com_trncli, only: trn_cli
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
  if(trn_analysis_mode.eq.1) then
    transient_requested_f=.false.
  else if(trn_analysis_mode.eq.2) then
    transient_requested_f=trn_enabled
  else
    transient_requested_f=trn_cli .or. trn_enabled
  endif
  return
end function transient_requested_f
!
!     Lateral response-domain selector written by the GUI in input.txt.
!       0 = LEGACY/AUTO (backwards compatible: command/options decide)
!       1 = STEADY_STATE (frequency-domain response)
!       2 = TRANSIENT (true physical time integration)
!
subroutine transient_set_analysis_mode(name,errmsg,ok)
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
  character(len=*) :: name, errmsg
  character(len=32) :: tmp
  integer :: ok
  tmp=adjustl(name)
  call transient_upper(tmp)
  if(index(tmp,'STEADY_STATE').eq.1 .or.&
  &index(tmp,'STEADY').eq.1 .or.&
  &index(tmp,'FREQUENCY_DOMAIN').eq.1) then
    trn_analysis_mode=1
  else if(index(tmp,'TRANSIENT').eq.1 .or.&
  &index(tmp,'TIME_DOMAIN').eq.1) then
    trn_analysis_mode=2
  else if(index(tmp,'LEGACY').eq.1 .or.&
  &index(tmp,'AUTO').eq.1) then
    trn_analysis_mode=0
  else
    errmsg='transient: invalid LATERAL_RESPONSE_MODE'
    ok=-1
    return
  endif
  ok=0
  return
end subroutine transient_set_analysis_mode
!
integer function transient_analysis_mode_f()
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
  transient_analysis_mode_f=trn_analysis_mode
  return
end function transient_analysis_mode_f
!
subroutine transient_analysis_mode_text(text)
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
  character(len=*) :: text
  if(trn_analysis_mode.eq.1) then
    text='STEADY_STATE'
  else if(trn_analysis_mode.eq.2) then
    text='TRANSIENT'
  else
    text='LEGACY'
  endif
  return
end subroutine transient_analysis_mode_text
!
!     Parse either canonical one-line syntax:
!       LATERAL_RESPONSE_MODE STEADY_STATE
!     or two-line syntax:
!       LATERAL_RESPONSE_MODE
!       STEADY_STATE
!
subroutine transient_parse_analysis_mode(iu,line,errmsg,ok)
  implicit none
  integer :: iu, ok, ios
  character(len=*) :: line, errmsg
  character(len=512) :: rest, next
  character(len=32) :: value
  call transient_rest(line,rest)
  value=' '
  read(rest,*,iostat=ios) value
  if(ios.ne.0 .or. len_trim(value).eq.0) then
    call transient_read_nonblank(iu,next,errmsg,ok)
    if(ok.ne.0) return
    read(next,*,iostat=ios) value
  endif
  if(ios.ne.0 .or. len_trim(value).eq.0) then
    errmsg='transient: missing LATERAL_RESPONSE_MODE value'
    ok=-1
    return
  endif
  call transient_set_analysis_mode(value,errmsg,ok)
  return
end subroutine transient_parse_analysis_mode
!
!     Apply an explicit input choice after all legacy OPTIONS/CLI parsin
!     The input mode is authoritative when present.  Campbell, modes and
!     other analyses are not changed; only lateral response-domain optio
!     are made mutually exclusive.
!
subroutine transient_apply_analysis_mode(opt,mop,errmsg,ok)
  use com_trncli, only: trn_cli
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
  integer :: mop, ok
  logical :: opt(mop)
  character(len=*) :: errmsg
  ok=0
  if(trn_analysis_mode.eq.0) return
  if(trn_analysis_mode.eq.1) then
!       opt(3) = steady-state unbalance/frequency response.
!       opt(4) = historical harmonic-orbit reconstruction, not true tran
    if(mop.ge.3) opt(3)=.true.
    if(mop.ge.4) opt(4)=.false.
    return
  endif
!     TRUE TRANSIENT selected by the input: its control block is mandato
  if(.not.trn_enabled) then
    errmsg='transient: LATERAL_RESPONSE_MODE TRANSIENT requires '//&
    &'an enabled TRANSIENT block'
    ok=-1
    return
  endif
  if(mop.ge.3) opt(3)=.false.
  if(mop.ge.4) opt(4)=.false.
  return
end subroutine transient_apply_analysis_mode
!
subroutine transient_reset()
  use com_trnctl, only: trn_enabled, trn_gravity, trn_unbalance, trn_convergence, trn_model, trn_speed_kind, &
    & trn_init_kind, trn_stride, trn_nprof, trn_nic, trn_nforce, trn_out_policy, trn_monitor_dof, trn_analysis_mode
  use com_trnforce, only: trn_fdof, trn_fnpt, trn_ftime, trn_fvalue
  use com_trnic, only: trn_icdof, trn_icq, trn_icv
  use com_trnprof, only: trn_ptime, trn_prpm, trn_pphase
  use com_trnreadbuf, only: trn_pending, trn_pending_line
  use com_trntime, only: trn_tstart, trn_tend, trn_dt, trn_beta, trn_gamma, trn_phase0, trn_audit_hz
  use rd_kinds, only: wp
  implicit none
  integer :: trn_mprof, trn_mic, trn_mforce, trn_mfpt
  parameter(trn_mprof=999,trn_mic=500,trn_mforce=20,&
  &trn_mfpt=999)
  integer :: i, j
  trn_pending=.false.
  trn_pending_line=' '
  trn_enabled=.false.
  trn_analysis_mode=0
  trn_gravity=.false.
  trn_unbalance=.false.
  trn_convergence=.false.
  trn_model=0
  trn_speed_kind=0
  trn_init_kind=1
  trn_stride=1
  trn_nprof=0
  trn_nic=0
  trn_nforce=0
  trn_out_policy=1
  trn_monitor_dof=1
  trn_tstart=0._wp
  trn_tend=0._wp
  trn_dt=0._wp
  trn_beta=.25_wp
  trn_gamma=.5_wp
  trn_phase0=0._wp
  trn_audit_hz=0._wp
  do i=1,trn_mprof
    trn_ptime(i)=0._wp
    trn_prpm(i)=0._wp
    trn_pphase(i)=0._wp
  enddo
  do i=1,trn_mic
    trn_icdof(i)=0
    trn_icq(i)=0._wp
    trn_icv(i)=0._wp
  enddo
  do i=1,trn_mforce
    trn_fdof(i)=0
    trn_fnpt(i)=0
    do j=1,trn_mfpt
      trn_ftime(i,j)=0._wp
      trn_fvalue(i,j)=0._wp
    enddo
  enddo
  return
end subroutine transient_reset
!
!     One-line look-ahead shared with the optional FOUNDATION reader.
!     This avoids BACKSPACE on stdin, which is not seekable when piped.
!
subroutine transient_stash_line(line)
  use com_trnreadbuf, only: trn_pending, trn_pending_line
  implicit none
  character(len=*) :: line
  trn_pending_line=line
  trn_pending=.true.
  return
end subroutine transient_stash_line
!
logical function transient_pending_f()
  use com_trnreadbuf, only: trn_pending, trn_pending_line
  implicit none
  transient_pending_f=trn_pending
  return
end function transient_pending_f
!
subroutine transient_read_raw(iu,line,ios)
  use com_trnreadbuf, only: trn_pending, trn_pending_line
  implicit none
  integer :: iu, ios
  character(len=*) :: line
  if(trn_pending) then
    line=trn_pending_line
    trn_pending=.false.
    trn_pending_line=' '
    ios=0
  else
    read(iu,'(A)',iostat=ios) line
  endif
  return
end subroutine transient_read_raw
!
subroutine transient_upper(s)
  implicit none
  character(len=*) :: s
  integer :: i, k
  do i=1,len(s)
    k=ichar(s(i:i))
    if(k.ge.97 .and. k.le.122) s(i:i)=char(k-32)
  enddo
  return
end subroutine transient_upper
!
subroutine transient_rest(line,val)
  implicit none
  character(len=*) :: line, val
  integer :: i, n
  val=' '
  n=len_trim(line)
  i=1
  do while(i.le.n .and. line(i:i).ne.' ' .and.&
  &line(i:i).ne.char(9))
    i=i+1
  enddo
  do while(i.le.n .and. (line(i:i).eq.' ' .or.&
  &line(i:i).eq.char(9)))
    i=i+1
  enddo
  if(i.le.n) val=line(i:n)
  return
end subroutine transient_rest
!
subroutine transient_read_nonblank(iu,line,errmsg,ok)
  implicit none
  integer :: iu, ios, ok
  character(len=*) :: line, errmsg
10 continue
  read(iu,'(A)',iostat=ios) line
  if(ios.ne.0) then
    errmsg='transient: unexpected EOF in input block'
    ok=-1
    return
  endif
  line=adjustl(line)
  if(len_trim(line).eq.0 .or. line(1:1).eq.'#') goto 10
  ok=0
  return
end subroutine transient_read_nonblank
!
subroutine transient_set_model(name,errmsg,ok)
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
  character(len=*) :: name, errmsg
  character(len=32) :: tmp
  integer :: ok
  tmp=adjustl(name)
  call transient_upper(tmp)
  if(index(tmp,'CONSTANT').eq.1) then
    trn_model=1
  else if(index(tmp,'STARTUP').eq.1) then
    trn_model=2
  else if(index(tmp,'COASTDOWN').eq.1) then
    trn_model=3
  else if(index(tmp,'USER_PROFILE').eq.1 .or.&
  &trim(tmp).eq.'USER') then
    trn_model=4
  else
    errmsg='transient: invalid MODEL'
    ok=-1
    return
  endif
  ok=0
  return
end subroutine transient_set_model
!
subroutine transient_set_init(name,errmsg,ok)
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
  character(len=*) :: name, errmsg
  character(len=32) :: tmp
  integer :: ok
  tmp=adjustl(name)
  call transient_upper(tmp)
  if(index(tmp,'ZERO').eq.1) then
    trn_init_kind=1
  else if(index(tmp,'USER').eq.1) then
    trn_init_kind=2
  else if(index(tmp,'STATIC_EQUILIBRIUM').eq.1) then
    trn_init_kind=3
  else
    errmsg='transient: invalid INITCOND'
    ok=-1
    return
  endif
  ok=0
  return
end subroutine transient_set_init
!
subroutine transient_read_control(iu,errmsg,ok)
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
  integer :: iu, ok, ios, i, j, n, ien, dof
  character(len=*) :: errmsg
  character(len=512) :: line, uline, rest
  character(len=32) :: key, value, model, integrator
  real(wp) :: v1, v2, v3, v4
  logical :: found, mhandled
  ok=0
  found=.false.
10 continue
  call transient_read_raw(iu,line,ios)
  if(ios.ne.0) return
  uline=adjustl(line)
  call transient_upper(uline)
  if(len_trim(uline).eq.0) goto 10
  call model_config_line(line,errmsg,ok,mhandled)
  if(ok.ne.0) return
  if(mhandled) goto 10
  if(index(uline,'LATERAL_RESPONSE_MODE').eq.1) then
    call transient_parse_analysis_mode(iu,line,errmsg,ok)
    if(ok.ne.0) return
    goto 10
  endif
  if(index(uline,'FOUNDATION').eq.1) then
    return
  endif
  if(index(uline,'TRANSIENT').eq.1) then
    found=.true.
    goto 20
  endif
  if(index(uline,'END').eq.1) return
  goto 10
20 continue
  call transient_read_nonblank(iu,line,errmsg,ok)
  if(ok.ne.0) return
  read(line,*,iostat=ios) ien,model,integrator
  if(ios.ne.0) then
    errmsg='transient: invalid control row'
    ok=-1
    return
  endif
  call transient_upper(integrator)
  if(index(integrator,'NEWMARK').ne.1) then
    errmsg='transient: only NEWMARK integrator is supported'
    ok=-1
    return
  endif
  trn_enabled=ien.ne.0
  call transient_set_model(model,errmsg,ok)
  if(ok.ne.0) return
30 continue
  call transient_read_nonblank(iu,line,errmsg,ok)
  if(ok.ne.0) return
  uline=adjustl(line)
  call transient_upper(uline)
  if(index(uline,'END_TRANSIENT').eq.1) goto 90
  read(uline,*,iostat=ios) key
  if(ios.ne.0) goto 30
  call transient_upper(key)
  call transient_rest(uline,rest)
  if(trim(key).eq.'TSTART') then
    call transient_read_nonblank(iu,line,errmsg,ok)
    if(ok.ne.0) return
    read(line,*,iostat=ios) trn_tstart,trn_tend,trn_dt
    if(ios.ne.0) then
      errmsg='transient: invalid TSTART/TEND/DT row'
      ok=-1
      return
    endif
  else if(trim(key).eq.'OUTPUT_STRIDE') then
    read(rest,*,iostat=ios) trn_stride
    if(ios.ne.0) goto 80
  else if(trim(key).eq.'MONITOR_DOF') then
    read(rest,*,iostat=ios) trn_monitor_dof
    if(ios.ne.0) goto 80
  else if(trim(key).eq.'INITCOND') then
    read(rest,*,iostat=ios) value
    if(ios.ne.0 .or. len_trim(value).eq.0) then
      call transient_read_nonblank(iu,line,errmsg,ok)
      if(ok.ne.0) return
      read(line,*,iostat=ios) value
    endif
    if(ios.ne.0) goto 80
    call transient_set_init(value,errmsg,ok)
    if(ok.ne.0) return
  else if(trim(key).eq.'INITIAL_DOF') then
    read(rest,*,iostat=ios) n
    if(ios.ne.0 .or. n.lt.1 .or. n.gt.trn_mic) goto 80
    trn_nic=n
    call transient_read_nonblank(iu,line,errmsg,ok)
    if(ok.ne.0) return
    do i=1,n
      call transient_read_nonblank(iu,line,errmsg,ok)
      if(ok.ne.0) return
      read(line,*,iostat=ios) trn_icdof(i),trn_icq(i),trn_icv(i)
      if(ios.ne.0) goto 80
    enddo
  else if(trim(key).eq.'SPEED_PROFILE') then
    read(rest,*,iostat=ios) value
    if(ios.ne.0) goto 80
    call transient_upper(value)
    if(index(value,'CONSTANT').eq.1) then
      trn_speed_kind=1
      trn_nprof=1
    else if(index(value,'LINEAR').eq.1) then
      trn_speed_kind=2
      trn_nprof=2
      trn_ptime(1)=trn_tstart
      trn_ptime(2)=trn_tend
    else if(index(value,'TABLE').eq.1) then
      trn_speed_kind=3
    else
      errmsg='transient: invalid SPEED_PROFILE type'
      ok=-1
      return
    endif
  else if(trim(key).eq.'RPM') then
    read(rest,*,iostat=ios) trn_prpm(1)
    if(ios.ne.0) goto 80
  else if(trim(key).eq.'START_RPM') then
    read(rest,*,iostat=ios) trn_prpm(1)
    if(ios.ne.0) goto 80
  else if(trim(key).eq.'END_RPM') then
    read(rest,*,iostat=ios) trn_prpm(2)
    if(ios.ne.0) goto 80
  else if(trim(key).eq.'START_TIME') then
    read(rest,*,iostat=ios) trn_ptime(1)
    if(ios.ne.0) goto 80
  else if(trim(key).eq.'END_TIME') then
    read(rest,*,iostat=ios) trn_ptime(2)
    if(ios.ne.0) goto 80
  else if(trim(key).eq.'NPOINTS') then
    if(trn_speed_kind.ne.3) then
      errmsg='transient: NPOINTS requires SPEED_PROFILE TABLE'
      ok=-1
      return
    endif
    read(rest,*,iostat=ios) n
    if(ios.ne.0 .or. n.lt.2 .or. n.gt.trn_mprof) goto 80
    trn_nprof=n
    call transient_read_nonblank(iu,line,errmsg,ok)
    if(ok.ne.0) return
    do i=1,n
      call transient_read_nonblank(iu,line,errmsg,ok)
      if(ok.ne.0) return
      read(line,*,iostat=ios) trn_ptime(i),trn_prpm(i)
      if(ios.ne.0) goto 80
    enddo
  else if(trim(key).eq.'INITIAL_PHASE') then
    read(rest,*,iostat=ios) trn_phase0
    if(ios.ne.0) goto 80
  else if(trim(key).eq.'FORCES') then
    trn_gravity=index(rest,'GRAVITY').gt.0
    trn_unbalance=index(rest,'UNBALANCE').gt.0
  else if(trim(key).eq.'GRAVITY') then
    call transient_upper(rest)
    trn_gravity=rest(1:1).eq.'Y' .or. rest(1:1).eq.'1' .or.&
    &rest(1:1).eq.'T'
  else if(trim(key).eq.'UNBALANCE') then
    call transient_upper(rest)
    trn_unbalance=rest(1:1).eq.'Y' .or. rest(1:1).eq.'1' .or.&
    &rest(1:1).eq.'T'
  else if(trim(key).eq.'GENERAL_FORCE') then
    read(rest,*,iostat=ios) dof,n
    if(ios.ne.0 .or. n.lt.2 .or. n.gt.trn_mfpt .or.&
    &trn_nforce.ge.trn_mforce) goto 80
    j=trn_nforce+1
    trn_nforce=j
    trn_fdof(j)=dof
    trn_fnpt(j)=n
    call transient_read_nonblank(iu,line,errmsg,ok)
    if(ok.ne.0) return
    do i=1,n
      call transient_read_nonblank(iu,line,errmsg,ok)
      if(ok.ne.0) return
      read(line,*,iostat=ios) trn_ftime(j,i),trn_fvalue(j,i)
      if(ios.ne.0) goto 80
    enddo
  else if(trim(key).eq.'OUT_OF_RANGE') then
    call transient_upper(rest)
    if(index(rest,'ERROR').eq.1) then
      trn_out_policy=1
    else
      errmsg='transient: only OUT_OF_RANGE ERROR is supported in V1'
      ok=-1
      return
    endif
  else if(trim(key).eq.'CONVERGENCE') then
    call transient_upper(rest)
    trn_convergence=rest(1:1).eq.'Y' .or. rest(1:1).eq.'1' .or.&
    &rest(1:1).eq.'T'
  else if(trim(key).eq.'AUDIT_FREQUENCY_HZ') then
    read(rest,*,iostat=ios) trn_audit_hz
    if(ios.ne.0) goto 80
  else if(trim(key).eq.'BETA') then
    read(rest,*,iostat=ios) trn_beta
    if(ios.ne.0) goto 80
  else if(trim(key).eq.'GAMMA') then
    read(rest,*,iostat=ios) trn_gamma
    if(ios.ne.0) goto 80
  else if(trim(key).eq.'END_SPEED_PROFILE' .or.&
  &trim(key).eq.'END_GENERAL_FORCE' .or.&
  &trim(key).eq.'END_INITCOND') then
    continue
  else
    errmsg='transient: unknown keyword '//trim(key)
    ok=-1
    return
  endif
  goto 30
80 continue
  errmsg='transient: invalid value for keyword '//trim(key)
  ok=-1
  return
90 continue
  call transient_prepare_profile(errmsg,ok)
  return
end subroutine transient_read_control
!
subroutine transient_prepare_profile(errmsg,ok)
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
  character(len=*) :: errmsg
  integer :: ok, i, j
  real(wp) :: pi, fac, dt, om0, om1
  parameter(pi=3.1415926535897932384626433832795_wp)
  ok=-1
  if(.not.trn_enabled) then
    ok=0
    return
  endif
  if(trn_model.lt.1 .or. trn_speed_kind.lt.1) then
    errmsg='transient: MODEL and SPEED_PROFILE are required'
    return
  endif
  if(trn_dt.le.0._wp) then
    errmsg='transient: DT must be greater than zero'
    return
  endif
  if(trn_tend.le.trn_tstart) then
    errmsg='transient: TEND must be greater than TSTART'
    return
  endif
  if(trn_stride.lt.1) then
    errmsg='transient: OUTPUT_STRIDE must be positive'
    return
  endif
  if(abs(trn_beta-.25_wp).gt.1e-12_wp .or.abs(trn_gamma-.5_wp).gt.1e-12_wp) then
    errmsg='transient: V1 requires beta=0.25 and gamma=0.5'
    return
  endif
  if(trn_speed_kind.eq.1) then
    trn_nprof=1
    trn_ptime(1)=trn_tstart
    trn_pphase(1)=trn_phase0
  else
    if(trn_nprof.lt.2) then
      errmsg='transient: speed profile requires at least two points'
      return
    endif
    if(abs(trn_ptime(1)-trn_tstart).gt.1e-10_wp .or.abs(trn_ptime(trn_nprof)-trn_tend).gt.1e-10_wp) then
      errmsg='transient: speed profile must span TSTART to TEND'
      return
    endif
    do i=2,trn_nprof
      if(trn_ptime(i).le.trn_ptime(i-1)) then
        errmsg='transient: speed-profile TIME must be increasing'
        return
      endif
    enddo
    trn_pphase(1)=trn_phase0
    fac=2._wp*pi/60._wp
    do i=2,trn_nprof
      dt=trn_ptime(i)-trn_ptime(i-1)
      om0=fac*trn_prpm(i-1)
      om1=fac*trn_prpm(i)
      trn_pphase(i)=trn_pphase(i-1)+.5_wp*(om0+om1)*dt
    enddo
  endif
  if(trn_model.eq.1 .and. trn_speed_kind.ne.1) then
    errmsg='transient: CONSTANT model requires CONSTANT profile'
    return
  endif
  if(trn_model.eq.2 .and.&
  &trn_prpm(trn_nprof).le.trn_prpm(1)) then
    errmsg='transient: STARTUP requires increasing endpoint RPM'
    return
  endif
  if(trn_model.eq.3 .and.&
  &trn_prpm(trn_nprof).ge.trn_prpm(1)) then
    errmsg='transient: COASTDOWN requires decreasing endpoint RPM'
    return
  endif
  if(trn_init_kind.eq.2 .and. trn_nic.lt.1) then
    errmsg='transient: USER initial condition needs INITIAL_DOF'
    return
  endif
  do i=1,trn_nforce
    if(trn_fnpt(i).lt.2) then
      errmsg='transient: GENERAL_FORCE needs at least two points'
      return
    endif
    do j=2,trn_fnpt(i)
      if(trn_ftime(i,j).le.trn_ftime(i,j-1)) then
        errmsg='transient: force TIME must be increasing'
        return
      endif
    enddo
    if(trn_ftime(i,1).gt.trn_tstart .or.&
    &trn_ftime(i,trn_fnpt(i)).lt.trn_tend) then
      errmsg='transient: force table must span TSTART to TEND'
      return
    endif
  enddo
  ok=0
  return
end subroutine transient_prepare_profile
!
subroutine transient_validate_input(dm,errmsg,ok)
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
  integer :: dm, ok, i, nstep
  real(wp) :: rn
  character(len=*) :: errmsg
  call transient_prepare_profile(errmsg,ok)
  if(ok.ne.0 .or. .not.trn_enabled) return
  if(dm.gt.500 .or. dm.lt.1) then
    errmsg='transient: global matrix exceeds mtg=500'
    ok=-1
    return
  endif
  rn=(trn_tend-trn_tstart)/trn_dt
  nstep=nint(rn)
  if(nstep.lt.1 .or. abs(rn-real(nstep, wp)).gt.1e-8_wp) then
    errmsg='transient: (TEND-TSTART)/DT must be an integer'
    ok=-1
    return
  endif
  if(trn_monitor_dof.lt.1 .or. trn_monitor_dof.gt.dm) then
    write(errmsg,'(A,I0,A,I0,A)') 'transient: MONITOR_DOF ',&
    &trn_monitor_dof,' outside global matrix [1,',dm,']'
    ok=-1
    return
  endif
  do i=1,trn_nic
    if(trn_icdof(i).lt.1 .or. trn_icdof(i).gt.dm) then
      errmsg='transient: INITIAL_DOF outside global matrix'
      ok=-1
      return
    endif
  enddo
  do i=1,trn_nforce
    if(trn_fdof(i).lt.1 .or. trn_fdof(i).gt.dm) then
      errmsg='transient: GENERAL_FORCE DOF outside global matrix'
      ok=-1
      return
    endif
  enddo
  ok=0
  return
end subroutine transient_validate_input
