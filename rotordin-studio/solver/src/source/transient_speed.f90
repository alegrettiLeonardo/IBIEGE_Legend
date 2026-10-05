!
!     Deterministic speed, acceleration and integrated phase profiles.
!
subroutine transient_speed_state(t,rpm,omega,alpha,phase,&
&errmsg,ok)
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
  real(wp) :: t, rpm, omega, alpha, phase, dt, slope, fac
  integer :: ok, lo, hi, mid, i
  character(len=*) :: errmsg
  real(wp) :: pi
  parameter(pi=3.1415926535897932384626433832795_wp)
  ok=-1
  fac=2._wp*pi/60._wp
  if(t.lt.trn_tstart-1e-10_wp .or. t.gt.trn_tend+1e-10_wp) then
    errmsg='transient: requested time is outside speed profile'
    return
  endif
  if(trn_speed_kind.eq.1) then
    rpm=trn_prpm(1)
    omega=fac*rpm
    alpha=0._wp
    phase=trn_phase0+omega*(t-trn_tstart)
    ok=0
    return
  endif
  if(t.ge.trn_ptime(trn_nprof)) then
    i=trn_nprof-1
  else
    lo=1
    hi=trn_nprof
10  if(hi-lo.gt.1) then
      mid=(lo+hi)/2
      if(t.ge.trn_ptime(mid)) then
        lo=mid
      else
        hi=mid
      endif
      goto 10
    endif
    i=lo
  endif
  dt=trn_ptime(i+1)-trn_ptime(i)
  if(dt.le.0._wp) then
    errmsg='transient: invalid zero speed-profile interval'
    return
  endif
  slope=(trn_prpm(i+1)-trn_prpm(i))/dt
  rpm=trn_prpm(i)+slope*(t-trn_ptime(i))
  omega=fac*rpm
  alpha=fac*slope
  phase=trn_pphase(i)+fac*trn_prpm(i)*(t-trn_ptime(i))+.5_wp*alpha*(t-trn_ptime(i))**2
  ok=0
  return
end subroutine transient_speed_state

