!
!     External-force assembly. HydroBear physics is deliberately absent.
!
real(wp) function transient_force_value(channel,t)
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
  integer :: channel, n, lo, hi, mid
  real(wp) :: t, x0, x1, y0, y1
  n=trn_fnpt(channel)
  if(t.le.trn_ftime(channel,1)) then
    transient_force_value=trn_fvalue(channel,1)
    return
  endif
  if(t.ge.trn_ftime(channel,n)) then
    transient_force_value=trn_fvalue(channel,n)
    return
  endif
  lo=1
  hi=n
10 if(hi-lo.gt.1) then
    mid=(lo+hi)/2
    if(t.ge.trn_ftime(channel,mid)) then
      lo=mid
    else
      hi=mid
    endif
    goto 10
  endif
  x0=trn_ftime(channel,lo)
  x1=trn_ftime(channel,lo+1)
  y0=trn_fvalue(channel,lo)
  y1=trn_fvalue(channel,lo+1)
  transient_force_value=y0+(y1-y0)*(t-x0)/(x1-x0)
  return
end function transient_force_value
!
subroutine transient_gravity_force(dm,pp,mass,force)
  use com_hang, only: hangle, accg, isf, ha
  use com_sec, only: n, y, nt, nn
  use rd_kinds, only: lrk, wp
  implicit none
  integer :: mtg, mts
  parameter(mtg=500,mts=999)
  integer :: dm, pp, i, j, nshaft, shaft_ndof_f
  real(wp) :: mass(mtg,mtg), force(mtg), ag(mtg)
  real(wp) :: gg, angle, az, pi
  character(len=1) :: ca
  parameter(pi=3.1415926535897932384626433832795_wp)
  gg=real(accg, wp)
  if(gg.le.0._wp) gg=9.8066_wp
  angle=real(hangle, wp)
  ca=ha(1:1)
  if(ca.ne.'r' .and. ca.ne.'R') angle=angle*pi/180._wp
  az=-gg*cos(angle)
  do i=1,dm
    ag(i)=0._wp
  enddo
  nshaft=shaft_ndof_f()
  do i=2,nshaft,4
    ag(i)=az
  enddo
  do i=1,dm
    do j=1,dm
      force(i)=force(i)+mass(i,j)*ag(j)
    enddo
  enddo
  return
end subroutine transient_gravity_force
!
subroutine transient_unbalance_force(dm,omega,alpha,phase,force,&
&errmsg,ok)
  use com_mfa, only: ma, rm, au
  use com_sec, only: n, y, nt, nn
  use com_unb, only: nini_r, nfin_r, dw_r
  use com_unb0, only: pr, desp, ori, np, nm
  use com_unb1, only: ndd, mu, ed, tpf, nb
  use rd_kinds, only: lrk, wp
  implicit none
  integer :: mtg, mxb, mxp, mts
  parameter(mtg=500,mxb=99,mxp=9,mts=999)
  integer :: dm, ok, i, k, l, oki
  integer :: pdd(mxb), inddis, ifidxof
  real(wp) :: omega, alpha, phase, force(mtg), theta, delta, pi, me
  character(len=*) :: errmsg
  parameter(pi=3.1415926535897932384626433832795_wp)
  ok=-1
  call inddbl(y,ndd,pdd,nb,nt,mxb,mxb,mts,errmsg,oki)
  if(oki.lt.0) return
  do i=1,nb
    if(tpf(i).eq.0) then
      delta=real(ed(i), wp)
      if(au(1:1).ne.'r' .and. au(1:1).ne.'R')delta=delta*pi/180._wp
      theta=phase+delta
      me=real(mu(i), wp)
      l=ifidxof(pdd(i),1)
      if(l.lt.1 .or. l+1.gt.dm) then
        errmsg='transient: invalid unbalance DOF mapping'
        return
      endif
      force(l)=force(l)+me*(-omega*omega*sin(theta)+&
      &alpha*cos(theta))
      force(l+1)=force(l+1)+me*(omega*omega*cos(theta)+&
      &alpha*sin(theta))
    endif
  enddo
  ok=0
  return
end subroutine transient_unbalance_force
!
subroutine transient_nonlinear_force(dm,q,v,omega,t,force)
  use rd_kinds, only: wp
  implicit none
  integer :: dm, i
  real(wp) :: q(dm), v(dm), omega, t, force(dm)
!     V1 hook. A future nonlinear provider adds into FORCE here.
  do i=1,dm
    force(i)=force(i)+0._wp*q(i)+0._wp*v(i)+0._wp*omega+0._wp*t
  enddo
  return
end subroutine transient_nonlinear_force
!
subroutine transient_force(dm,pp,mass,t,omega,alpha,phase,q,v,&
&static_only,force,errmsg,ok)
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
  integer :: mtg
  parameter(mtg=500)
  integer :: dm, pp, ok, i, oki
  real(wp) :: mass(mtg,mtg), t, omega, alpha, phase
  real(wp) :: q(mtg), v(mtg), force(mtg)
  real(wp) :: transient_force_value
  logical :: static_only
  character(len=*) :: errmsg
  do i=1,dm
    force(i)=0._wp
  enddo
  if(trn_gravity) call transient_gravity_force(dm,pp,mass,force)
  if(.not.static_only .and. trn_unbalance) then
    call transient_unbalance_force(dm,omega,alpha,phase,force,&
    &errmsg,oki)
    if(oki.ne.0) then
      ok=-1
      return
    endif
  endif
  do i=1,trn_nforce
    force(trn_fdof(i))=force(trn_fdof(i))+&
    &transient_force_value(i,t)
  enddo
  if(.not.static_only)&
  &call transient_nonlinear_force(dm,q,v,omega,t,force)
  ok=0
  return
end subroutine transient_force

