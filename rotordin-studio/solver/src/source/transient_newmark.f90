!
!     Newmark-beta average-acceleration integration kernel.
!     This file has no dependency on RotorDin common blocks so that the
!     numerical method can be certified independently.
!
subroutine transient_newmark_coefficients(dt,beta,gamma,&
&a0,a1,a2,a3,a4,a5,a6,a7,errmsg,ok)
  use rd_kinds, only: wp
  implicit none
  real(wp) :: dt, beta, gamma, a0, a1, a2, a3, a4, a5, a6, a7
  character(len=*) :: errmsg
  integer :: ok
  ok=-1
  if(dt.le.0._wp) then
    errmsg='transient: Newmark dt must be greater than zero'
    return
  endif
  if(beta.le.0._wp .or. gamma.le.0._wp) then
    errmsg='transient: invalid Newmark beta/gamma'
    return
  endif
  a0=1._wp/(beta*dt*dt)
  a1=gamma/(beta*dt)
  a2=1._wp/(beta*dt)
  a3=1._wp/(2._wp*beta)-1._wp
  a4=gamma/beta-1._wp
  a5=dt*(gamma/(2._wp*beta)-1._wp)
  a6=dt*(1._wp-gamma)
  a7=gamma*dt
  ok=0
  return
end subroutine transient_newmark_coefficients
!
subroutine transient_newmark_factor(n,ld,m,c,k,dt,beta,gamma,&
&kn,ipiv,errmsg,ok)
  use rd_kinds, only: wp
  implicit none
  integer :: n, ld, ipiv(ld), ok, i, j, info
  real(wp) :: m(ld,ld), c(ld,ld), k(ld,ld), kn(ld,ld)
  real(wp) :: dt, beta, gamma, a0, a1, a2, a3, a4, a5, a6, a7
  character(len=*) :: errmsg
  call transient_newmark_coefficients(dt,beta,gamma,&
  &a0,a1,a2,a3,a4,a5,a6,a7,errmsg,ok)
  if(ok.ne.0) return
  if(n.lt.1 .or. n.gt.ld) then
    errmsg='transient: Newmark matrix dimension exceeds limit'
    ok=-1
    return
  endif
  do j=1,n
    do i=1,n
      kn(i,j)=k(i,j)+a0*m(i,j)+a1*c(i,j)
    enddo
  enddo
  call dgetrf(n,n,kn,ld,ipiv,info)
  if(info.ne.0) then
    write(errmsg,'(A,I8)')&
    &'transient: Newmark DGETRF failed, INFO=',info
    ok=-1
    return
  endif
  ok=0
  return
end subroutine transient_newmark_factor
!
subroutine transient_newmark_step(n,ld,m,c,dt,beta,gamma,&
&kn,ipiv,force,q,v,a,qn,vn,an,rhs,errmsg,ok)
  use rd_kinds, only: wp
  implicit none
  integer :: n, ld, ipiv(ld), ok, i, j, info
  real(wp) :: m(ld,ld), c(ld,ld), kn(ld,ld)
  real(wp) :: force(ld), q(ld), v(ld), a(ld)
  real(wp) :: qn(ld), vn(ld), an(ld), rhs(ld)
  real(wp) :: dt, beta, gamma, a0, a1, a2, a3, a4, a5, a6, a7
  real(wp) :: one
  character(len=*) :: errmsg
  parameter(one=1._wp)
  call transient_newmark_coefficients(dt,beta,gamma,&
  &a0,a1,a2,a3,a4,a5,a6,a7,errmsg,ok)
  if(ok.ne.0) return
!     Predictors depend only on column j.  The historical loop
!     recomputed them once for every matrix row.  Build each predictor
!     once and use BLAS matrix-vector products for the same Newmark RHS.
!     qn and vn are scratch here and are overwritten below by q(n+1)
!     and v(n+1), respectively.
  do j=1,n
    qn(j)=a0*q(j)+a2*v(j)+a3*a(j)
    vn(j)=a1*q(j)+a4*v(j)+a5*a(j)
    rhs(j)=force(j)
  enddo
  call dgemv('N',n,n,one,m,ld,qn,1,one,rhs,1)
  call dgemv('N',n,n,one,c,ld,vn,1,one,rhs,1)
  do i=1,n
    qn(i)=rhs(i)
  enddo
  call dgetrs('N',n,1,kn,ld,ipiv,qn,ld,info)
  if(info.ne.0) then
    write(errmsg,'(A,I8)')&
    &'transient: Newmark DGETRS failed, INFO=',info
    ok=-1
    return
  endif
  do i=1,n
    an(i)=a0*(qn(i)-q(i))-a2*v(i)-a3*a(i)
    vn(i)=v(i)+a6*a(i)+a7*an(i)
  enddo
  ok=0
  return
end subroutine transient_newmark_step
!
subroutine transient_initial_acceleration(n,ld,m,c,k,force,q,v,&
&a,work,rhs,ipiv,errmsg,ok)
  use rd_kinds, only: wp
  implicit none
  integer :: n, ld, ipiv(ld), ok, i, j, info
  real(wp) :: m(ld,ld), c(ld,ld), k(ld,ld), work(ld,ld)
  real(wp) :: force(ld), q(ld), v(ld), a(ld), rhs(ld)
  character(len=*) :: errmsg
  if(n.lt.1 .or. n.gt.ld) then
    errmsg='transient: initial acceleration dimension exceeds limit'
    ok=-1
    return
  endif
  do j=1,n
    do i=1,n
      work(i,j)=m(i,j)
    enddo
  enddo
  do i=1,n
    rhs(i)=force(i)
  enddo
!     rhs = force - C*v - K*q.  BLAS preserves the governing equation
!     while removing scalar matrix-vector loops from the transient path.
  call dgemv('N',n,n,-1._wp,c,ld,v,1,1._wp,rhs,1)
  call dgemv('N',n,n,-1._wp,k,ld,q,1,1._wp,rhs,1)
  do i=1,n
    a(i)=rhs(i)
  enddo
  call dgetrf(n,n,work,ld,ipiv,info)
  if(info.ne.0) then
    write(errmsg,'(A,I8)')&
    &'transient: mass DGETRF failed, INFO=',info
    ok=-1
    return
  endif
  call dgetrs('N',n,1,work,ld,ipiv,a,ld,info)
  if(info.ne.0) then
    write(errmsg,'(A,I8)')&
    &'transient: mass DGETRS failed, INFO=',info
    ok=-1
    return
  endif
  ok=0
  return
end subroutine transient_initial_acceleration
!
subroutine transient_solve_system(n,ld,matrix,rhs,solution,work,&
&ipiv,errmsg,ok)
  use rd_kinds, only: wp
  implicit none
  integer :: n, ld, ipiv(ld), ok, i, j, info
  real(wp) :: matrix(ld,ld), rhs(ld), solution(ld), work(ld,ld)
  character(len=*) :: errmsg
  if(n.lt.1 .or. n.gt.ld) then
    errmsg='transient: linear-system dimension exceeds limit'
    ok=-1
    return
  endif
  do j=1,n
    do i=1,n
      work(i,j)=matrix(i,j)
    enddo
  enddo
  do i=1,n
    solution(i)=rhs(i)
  enddo
  call dgetrf(n,n,work,ld,ipiv,info)
  if(info.ne.0) then
    write(errmsg,'(A,I8)')&
    &'transient: linear-system DGETRF failed, INFO=',info
    ok=-1
    return
  endif
  call dgetrs('N',n,1,work,ld,ipiv,solution,ld,info)
  if(info.ne.0) then
    write(errmsg,'(A,I8)')&
    &'transient: linear-system DGETRS failed, INFO=',info
    ok=-1
    return
  endif
  ok=0
  return
end subroutine transient_solve_system
!
logical function transient_finite_vector(n,x)
  use rd_kinds, only: wp
  implicit none
  integer :: n, i
  real(wp) :: x(n), lim
  lim=huge(1._wp)
  transient_finite_vector=.true.
  do i=1,n
    if(x(i).ne.x(i) .or. abs(x(i)).ge.lim) then
      transient_finite_vector=.false.
      return
    endif
  enddo
  return
end function transient_finite_vector
