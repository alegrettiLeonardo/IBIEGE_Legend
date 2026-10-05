! F08: diagnostics for the ACTUAL augmented frequency-domain operator.
! No changes to forces, matrices, LU factors, or the resulting physical response.
module rd_response_quality
  use rd_kinds, only: wp
  use rd_modal_metrics, only: finite_complex
  use, intrinsic :: ieee_arithmetic, only: ieee_is_finite
  implicit none
  private
  type, public :: response_quality_t
    complex(wp), allocatable :: original(:,:)
    real(wp) :: anorm=0._wp,rcond=0._wp
  end type
  public :: response_capture, response_condition, response_report
  interface
    real(wp) function zlange(norm,m,n,a,lda,work)
      import wp
      character(len=1), intent(in) :: norm
      integer, intent(in) :: m,n,lda
      complex(wp), intent(in) :: a(lda,*)
      real(wp), intent(out) :: work(*)
    end function
    subroutine zgecon(norm,n,a,lda,anorm,rcond,work,rwork,info)
      import wp
      character(len=1), intent(in) :: norm
      integer, intent(in) :: n,lda
      complex(wp), intent(in) :: a(lda,*)
      real(wp), intent(in) :: anorm
      real(wp), intent(out) :: rcond,rwork(*)
      complex(wp), intent(out) :: work(*)
      integer, intent(out) :: info
    end subroutine
  end interface
contains
  subroutine response_capture(h,n,state,errmsg,ok)
    complex(wp), intent(in) :: h(:,:)
    integer, intent(in) :: n
    type(response_quality_t), intent(inout) :: state
    character(len=*), intent(out) :: errmsg
    integer, intent(out) :: ok
    real(wp), allocatable :: work(:)
    ok=-1; errmsg=''
    if(n<2.or.mod(n,2)/=0.or.size(h,1)<n.or.size(h,2)<n) then
      errmsg='response audit: invalid augmented dimensions'; return
    end if
    if(.not.all(finite_complex(h(1:n,1:n)))) then
      errmsg='response audit: nonfinite original dynamic stiffness'; return
    end if
    if(allocated(state%original)) deallocate(state%original)
    allocate(state%original(n,n),work(n))
    state%original=h(1:n,1:n)
    state%anorm=zlange('1',n,n,state%original,n,work)
    if(.not.ieee_is_finite(state%anorm).or.state%anorm<=0._wp) then
      errmsg='response audit: zero/nonfinite original operator norm'; return
    end if
    ok=0
  end subroutine

  subroutine response_condition(lu,n,state,errmsg,ok)
    complex(wp), intent(in) :: lu(:,:)
    integer, intent(in) :: n
    type(response_quality_t), intent(inout) :: state
    character(len=*), intent(out) :: errmsg
    integer, intent(out) :: ok
    complex(wp), allocatable :: work(:),a(:,:)
    real(wp), allocatable :: rwork(:)
    integer :: info
    ok=-1; errmsg=''
    if(.not.allocated(state%original)) then
      errmsg='response audit: original matrix was not captured'; return
    end if
    allocate(work(2*n),rwork(2*n),a(n,n)); a=lu(1:n,1:n)
    call zgecon('1',n,a,n,state%anorm,state%rcond,work,rwork,info)
    if(info/=0.or..not.ieee_is_finite(state%rcond)) then
      errmsg='response audit: ZGECON failed'; return
    end if
    ok=0
  end subroutine

  subroutine response_report(rpm,omega,x,force,state,errmsg,ok)
    real(wp), intent(in) :: rpm,omega
    complex(wp), intent(in) :: x(:),force(:)
    type(response_quality_t), intent(in) :: state
    character(len=*), intent(out) :: errmsg
    integer, intent(out) :: ok
    complex(wp), allocatable :: residual(:),d(:,:),physical(:),constraint(:)
    real(wp), allocatable :: work(:)
    complex(wp) :: iw
    real(wp) :: normx,normf,den,full_error,physical_error,constraint_error,dnorm
    integer :: n,p
    character(len=24) :: status
    ok=-1; errmsg=''
    if(.not.allocated(state%original)) then
      errmsg='response audit: missing original matrix'; return
    end if
    n=size(state%original,1); p=n/2
    if(size(x)<n.or.size(force)<n) then
      errmsg='response audit: solution/RHS dimension mismatch'; return
    end if
    if(.not.all(finite_complex(x(1:n))).or..not.all(finite_complex(force(1:n)))) then
      errmsg='response audit: solution or applied force is nonfinite'; return
    end if
    allocate(residual(n),d(p,p),physical(p),constraint(p),work(p))
    iw=cmplx(0._wp,omega,wp)
    normx=sum(abs(x(1:n))); normf=sum(abs(force(1:n)))
    residual=matmul(state%original,x(1:n))-force(1:n)
    den=state%anorm*normx+normf
    if(.not.ieee_is_finite(den)) then
      errmsg='response audit: residual scale overflow'; return
    end if
    full_error=0._wp
    if(den>tiny(1._wp)) full_error=sum(abs(residual))/den
    ! Derive D from the matrix actually solved, including foundation impedance.
    d=state%original(1:p,1:p)+iw*state%original(1:p,p+1:n)
    physical=matmul(d,x(1:p))-force(1:p)
    dnorm=zlange('1',p,p,d,p,work)
    den=dnorm*sum(abs(x(1:p)))+sum(abs(force(1:p)))
    physical_error=0._wp
    if(den>tiny(1._wp)) physical_error=sum(abs(physical))/den
    constraint=x(p+1:n)-iw*x(1:p)
    den=sum(abs(x(p+1:n)))+abs(omega)*sum(abs(x(1:p)))
    constraint_error=0._wp
    if(den>tiny(1._wp)) constraint_error=sum(abs(constraint))/den
    if(.not.ieee_is_finite(full_error).or..not.ieee_is_finite(physical_error).or. &
       .not.ieee_is_finite(constraint_error)) then
      errmsg='response audit: nonfinite equilibrium residual'; return
    end if
    status='DIAGNOSTIC_OK'
    if(state%rcond<sqrt(epsilon(1._wp))) status='ILL_CONDITIONED_H'
    if(max(full_error,physical_error,constraint_error)>1.e-8_wp) status='HIGH_RESIDUAL'
    if(any(abs(force(p+1:n))>0._wp)) then
      errmsg='response audit: reduced equilibrium assumes a zero auxiliary RHS'; return
    end if
    write(*,'(a,1x,es24.16e3,1x,a,5(1x,es24.16e3))') &
      'RD_AUDIT_V1 RESPONSE_H',rpm,trim(status),state%rcond,state%anorm, &
      full_error,physical_error,constraint_error
    ! A small RCOND at a physical resonance is a warning, not an automatic
    ! rejection or an invitation to change damping. NaN/overflow is rejected.
    ok=0
  end subroutine
end module rd_response_quality
