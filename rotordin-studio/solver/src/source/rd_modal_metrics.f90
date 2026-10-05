! Audit F02/F03/F04. Prescribed-spin convention: BB*v=a*AA*v, s=-a.
! This module does NOT reinterpret torsional or synchronous auxiliary poles.
module rd_modal_metrics
  use rd_kinds, only: wp
  use, intrinsic :: ieee_arithmetic, only: ieee_is_finite, ieee_value, ieee_quiet_nan
  implicit none
  private
  integer, parameter, public :: root_finite=0, root_infinite=1, root_indeterminate=2, &
    root_nonfinite=3, root_out_of_range=4, root_underflow=5
  type, public :: modal_metrics_t
    real(wp) :: growth=0._wp, wd=0._wp, radius=0._wp, zeta=0._wp, delta=0._wp
    logical :: valid=.false., zeta_defined=.false., delta_defined=.false., oscillatory=.false.
  end type
  complex(wp), allocatable, save, public :: eigen_alpha(:), eigen_beta(:)
  integer, allocatable, save, public :: eigen_root_status(:)
  public :: generalized_quotient, record_generalized_roots, prescribed_metrics
  public :: orbit_component, whirl_code, spectrum_report, finite_complex
contains
  elemental logical function finite_complex(z)
    complex(wp), intent(in) :: z
    finite_complex=ieee_is_finite(real(z,wp)).and.ieee_is_finite(aimag(z))
  end function

  pure subroutine generalized_quotient(alpha,beta,a,status)
    complex(wp), intent(in) :: alpha,beta
    complex(wp), intent(out) :: a
    integer, intent(out) :: status
    complex(wp) :: z
    real(wp) :: sa,sb,sz,logratio,factor
    a=cmplx(huge(1._wp),0._wp,wp)
    status=root_nonfinite
    if (.not.finite_complex(alpha).or..not.finite_complex(beta)) return
    sa=max(abs(real(alpha,wp)),abs(aimag(alpha)))
    sb=max(abs(real(beta,wp)),abs(aimag(beta)))
    if (sb==0._wp) then
      status=root_infinite
      if (sa==0._wp) status=root_indeterminate
      return
    end if
    if (sa==0._wp) then
      a=cmplx(0._wp,0._wp,wp); status=root_finite; return
    end if
    ! Division of independently normalized complex numbers cannot overflow.
    z=(alpha/sa)/(beta/sb)
    sz=abs(z)
    logratio=log(sa)-log(sb)
    if (logratio+log(sz)>=log(huge(1._wp))) then
      status=root_out_of_range; return
    end if
    ! Conservatively retain an underflow status, rather than call a lost pole zero.
    if (logratio+log(sz)<log(tiny(1._wp))) then
      a=cmplx(0._wp,0._wp,wp); status=root_underflow; return
    end if
    if(max(sa,sb)<sqrt(huge(1._wp))/4._wp.and.min(sa,sb)>4._wp*sqrt(tiny(1._wp))) then
      ! Preserve the ordinary quotient in the safely representable regime.
      a=alpha/beta
    else
      factor=exp(0.5_wp*logratio)
      a=(z*factor)*factor
    end if
    status=root_finite
    if (.not.finite_complex(a)) then
      a=cmplx(huge(1._wp),0._wp,wp); status=root_out_of_range
    end if
  end subroutine

  subroutine record_generalized_roots(alpha,beta,idx,n)
    integer, intent(in) :: n,idx(n)
    complex(wp), intent(in) :: alpha(n),beta(n)
    complex(wp) :: value
    integer :: j
    if (allocated(eigen_alpha)) deallocate(eigen_alpha,eigen_beta,eigen_root_status)
    allocate(eigen_alpha(n),eigen_beta(n),eigen_root_status(n))
    do j=1,n
      eigen_alpha(j)=alpha(idx(j)); eigen_beta(j)=beta(idx(j))
      call generalized_quotient(eigen_alpha(j),eigen_beta(j),value,eigen_root_status(j))
    end do
  end subroutine

  pure function prescribed_metrics(a) result(m)
    complex(wp), intent(in) :: a
    type(modal_metrics_t) :: m
    real(wp) :: scale,norm_scaled,r,ratio,pi
    m=modal_metrics_t()
    m%zeta=ieee_value(0._wp,ieee_quiet_nan)
    m%delta=ieee_value(0._wp,ieee_quiet_nan)
    if (.not.finite_complex(a)) return
    ! Historical ADJEIG sentinel is never a physical stable real root.
    if (max(abs(real(a,wp)),abs(aimag(a)))>=huge(1._wp)) return
    r=real(a,wp); m%growth=-r; m%wd=abs(aimag(a))
    scale=max(abs(r),m%wd)
    m%valid=.true.
    if (scale==0._wp) return
    norm_scaled=sqrt((r/scale)**2+(m%wd/scale)**2)
    if (scale>huge(1._wp)/norm_scaled) then
      m%valid=.false.; return
    end if
    m%radius=scale*norm_scaled
    m%zeta=(r/scale)/norm_scaled; m%zeta_defined=.true.
    ! This is a numerical oscillatory threshold, not a damping-ratio cutoff.
    m%oscillatory=m%wd>max(1.e-12_wp,64._wp*epsilon(1._wp)*scale)
    if (.not.m%oscillatory) return
    pi=acos(-1._wp)
    if (m%wd<1._wp) then
      if (abs(r)>huge(1._wp)*m%wd/(2._wp*pi)) return
    end if
    ratio=r/m%wd
    if (abs(ratio)>huge(1._wp)/(2._wp*pi)) return
    m%delta=2._wp*pi*ratio
    m%delta_defined=ieee_is_finite(m%delta)
  end function

  elemental real(wp) function orbit_component(phi,phase)
    complex(wp), intent(in) :: phi
    real(wp), intent(in) :: phase
    orbit_component=real(phi,wp)*cos(phase)+aimag(phi)*sin(phase)
  end function

  function whirl_code(x,z,spin,relative_node_tol,linear_tol) result(code)
    complex(wp), intent(in) :: x(:),z(:)
    real(wp), intent(in) :: spin
    real(wp), intent(in), optional :: relative_node_tol,linear_tol
    character(len=2) :: code
    real(wp) :: scale,maximum,chi,ntol,ltol,den
    complex(wp) :: xn,zn
    logical :: fw,bw,active
    integer :: i
    ntol=1.e-10_wp; if(present(relative_node_tol)) ntol=relative_node_tol
    ltol=1.e-6_wp; if(present(linear_tol)) ltol=linear_tol
    code='ND'; fw=.false.; bw=.false.; active=.false.; maximum=0._wp
    if (size(x)/=size(z).or.size(x)==0) return
    if (.not.all(finite_complex(x)).or..not.all(finite_complex(z))) then
      code='UD'; return
    end if
    do i=1,size(x)
      maximum=max(maximum,abs(x(i)),abs(z(i)))
    end do
    if(maximum<=tiny(1._wp)) return
    do i=1,size(x)
      scale=max(abs(x(i)),abs(z(i)))
      if(scale/maximum<=ntol) cycle
      active=.true.; xn=x(i)/scale; zn=z(i)/scale
      den=abs(xn)**2+abs(zn)**2
      chi=2._wp*aimag(xn*conjg(zn))/den
      if (abs(spin)<=1.e-12_wp) cycle
      if (spin<0._wp) chi=-chi
      if(chi>ltol) fw=.true.
      if(chi<-ltol) bw=.true.
    end do
    if (.not.active) return
    if(abs(spin)<=1.e-12_wp) then
      code='UD'
    else if(fw.and.bw) then
      code='MX'
    else if(fw) then
      code='FW'
    else if(bw) then
      code='BW'
    else
      code='LN'
    end if
  end function

  subroutine spectrum_report(context,rpm,a)
    character(len=*), intent(in) :: context
    real(wp), intent(in) :: rpm
    complex(wp), intent(in) :: a(:)
    type(modal_metrics_t) :: m
    integer :: j,nfinite,ninvalid,nunstable,nmarginal,nreal
    real(wp) :: maxgrowth,tol
    character(len=32) :: status
    logical :: have_metadata
    nfinite=0; ninvalid=0; nunstable=0; nmarginal=0; nreal=0
    maxgrowth=-huge(1._wp)
    have_metadata=.false.
    if(allocated(eigen_root_status)) have_metadata=size(eigen_root_status)==size(a)
    do j=1,size(a)
      if(have_metadata) then
        if(eigen_root_status(j)/=root_finite) then
          ninvalid=ninvalid+1
          write(*,'(a,1x,a,1x,es24.16e3,2(1x,i0),4(1x,es24.16e3))') &
            'RD_AUDIT_V1 ROOT_STATUS',trim(context),rpm,j,eigen_root_status(j), &
            real(eigen_alpha(j),wp),aimag(eigen_alpha(j)),real(eigen_beta(j),wp),aimag(eigen_beta(j))
          cycle
        end if
      end if
      m=prescribed_metrics(a(j))
      if(.not.m%valid) then
        ninvalid=ninvalid+1; cycle
      end if
      nfinite=nfinite+1; maxgrowth=max(maxgrowth,m%growth)
      if(.not.m%oscillatory) nreal=nreal+1
      tol=max(1.e-10_wp,128._wp*epsilon(1._wp)*m%radius)
      if(m%growth>tol) nunstable=nunstable+1
      if(abs(m%growth)<=tol) nmarginal=nmarginal+1
      if(m%growth>tol.or..not.m%oscillatory) then
        write(*,'(a,1x,a,1x,es24.16e3,1x,i0,3(1x,es24.16e3))') &
          'RD_AUDIT_V1 STABILITY_ROOT',trim(context),rpm,j,m%growth,-aimag(a(j)),m%wd
      end if
    end do
    if(nunstable>0) then
      status='UNSTABLE'
      if(ninvalid>0) status='UNSTABLE_INCOMPLETE_SPECTRUM'
    else if(ninvalid>0.or.nfinite==0) then
      status='INDETERMINATE'
    else if(nmarginal>0) then
      ! No unsupported Lyapunov-stability claim for imaginary-axis/defective roots.
      status='MARGINAL_UNRESOLVED'
    else
      status='ASYMPTOTICALLY_STABLE'
    end if
    if(nfinite==0) maxgrowth=ieee_value(0._wp,ieee_quiet_nan)
    write(*,'(a,1x,a,1x,es24.16e3,1x,a,1x,es24.16e3,5(1x,i0))') &
      'RD_AUDIT_V1 SPECTRUM',trim(context),rpm,trim(status),maxgrowth, &
      size(a),nfinite,ninvalid,nunstable,nreal
  end subroutine
end module rd_modal_metrics
