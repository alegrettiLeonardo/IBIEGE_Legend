! F03/F04/F05. Physical eigensolves, adaptive continuation, and refined crossings.
! DW retains its existing meaning: requested NUMBER OF SPEED SAMPLES, not rpm step.
module rd_campbell_engine
  use rd_kinds, only: wp,lrk
  use rd_campbell_families, only: run_family_campbell
  use com_mat, only: mm,mg,dm
  use com_mtk, only: mk1
  use com_epmq, only: modal_valid
  use rd_modal_metrics, only: spectrum_report,eigen_root_status,root_finite,finite_complex
  use rd_modal_tracking, only: mass_factor,track_modes
  use, intrinsic :: ieee_arithmetic, only: ieee_is_finite
  implicit none
  private
  integer, parameter :: max_physical=500,max_samples=500,max_modes=19,max_orders=9
  real(wp), parameter :: root_relative_tol=1.e-8_wp
  public :: run_campbell, run_campbell_vectors, campbell_point, refined_speed_target
  interface
    subroutine prpkc(rad,n,pp,k,c,fx,errmsg,ok)
      import wp,lrk
      real(lrk) :: rad
      integer :: n,pp,fx,ok
      complex(wp) :: k(500,500),c(500,500)
      character(len=99) :: errmsg
    end subroutine
    subroutine adjeig(aa,bb,fi,psi,avl,n,fpdm,nmax,novec,errmsg,ok)
      import wp
      integer :: n,fpdm,nmax,ok
      complex(wp) :: aa(nmax,nmax),bb(nmax,nmax),fi(fpdm,fpdm),psi(fpdm,fpdm),avl(nmax)
      logical :: novec
      character(len=99) :: errmsg
    end subroutine
    subroutine s_campbl(ncf,ncc,npi,ncs,nit,gi,rcr,fcr,dmp,av,omg,rpmi,rpmf, &
        spdn,nini,ndw,mtg,mxc,mxm,std,plt,errmsg,ok)
      import wp,lrk
      integer :: ncf,ncc,npi,nit,mtg,mxc,mxm,ok,ncs(mxm)
      real(lrk) :: gi(mtg,mtg),rcr(mxm,mxc),fcr(mxm,mxc),dmp(mxm),omg(mtg)
      real(lrk) :: rpmi,rpmf,spdn,nini,ndw
      complex(wp) :: av(mtg,mtg)
      logical :: std,plt
      character(len=99) :: errmsg
    end subroutine
  end interface
contains
  subroutine run_campbell(pp,variable,std,plt,errmsg,ok)
    ! Dispatch on actual matrix data; never discard a nonzero imaginary term.
    ! The individual-vector route is retained for complex physical pencils.
    use com_cpb, only: nini
    integer, intent(in) :: pp
    logical, intent(in) :: variable,std,plt
    character(len=99), intent(out) :: errmsg
    integer, intent(out) :: ok
    complex(wp), allocatable :: kb(:,:),cb(:,:)
    real(lrk) :: omega
    integer :: fx
    ok=-1;errmsg=''
    if(dm<1.or.dm>max_physical.or..not.ieee_is_finite(nini))then
      errmsg='Campbell: invalid physical dimension or initial rpm';return
    endif
    if(nini<0._lrk)then
      errmsg='Campbell: negative initial rpm';return
    endif
    allocate(kb(max_physical,max_physical),cb(max_physical,max_physical))
    fx=1;if(variable)fx=0
    omega=nini*acos(-1._lrk)/30._lrk
    call prpkc(omega,dm,pp,kb,cb,fx,errmsg,ok)
    if(ok/=0)return
    if(all(aimag(kb(1:dm,1:dm))==0._wp).and.all(aimag(cb(1:dm,1:dm))==0._wp))then
      write(*,'(a)') 'RD_AUDIT_V1 NOTE CAMPBELL_ROUTE_SPECTRAL_FAMILIES_REAL'
      call run_family_campbell(pp,variable,std,plt,errmsg,ok)
    else
      write(*,'(a)') 'RD_AUDIT_V1 NOTE CAMPBELL_ROUTE_LEGACY_COMPLEX_VECTORS'
      call run_campbell_vectors(pp,variable,std,plt,errmsg,ok)
    endif
  end subroutine run_campbell

  subroutine campbell_point(pp,variable,rpm,fi,av,valid,errmsg,ok)
    integer, intent(in) :: pp
    logical, intent(in) :: variable
    real(wp), intent(in) :: rpm
    complex(wp), allocatable, intent(out) :: fi(:,:),av(:)
    logical, allocatable, intent(out) :: valid(:)
    character(len=99), intent(out) :: errmsg
    integer, intent(out) :: ok
    complex(wp), allocatable :: aa(:,:),bb(:,:),psi(:,:),kb(:,:),cb(:,:)
    real(lrk) :: omega
    integer :: n,fx
    ok=-1; errmsg=''
    if(dm<1.or.dm>max_physical.or..not.ieee_is_finite(rpm).or.rpm<0._wp) then
      errmsg='Campbell: invalid physical dimension or prescribed rpm'; return
    end if
    n=2*dm; omega=real(rpm*acos(-1._wp)/30._wp,lrk)
    allocate(aa(n,n),bb(n,n),fi(n,n),psi(n,n),av(n),valid(n))
    allocate(kb(max_physical,max_physical),cb(max_physical,max_physical))
    fx=1; if(variable) fx=0
    call prpkc(omega,dm,pp,kb,cb,fx,errmsg,ok)
    if(ok/=0) return
    aa=cmplx(0._wp,0._wp,wp); bb=aa
    aa(1:dm,1:dm)=cb(1:dm,1:dm)+omega*mg(1:dm,1:dm)
    aa(1:dm,dm+1:n)=cmplx(mm(1:dm,1:dm),0._wp,wp)
    aa(dm+1:n,1:dm)=aa(1:dm,dm+1:n)
    bb(1:dm,1:dm)=kb(1:dm,1:dm)+mk1(1:dm,1:dm)
    bb(dm+1:n,dm+1:n)=-cmplx(mm(1:dm,1:dm),0._wp,wp)
    call adjeig(aa,bb,fi,psi,av,n,n,n,.false.,errmsg,ok)
    if(ok/=0) return
    valid=modal_valid(1:n).and.(eigen_root_status(1:n)==root_finite)
  end subroutine

  subroutine run_campbell_vectors(pp,variable,std,plt,errmsg,ok)
    use com_cpb, only: nini,nfin,dw,npi,ncc,imt
    use com_cpbd, only: dmp,nit
    use com_cpbn, only: spdn
    integer, intent(in) :: pp
    logical, intent(in) :: variable,std,plt
    character(len=99), intent(out) :: errmsg
    integer, intent(out) :: ok
    complex(wp), allocatable :: fi(:,:),av(:),previous(:,:),history(:,:,:),tracked(:,:)
    complex(wp), allocatable :: published(:,:),rank_previous(:,:)
    real(wp), allocatable :: lmass(:,:),samples(:),frequency(:,:),quality(:)
    real(lrk), allocatable :: plot(:,:),omegas(:),rcr(:,:),fcr(:,:)
    integer, allocatable :: selected(:),clusters(:),rank_sel(:),rank_clusters(:)
    real(wp), allocatable :: rank_quality(:)
    logical, allocatable :: valid(:)
    integer :: ns,requested,i,j,k,h,c,info,attempt,root_count(max_orders),root_branch(max_orders,max_modes)
    real(wp) :: step,target,next_step,start_rpm,end_rpm,omega,h0,h1,h2,xroot,froot,width,resid
    real(wp) :: refined_target,last_accepted
    integer :: refinement_status
    real(lrk) :: plot_step,rpmi,rpmf
    ok=-1; errmsg=''
    if(dm<1.or.dm>max_physical) then
      errmsg='Campbell: physical dimension must be 1..500'; return
    end if
    if(ncc<=0) ncc=5
    if(npi<=2) npi=50
    if(nit==0) then
      nit=3; dmp(1:3)=[1._lrk,2._lrk,0.5_lrk]
    end if
    if(ncc>max_modes.or.ncc<1.or.npi>max_samples.or.npi<2.or.nit<1.or.nit>max_orders) then
      errmsg='Campbell: limits are 1..19 modes, 2..500 display points, 1..9 orders'; return
    end if
    if(.not.ieee_is_finite(dw).or.dw<2._lrk.or.dw>real(max_samples,lrk)) then
      errmsg='Campbell: DW must specify 2..500 speed samples, not an rpm increment'; return
    end if
    requested=nint(dw)
    if(abs(dw-real(requested,lrk))>64._lrk*epsilon(dw)*max(1._lrk,abs(dw))) then
      errmsg='Campbell: DW sample count must be an integer'; return
    end if
    start_rpm=real(nini,wp); end_rpm=real(nfin,wp)
    if(.not.ieee_is_finite(start_rpm).or..not.ieee_is_finite(end_rpm).or. &
       start_rpm<0._wp.or.end_rpm<=start_rpm) then
      errmsg='Campbell: expected 0 <= initial rpm < final rpm'; return
    end if
    if(.not.all(ieee_is_finite(dmp(1:nit))).or.any(dmp(1:nit)<=0._lrk)) then
      errmsg='Campbell: excitation orders must be finite and strictly positive'; return
    end if
    do i=1,nit
      do j=1,i-1
        if(abs(dmp(i)-dmp(j))<64._lrk*epsilon(1._lrk)*max(dmp(i),dmp(j))) then
          errmsg='Campbell: duplicate excitation order'; return
        end if
      end do
    end do
    call mass_factor(mm(1:dm,1:dm),lmass,errmsg,info); if(info/=0) return
    allocate(previous(dm,ncc),rank_previous(dm,ncc),history(dm,ncc,max_samples))
    allocate(tracked(max_samples,ncc),published(max_samples,max_samples))
    allocate(samples(max_samples),frequency(max_samples,ncc),quality(ncc),rank_quality(ncc))
    allocate(selected(ncc),clusters(ncc),rank_sel(ncc),rank_clusters(ncc))
    allocate(plot(max_samples,max_samples),omegas(max_samples),rcr(max_orders,max_modes),fcr(max_orders,max_modes))
    previous=cmplx(0._wp,0._wp,wp); rank_previous=previous
    published=cmplx(0._wp,0._wp,wp); plot=0._lrk; omegas=0._lrk
    rcr=0._lrk; fcr=0._lrk; root_count=0; root_branch=0
    step=(end_rpm-start_rpm)/real(requested-1,wp); next_step=step
    ns=0; target=start_rpm; attempt=0
    write(*,'(a,1x,i0,1x,i0)') 'RD_AUDIT_V1 CAMPBELL_POLICY',imt,requested
    call progress_stage('CAMPBELL','solve eigenproblem at each speed')
    do
      ! HF02: a successful repeat at the old rpm is NOT a continuation step.
      if(ns>0) then
        if(target<=samples(ns)) then
          errmsg='Campbell: speed stagnation before eigensolve; no duplicate sample accepted'
          write(*,'(a,2(1x,es24.16e3))') 'RD_AUDIT_V1 CAMPBELL_STAGNATION',samples(ns),target
          return
        end if
      end if
      call campbell_point(pp,variable,target,fi,av,valid,errmsg,info)
      if(info/=0) return
      call track_modes(lmass,fi,av,valid,previous,ns==0,selected,quality,clusters,errmsg,info)
      if(info/=0) then
        call spectrum_report('CAMPBELL_REJECTED',target,av)
        ! Preserve the reason BEFORE another successful trial clears errmsg.
        last_accepted=target
        if(ns>0) last_accepted=samples(ns)
        write(*,'(a,2(1x,es24.16e3),2(1x,i0),1x,a)') &
          'RD_AUDIT_V1 TRACK_REJECT',last_accepted,target,attempt,info,trim(errmsg)
        if(info/=-2.or.ns==0.or.attempt>=12) return
        call refined_speed_target(samples(ns),target,refined_target,refinement_status)
        if(refinement_status/=0) then
          write(*,'(a,2(1x,es24.16e3))') 'RD_AUDIT_V1 CAMPBELL_STAGNATION',samples(ns),target
          errmsg='Campbell: no representable refinement step; see TRACK_REJECT; no forced mode match'
          return
        end if
        target=refined_target; next_step=target-samples(ns)
        attempt=attempt+1
        write(*,'(a,1x,es24.16e3,1x,i0)') 'RD_AUDIT_V1 CAMPBELL_REFINE',target,attempt
        cycle
      end if
      if(ns==max_samples) then
        errmsg='Campbell: adaptive sampling exceeded 500 points; narrow range or refine inputs'; return
      end if
      if(ns>0) then
        if(target<=samples(ns)) then
          errmsg='Campbell: refusing a duplicate or backward accepted speed'; return
        end if
      end if
      ns=ns+1; samples(ns)=target
      previous=fi(1:dm,selected); history(:,:,ns)=previous; tracked(ns,:)=av(selected)
      if(imt==0) then
        ! Keep the legacy user-requested rank view. Crossings below are explicitly
        ! physical continuations from the initial displayed modes, not rank jumps.
        call track_modes(lmass,fi,av,valid,rank_previous,.true.,rank_sel,rank_quality, &
          rank_clusters,errmsg,info)
        if(info/=0) return
        published(ns,1:ncc)=av(rank_sel)
      else
        published(ns,1:ncc)=tracked(ns,:)
      end if
      frequency(ns,:)=abs(aimag(published(ns,1:ncc)))
      omegas(ns)=real(target*acos(-1._wp)/30._wp,lrk)
      call spectrum_report('CAMPBELL',target,av)
      write(*,'(a,1x,es24.16e3,1x,es24.16e3)') 'RD_AUDIT_V1 TRACK_MIN_MAC',target,minval(quality)
      do k=1,ncc
        write(*,'(a,1x,es24.16e3,2(1x,i0),3(1x,es24.16e3))') 'RD_AUDIT_V1 TRACK_MODE', &
          target,k,selected(k),real(av(selected(k)),wp),aimag(av(selected(k))),quality(k)
      end do
      ! Nominal-grid milestones only: adaptive samples do not change DW.
      ! The two final milestones are root refinement and successful reporting.
      call progress_update('CAMPBELL',merge(requested, &
        min(requested,1+int((target-start_rpm)/step)),target>=end_rpm), &
        requested+2,real(target,lrk),'RPM')
      if(target>=end_rpm) exit
      attempt=0; next_step=min(step,2._wp*next_step)
      target=min(end_rpm,target+next_step)
    end do
    if(imt==0) write(*,'(a)') &
      'RD_AUDIT_V1 NOTE RANK_VIEW_CROSSINGS_USE_CONTINUED_INITIAL_MODES'
    call progress_stage('CAMPBELL','refine physical crossings')
    ! Root discovery is performed on physically continued eigensolutions, NOT
    ! on the display interpolation and NOT on abs(a).
    do h=1,nit
      do k=1,ncc
        do i=1,ns-1
          h0=crossing_value(tracked(i,k),samples(i),real(dmp(h),wp))
          h1=crossing_value(tracked(i+1,k),samples(i+1),real(dmp(h),wp))
          if(is_root(h0,tracked(i,k),samples(i),real(dmp(h),wp))) then
            call store_root(h,k,samples(i),abs(aimag(tracked(i,k))),0._wp, &
              abs(h0)/max(1._wp,abs(aimag(tracked(i,k)))), 'ENDPOINT',info)
            if(info/=0) return
          end if
          if(sign(1._wp,h0)/=sign(1._wp,h1).and.h0/=0._wp.and.h1/=0._wp) then
            call refine_crossing(samples(i),samples(i+1),tracked(i,k),tracked(i+1,k), &
              history(:,k,i),history(:,k,i+1),real(dmp(h),wp),xroot,froot,width,resid,info)
            if(info/=0) return
            call store_root(h,k,xroot,froot,width,resid,'BRACKETED',info)
            if(info/=0) return
          end if
        end do
        h1=crossing_value(tracked(ns,k),samples(ns),real(dmp(h),wp))
        if(is_root(h1,tracked(ns,k),samples(ns),real(dmp(h),wp))) then
          call store_root(h,k,samples(ns),abs(aimag(tracked(ns,k))),0._wp, &
            abs(h1)/max(1._wp,abs(aimag(tracked(ns,k)))), 'ENDPOINT',info)
          if(info/=0) return
        end if
        ! A same-sign local minimum can indicate a tangent or two unresolved
        ! crossings. Minimize |h| with actual eigensolves; call it NEAR_ZERO,
        ! not a proof that every tangent on a continuous speed band was found.
        do i=2,ns-1
          h0=crossing_value(tracked(i-1,k),samples(i-1),real(dmp(h),wp))
          h1=crossing_value(tracked(i,k),samples(i),real(dmp(h),wp))
          h2=crossing_value(tracked(i+1,k),samples(i+1),real(dmp(h),wp))
          if(sign(1._wp,h0)/=sign(1._wp,h1).or.sign(1._wp,h1)/=sign(1._wp,h2)) cycle
          if(abs(h1)>=min(abs(h0),abs(h2))) cycle
          call refine_near_zero(samples(i-1),samples(i+1),history(:,k,i),real(dmp(h),wp), &
            xroot,froot,width,resid,info)
          if(info<0) return
          if(info==0) then
            call store_root(h,k,xroot,froot,width,resid,'NEAR_ZERO',info)
            if(info/=0) return
          end if
        end do
      end do
    end do
    call progress_update('CAMPBELL',requested+1,requested+2,real(end_rpm,lrk),'RPM')
    call progress_stage('CAMPBELL','prepare display interpolation')
    ! Preserve ascending speed order in the legacy rectangular crossing table.
    do h=1,nit
      do i=2,root_count(h)
        xroot=real(rcr(h,i),wp); froot=real(fcr(h,i),wp); k=root_branch(h,i); j=i-1
        do while(j>=1)
          if(real(rcr(h,j),wp)<=xroot) exit
          rcr(h,j+1)=rcr(h,j); fcr(h,j+1)=fcr(h,j)
          root_branch(h,j+1)=root_branch(h,j); j=j-1
        end do
        rcr(h,j+1)=real(xroot,lrk); fcr(h,j+1)=real(froot,lrk); root_branch(h,j+1)=k
      end do
    end do
    ! Display-only piecewise-linear interpolation: no additional root accuracy
    ! is implied by the number of points requested for a plot.
    c=1
    do j=1,npi
      target=start_rpm+(end_rpm-start_rpm)*real(j-1,wp)/real(npi-1,wp)
      do while(c<ns-1)
        if(samples(c+1)>=target) exit
        c=c+1
      end do
      omega=(target-samples(c))/(samples(c+1)-samples(c))
      plot(1:ncc,j)=real((1._wp-omega)*frequency(c,:)+omega*frequency(c+1,:),lrk)
    end do
    rpmi=real(start_rpm,lrk); rpmf=real(end_rpm,lrk)
    plot_step=real((end_rpm-start_rpm)*acos(-1._wp)/(30._wp*real(npi-1,wp)),lrk)
    call progress_stage('CAMPBELL','write Campbell results')
    call s_campbl(ns,ncc,npi,root_count,nit,plot,rcr,fcr,dmp,published,omegas, &
      rpmi,rpmf,spdn,nini,plot_step,max_samples,max_modes,max_orders,std,plt,errmsg,info)
    if(info/=0) return
    call progress_update('CAMPBELL',requested+2,requested+2,real(end_rpm,lrk),'RPM')
    write(*,'(a,1x,i0)') 'RD_AUDIT_V1 CROSSING_COVERAGE FINITE_ADAPTIVE_GRID',ns
    ok=0
  contains
    subroutine evaluate(rpm,reference,order,a,q,value,istat)
      real(wp), intent(in) :: rpm,order
      complex(wp), intent(in) :: reference(:)
      complex(wp), intent(out) :: a,q(:)
      real(wp), intent(out) :: value
      integer, intent(out) :: istat
      complex(wp), allocatable :: vec(:,:),poles(:),ref(:,:)
      logical, allocatable :: usable(:)
      integer :: pick(1),group(1)
      real(wp) :: mac(1)
      allocate(ref(dm,1)); ref(:,1)=reference
      call campbell_point(pp,variable,rpm,vec,poles,usable,errmsg,istat)
      if(istat/=0) return
      call spectrum_report('CAMPBELL_ROOT',rpm,poles)
      call track_modes(lmass,vec,poles,usable,ref,.false.,pick,mac,group,errmsg,istat)
      if(istat/=0) return
      a=poles(pick(1)); q=vec(1:dm,pick(1)); value=crossing_value(a,rpm,order)
    end subroutine

    subroutine refine_crossing(left,right,aleft,aright,qleft,qright,order,x,f,width,residual,istat)
      real(wp), intent(in) :: left,right,order
      complex(wp), intent(in) :: aleft,aright,qleft(:),qright(:)
      real(wp), intent(out) :: x,f,width,residual
      integer, intent(out) :: istat
      real(wp) :: lo,hi,fl,fh,fm,tolerance
      complex(wp) :: a,ql(dm),qh(dm),q(dm)
      integer :: iteration
      lo=left; hi=right; ql=qleft; qh=qright
      fl=crossing_value(aleft,lo,order); fh=crossing_value(aright,hi,order)
      istat=-1
      do iteration=1,80
        x=0.5_wp*(lo+hi)
        call evaluate(x,ql,order,a,q,fm,istat); if(istat/=0) return
        residual=abs(fm)/max(1._wp,abs(aimag(a)),order*x*acos(-1._wp)/30._wp)
        width=hi-lo; tolerance=max(1.e-6_wp,1.e-9_wp*max(abs(lo),abs(hi)))
        if(residual<=root_relative_tol.and.width<=tolerance) then
          f=abs(aimag(a)); istat=0; return
        end if
        if(fm==0._wp) then
          f=abs(aimag(a)); width=0._wp; residual=0._wp; istat=0; return
        end if
        if(sign(1._wp,fl)/=sign(1._wp,fm)) then
          hi=x; fh=fm; qh=q
        else
          lo=x; fl=fm; ql=q
        end if
      end do
      errmsg='Campbell: physical crossing did not converge in 80 eigensolves'; istat=-1
    end subroutine

    subroutine refine_near_zero(left,right,reference,order,x,f,width,residual,istat)
      real(wp), intent(in) :: left,right,order
      complex(wp), intent(in) :: reference(:)
      real(wp), intent(out) :: x,f,width,residual
      integer, intent(out) :: istat
      real(wp), parameter :: golden=0.6180339887498948482_wp
      real(wp) :: lo,hi,x1,x2,hv1,hv2
      complex(wp) :: a1,a2,q(dm)
      integer :: iteration
      lo=left; hi=right; x1=hi-golden*(hi-lo); x2=lo+golden*(hi-lo)
      call evaluate(x1,reference,order,a1,q,hv1,istat); if(istat/=0) return
      call evaluate(x2,reference,order,a2,q,hv2,istat); if(istat/=0) return
      do iteration=1,70
        if(hi-lo<=max(1.e-6_wp,1.e-9_wp*max(abs(lo),abs(hi)))) exit
        if(abs(hv1)<abs(hv2)) then
          hi=x2; x2=x1; hv2=hv1; a2=a1; x1=hi-golden*(hi-lo)
          call evaluate(x1,reference,order,a1,q,hv1,istat)
        else
          lo=x1; x1=x2; hv1=hv2; a1=a2; x2=lo+golden*(hi-lo)
          call evaluate(x2,reference,order,a2,q,hv2,istat)
        end if
        if(istat/=0) return
      end do
      x=x1; f=abs(aimag(a1)); residual=abs(hv1)/max(1._wp,f,order*x*acos(-1._wp)/30._wp)
      if(abs(hv2)<abs(hv1)) then
        x=x2; f=abs(aimag(a2)); residual=abs(hv2)/max(1._wp,f,order*x*acos(-1._wp)/30._wp)
      end if
      width=hi-lo; istat=1
      if(residual<=root_relative_tol) istat=0
      write(*,'(a,3(1x,es24.16e3),1x,i0)') 'RD_AUDIT_V1 TANGENCY_SEARCH',x,width,residual,istat
    end subroutine

    subroutine store_root(order_index,branch,rpm,freq,bracket,residual,kind,istat)
      integer, intent(in) :: order_index,branch
      real(wp), intent(in) :: rpm,freq,bracket,residual
      character(len=*), intent(in) :: kind
      integer, intent(out) :: istat
      integer :: t,pos
      real(wp) :: previous_rpm
      istat=0
      do t=1,root_count(order_index)
        previous_rpm=real(rcr(order_index,t),wp)*30._wp/acos(-1._wp)
        if(root_branch(order_index,t)/=branch) cycle
        if(abs(previous_rpm-rpm)<=max(1.e-6_wp,1.e-8_wp*abs(rpm))) return
      end do
      pos=root_count(order_index)+1
      if(pos>max_modes) then
        errmsg='Campbell: more than 19 crossings per order; output capacity exceeded, no truncation'
        istat=-1; return
      end if
      root_count(order_index)=pos; root_branch(order_index,pos)=branch
      rcr(order_index,pos)=real(rpm*acos(-1._wp)/30._wp,lrk)
      fcr(order_index,pos)=real(freq,lrk)
      write(*,'(a,2(1x,i0),1x,a,5(1x,es24.16e3))') 'RD_AUDIT_V1 CROSSING', &
        order_index,branch,trim(kind),real(dmp(order_index),wp),rpm,freq,bracket,residual
    end subroutine
  end subroutine

  pure subroutine refined_speed_target(last,rejected,candidate,istat)
    ! Strict open-interval refinement. If rounding returns either endpoint,
    ! do not call the eigensolver again at an already accepted speed.
    real(wp), intent(in) :: last,rejected
    real(wp), intent(out) :: candidate
    integer, intent(out) :: istat
    real(wp) :: width,resolution
    candidate=last; istat=-1
    if(.not.ieee_is_finite(last).or..not.ieee_is_finite(rejected)) return
    if(last<0._wp.or.rejected<=last) return
    width=rejected-last
    resolution=64._wp*spacing(max(1._wp,abs(last),abs(rejected)))
    if(width<=resolution) return
    candidate=last+0.5_wp*width
    if(candidate<=last.or.candidate>=rejected) return
    istat=0
  end subroutine refined_speed_target

  pure real(wp) function crossing_value(a,rpm,order)
    complex(wp), intent(in) :: a
    real(wp), intent(in) :: rpm,order
    crossing_value=abs(aimag(a))-order*rpm*acos(-1._wp)/30._wp
  end function

  pure logical function is_root(h,a,rpm,order)
    real(wp), intent(in) :: h,rpm,order
    complex(wp), intent(in) :: a
    is_root=abs(h)<=root_relative_tol*max(1._wp,abs(aimag(a)),order*rpm*acos(-1._wp)/30._wp)
  end function
end module rd_campbell_engine
