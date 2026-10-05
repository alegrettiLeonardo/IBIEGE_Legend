! HF03 integrated family-based Campbell for exactly real M/K/D.
! Historical individual-eigenvector and complex-pencil routes remain separate.
module rd_campbell_families
  use rd_kinds, only: wp,lrk
  use com_mat, only: mm,mg,dm
  use com_mtk, only: mk1
  use rd_modal_metrics, only: spectrum_report,record_generalized_roots
  use rd_spectral_families
  use, intrinsic :: ieee_arithmetic, only: ieee_is_finite,ieee_value,ieee_quiet_nan
  implicit none
  private
  integer, parameter :: max_physical=500,max_display=500,max_samples=1024,max_modes=19,max_orders=9
  real(wp), parameter :: root_relative_tol=1.e-8_wp
  public :: run_family_campbell
  interface
    subroutine prpkc(rad,n,pp,k,c,fx,errmsg,ok)
      import wp,lrk
      real(lrk) :: rad
      integer :: n,pp,fx,ok
      complex(wp) :: k(500,500),c(500,500)
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
  subroutine run_family_campbell(pp,variable,std,plt,errmsg,ok)
    use com_cpb, only: nini,nfin,dw,npi,ncc,imt
    use com_cpbd, only: dmp,nit
    use com_cpbn, only: spdn
    integer,intent(in)::pp
    logical,intent(in)::variable,std,plt
    character(len=99),intent(out)::errmsg
    integer,intent(out)::ok
    type(spectral_metric)::metric
    type(spectral_point)::previous,current,report_point,refleft
    type(spectral_checkpoint),allocatable::history(:)
    integer,allocatable::parents(:),root_slot(:,:)
    complex(wp),allocatable::published(:,:)
    real(lrk),allocatable::plot(:,:),omegas(:),rcr(:,:),fcr(:,:)
    real(wp),allocatable::leftfreq(:),rightfreq(:),freq(:),prevfreq(:),nextfreq(:)
    real(wp),allocatable::samples(:),frequency(:,:),quality(:),separation(:),residuals(:)
    integer::ns,requested,i,j,k,h,c,info,attempt,root_count(max_orders),slot,id,m,nmembers
    integer::p,q,root_keys(max_orders,max_modes)
    real(wp)::step,target,next_step,start_rpm,end_rpm,h0,h1,h2,xroot,froot,width,resid
    real(wp)::refined_target,event_span,w,nan,fr,prev_rpm
    real(lrk)::plot_step,rpmi,rpmf
    logical::event,already
    ok=-1;errmsg=''
    if(dm<1.or.dm>max_physical) then
      errmsg='Campbell: physical dimension must be 1..500'; return
    end if
    if(ncc<=0) ncc=5
    if(npi<=2) npi=50
    if(nit==0) then
      nit=3; dmp(1:3)=[1._lrk,2._lrk,0.5_lrk]
    end if
    if(ncc>max_modes.or.ncc<1.or.npi>max_display.or.npi<2.or.nit<1.or.nit>max_orders) then
      errmsg='Campbell: limits are 1..19 modes, 2..500 display points, 1..9 orders'; return
    end if
    if(.not.ieee_is_finite(dw).or.dw<2._lrk.or.dw>real(max_display,lrk)) then
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

    call spectral_initialize_metric(mm(1:dm,1:dm),max(1._wp,end_rpm*acos(-1._wp)/30._wp), &
      metric,errmsg,info)
    if(info/=0)return
    allocate(history(max_samples),samples(max_samples),quality(max_samples), &
      separation(max_samples),residuals(max_samples))
    allocate(published(max_samples,max_samples),frequency(max_samples,ncc))
    allocate(plot(max_samples,max_samples),omegas(max_samples), &
      rcr(max_orders,max_modes),fcr(max_orders,max_modes),root_slot(max_orders,max_modes))
    nan=ieee_value(0._wp,ieee_quiet_nan)
    published=cmplx(0._wp,0._wp,wp);frequency=nan;plot=0;omegas=0;rcr=0;fcr=0
    root_count=0;root_keys=0;root_slot=0
    step=(end_rpm-start_rpm)/real(requested-1,wp);next_step=step
    ! Topology changes are bracketed on a finite grid, not located to machine
    ! epsilon. Crossings retain their independent 1e-8 residual requirement.
    event_span=max(1.e-5_wp,step/16._wp)
    ns=0;target=start_rpm;attempt=0
    write(*,'(a,1x,i0,1x,i0)') 'RD_AUDIT_V1 CAMPBELL_POLICY',imt,requested
    write(*,'(a,1x,es24.16e3)') 'RD_AUDIT_V1 FAMILY_METRIC MASS_WHITENED_STATE',metric%omega_ref
    write(*,'(a)') 'RD_AUDIT_V1 NOTE FAMILY_SCHUR_STANDARD_NO_EXPLICIT_MASS_INVERSE'
    write(*,'(a)') 'RD_AUDIT_V1 NOTE FAMILY_QUALITY_SCOPE_INCOMING_ACCEPTED_PARTITION'
    write(*,'(a)') 'RD_AUDIT_V1 NOTE FAMILY_REPORT_PARTITION_FINAL_MONOTONIC_UNION'
    write(*,'(a)') 'RD_AUDIT_V1 NOTE ROOT_SUBSPACE_VALIDATION_TARGET_FAMILY_ONLY'
    write(*,'(a)') 'RD_AUDIT_V1 NOTE IMDTRK_CONTROLS_LEGACY_DISPLAY_ONLY_FAMILIES_ALWAYS_CONTINUED'
    write(*,'(a,2(1x,i0))') 'RD_AUDIT_V1 CAMPBELL_CAPACITY DISPLAY INTERNAL',max_display,max_samples
    call progress_stage('CAMPBELL','solve eigenproblem at each speed')
    do
      if(ns>0)then
        if(target<=samples(ns))then
          errmsg='HF03: speed stagnation before family eigensolve';return
        endif
      endif
      call solve_point(target,current,info);if(info/=0)return
      if(ns==0)then
        call spectral_seed(current,ncc,errmsg,info)
      else
        call spectral_continue(previous,current,errmsg,info)
      endif
      if(info/=0)then
        prev_rpm=target;if(ns>0)prev_rpm=samples(ns)
        write(*,'(a,2(1x,es24.16e3),2(1x,i0),1x,a)') &
          'RD_AUDIT_V1 TRACK_REJECT',prev_rpm,target,attempt,info,trim(errmsg)
        if(info/=-2.or.ns==0.or.attempt>=12)return
        call refine_target(samples(ns),target,refined_target,info)
        if(info/=0)then
          errmsg='HF03: no representable family refinement step; no forced association';return
        endif
        target=refined_target;next_step=target-samples(ns);attempt=attempt+1
        write(*,'(a,1x,es24.16e3,1x,i0)') 'RD_AUDIT_V1 CAMPBELL_REFINE',target,attempt
        cycle
      endif
      event=.false.
      if(ns>0)then
        event=topology_changed(previous,current)
        if(event.and.target-samples(ns)>event_span)then
          call refine_target(samples(ns),target,refined_target,info)
          if(info/=0)then
            errmsg='HF03: topology event cannot be bracketed';return
          endif
          target=refined_target;next_step=target-samples(ns)
          write(*,'(a,1x,es24.16e3,1x,i0)') 'RD_AUDIT_V1 CAMPBELL_REFINE',target,attempt
          cycle
        endif
      endif
      if(ns>=max_samples)then
        write(errmsg,'(a,i0,a)') 'HF03C: ',max_samples,' accepted sample capacity exceeded; no truncation';return
      endif
      if(event) write(*,'(a,2(1x,es24.16e3))') &
        'RD_AUDIT_V1 FAMILY_EVENT_BRACKET',samples(ns),target
      if(ns>0)then
        do j=1,size(current%parent)
          if(previous%parent(j)/=current%parent(j)) &
            write(*,'(a,1x,es24.16e3,2(1x,i0))') &
              'RD_AUDIT_V1 FAMILY_MERGE',target,j,current%parent(j)
        enddo
      endif
      ns=ns+1;samples(ns)=target;history(ns)=spectral_checkpoint_of(current)
      quality(ns)=current%min_cos2;separation(ns)=current%min_separation;residuals(ns)=current%max_residual
      call full_spectrum_report('CAMPBELL',current)
      write(*,'(a,2(1x,es24.16e3))') 'RD_AUDIT_V1 FAMILY_MIN_COS2',target,current%min_cos2
      call progress_update('CAMPBELL',merge(requested, &
        min(requested,1+int((target-start_rpm)/step)),target>=end_rpm),requested+2,real(target,lrk),'RPM')
      if(target>=end_rpm)exit
      previous=current;target=min(end_rpm,target+min(step,1.5_wp*next_step))
      next_step=target-samples(ns);attempt=0
    enddo
    parents=current%parent
    ! A permanent group merger is an explicit loss of individual genealogy,
    ! not loss of poles. Report all members on the SAME final family partition
    ! at every stored point. No large Schur matrix history is retained.
    do i=1,ns
      report_point%poles=history(i)%poles;report_point%owner=history(i)%owner
      report_point%parent=parents;report_point%nselected=ncc;report_point%rpm=samples(i)
      report_point%min_cos2=quality(i);report_point%min_separation=separation(i)
      report_point%max_residual=residuals(i)
      call spectral_report(report_point)
      omegas(i)=real(samples(i)*acos(-1._wp)/30._wp,lrk)
      published(i,1:ncc)=cmplx(nan,nan,wp)
      frequency(i,1:ncc)=nan
      ! A final spectral group may contain several of the initially selected
      ! families (zero-speed isotropic degeneracy is the canonical case).
      ! The legacy table cannot claim individual genealogy, but it CAN publish
      ! the ordered frequency multiset whenever the number of group frequency
      ! slots equals the number of selected display slots.  Keep NaN only when
      ! an auxiliary-pole union makes that legacy representation incomplete.
      do id=1,ncc
        if(spectral_owner(parents,id)/=id)cycle
        nmembers=0
        do k=1,ncc
          if(spectral_owner(parents,k)==id)nmembers=nmembers+1
        enddo
        if(nmembers==0)cycle
        call spectral_frequencies(history(i)%poles,history(i)%owner,parents,id,freq)
        if(size(freq)==nmembers)then
          call publish_group_slots(history(i)%poles,history(i)%owner,parents,id,freq, &
            published(i,1:ncc),frequency(i,1:ncc),info)
          if(info/=0)then
            errmsg='HF03B: representable spectral-group display publication failed';return
          endif
        else
          write(*,'(a,1x,es24.16e3,3(1x,i0))') &
            'RD_AUDIT_V1 LEGACY_GROUP_UNREPRESENTABLE',samples(i),id,nmembers,size(freq)
        endif
      enddo
      if(imt==0)call instantaneous_rank(history(i)%poles,published(i,1:ncc),frequency(i,:))
    enddo
    call progress_stage('CAMPBELL','refine physical crossings')
    ! Slots are sorted frequency multisets WITHIN a fixed spectral group.
    ! They are never advertised as unique eigenvector genealogies.
    do id=1,ncc
      if(spectral_owner(parents,id)/=id)cycle
      call spectral_frequencies(history(1)%poles,history(1)%owner,parents,id,freq)
      do slot=1,size(freq)
        do h=1,nit
          do i=1,ns-1
            call spectral_frequencies(history(i)%poles,history(i)%owner,parents,id,leftfreq)
            call spectral_frequencies(history(i+1)%poles,history(i+1)%owner,parents,id,rightfreq)
            if(size(leftfreq)/=size(rightfreq))then
              errmsg='HF03: group cardinality changed in crossing history';return
            endif
            h0=leftfreq(slot)-real(dmp(h),wp)*real(omegas(i),wp)
            h1=rightfreq(slot)-real(dmp(h),wp)*real(omegas(i+1),wp)
            if(root_match(h0,leftfreq(slot),samples(i),real(dmp(h),wp)))then
              call store_root(h,id,slot,samples(i),leftfreq(slot),0._wp, &
                abs(h0)/max(1._wp,leftfreq(slot)),'ENDPOINT',info)
              if(info/=0)return
            endif
            if(h0/=0._wp.and.h1/=0._wp.and.sign(1._wp,h0)/=sign(1._wp,h1))then
              call restore_point(i,refleft,info);if(info/=0)return
              call refine_root(samples(i),samples(i+1),h0,h1,id,slot,real(dmp(h),wp), &
                refleft,xroot,froot,width,resid,info)
              if(info/=0)return
              call store_root(h,id,slot,xroot,froot,width,resid,'BRACKETED',info);if(info/=0)return
            endif
          enddo
          h1=rightfreq(slot)-real(dmp(h),wp)*real(omegas(ns),wp)
          if(root_match(h1,rightfreq(slot),samples(ns),real(dmp(h),wp)))then
            call store_root(h,id,slot,samples(ns),rightfreq(slot),0._wp, &
              abs(h1)/max(1._wp,rightfreq(slot)),'ENDPOINT',info)
            if(info/=0)return
          endif
          do i=2,ns-1
            call spectral_frequencies(history(i-1)%poles,history(i-1)%owner,parents,id,prevfreq)
            call spectral_frequencies(history(i)%poles,history(i)%owner,parents,id,leftfreq)
            call spectral_frequencies(history(i+1)%poles,history(i+1)%owner,parents,id,nextfreq)
            if(min(prevfreq(slot),leftfreq(slot),nextfreq(slot))<=0._wp)cycle
            h0=prevfreq(slot)-real(dmp(h),wp)*real(omegas(i-1),wp)
            h1=leftfreq(slot)-real(dmp(h),wp)*real(omegas(i),wp)
            h2=nextfreq(slot)-real(dmp(h),wp)*real(omegas(i+1),wp)
            if(sign(1._wp,h0)/=sign(1._wp,h1).or.sign(1._wp,h1)/=sign(1._wp,h2))cycle
            if(abs(h1)>=min(abs(h0),abs(h2)))cycle
            call restore_point(i,refleft,info);if(info/=0)return
            call refine_minimum(samples(i-1),samples(i+1),id,slot,real(dmp(h),wp), &
              refleft,xroot,froot,width,resid,info)
            if(info<0)return
            if(info==0)then
              call store_root(h,id,slot,xroot,froot,width,resid,'NEAR_ZERO',info);if(info/=0)return
            endif
          enddo
        enddo
      enddo
    enddo
    call progress_update('CAMPBELL',requested+1,requested+2,real(end_rpm,lrk),'RPM')
    call progress_stage('CAMPBELL','prepare display interpolation')
    do h=1,nit
      do i=2,root_count(h)
        xroot=real(rcr(h,i),wp);froot=real(fcr(h,i),wp);j=i-1
        do while(j>=1)
          if(real(rcr(h,j),wp)<=xroot)exit
          rcr(h,j+1)=rcr(h,j);fcr(h,j+1)=fcr(h,j);j=j-1
        enddo
        rcr(h,j+1)=real(xroot,lrk);fcr(h,j+1)=real(froot,lrk)
      enddo
    enddo
    c=1
    do j=1,npi
      target=start_rpm+(end_rpm-start_rpm)*real(j-1,wp)/real(npi-1,wp)
      do while(c<ns-1)
        if(samples(c+1)>=target)exit
        c=c+1
      enddo
      w=(target-samples(c))/(samples(c+1)-samples(c))
      do k=1,ncc
        if(ieee_is_finite(frequency(c,k)).and.ieee_is_finite(frequency(c+1,k)))then
          plot(k,j)=real((1._wp-w)*frequency(c,k)+w*frequency(c+1,k),lrk)
        else
          plot(k,j)=real(nan,lrk)
        endif
      enddo
    enddo
    rpmi=real(start_rpm,lrk);rpmf=real(end_rpm,lrk)
    plot_step=real((end_rpm-start_rpm)*acos(-1._wp)/(30._wp*real(npi-1,wp)),lrk)
    call progress_stage('CAMPBELL','write Campbell results')
    call s_campbl(ns,ncc,npi,root_count,nit,plot,rcr,fcr,dmp,published,omegas, &
      rpmi,rpmf,spdn,nini,plot_step,max_samples,max_modes,max_orders,std,plt,errmsg,info)
    if(info/=0)return
    call progress_update('CAMPBELL',requested+2,requested+2,real(end_rpm,lrk),'RPM')
    write(*,'(a,1x,i0)') 'RD_AUDIT_V1 CROSSING_COVERAGE FINITE_ADAPTIVE_FAMILY_GRID',ns
    write(*,'(a)') 'RD_AUDIT_V1 NOTE FAMILY_FREQUENCY_RANK_IS_NOT_INDIVIDUAL_POLE_IDENTITY'
    ok=0
  contains
    subroutine publish_group_slots(poles,labels,parent,id,freq,aout,fout,istat)
      ! Publish a group as ordered frequency slots, never as invented
      ! eigenvector genealogy.  Selected member IDs provide only stable legacy
      ! column slots.  Aperiodic slots (frequency=0) deliberately keep AOUT NaN
      ! because a single complex pole/decrement representation is undefined.
      complex(wp),intent(in)::poles(:)
      integer,intent(in)::labels(:),parent(:),id
      real(wp),intent(in)::freq(:)
      complex(wp),intent(inout)::aout(:)
      real(wp),intent(inout)::fout(:)
      integer,intent(out)::istat
      integer,allocatable::members(:),positive(:)
      integer::j,k,nm,np,tmp,slot,pidx
      real(wp)::fscale
      istat=-1
      nm=count([(spectral_owner(parent,j)==id,j=1,min(ncc,size(aout)))])
      np=0
      do j=1,size(poles)
        if(spectral_owner(parent,labels(j))==id.and.aimag(poles(j))>0._wp)np=np+1
      enddo
      if(nm/=size(freq).or.nm<1) return
      allocate(members(nm),positive(np))
      k=0
      do j=1,min(ncc,size(aout))
        if(spectral_owner(parent,j)/=id)cycle
        k=k+1;members(k)=j
      enddo
      k=0
      do j=1,size(poles)
        if(spectral_owner(parent,labels(j))/=id.or.aimag(poles(j))<=0._wp)cycle
        k=k+1;positive(k)=j
      enddo
      do j=2,np
        tmp=positive(j);k=j-1
        do while(k>=1)
          if(aimag(poles(positive(k)))<=aimag(poles(tmp)))exit
          positive(k+1)=positive(k);k=k-1
        enddo
        positive(k+1)=tmp
      enddo
      pidx=0
      do slot=1,nm
        fout(members(slot))=freq(slot)
        if(freq(slot)<=0._wp)cycle
        pidx=pidx+1
        if(pidx>np) return
        fscale=max(1._wp,abs(freq(slot)),abs(aimag(poles(positive(pidx)))))
        if(abs(freq(slot)-aimag(poles(positive(pidx))))> &
           128._wp*epsilon(1._wp)*fscale) return
        aout(members(slot))=-conjg(poles(positive(pidx)))
      enddo
      if(pidx/=np) return
      istat=0
    end subroutine publish_group_slots

    subroutine instantaneous_rank(poles,a,f)
      ! IMDTRK=0 preserves the legacy instantaneous oscillatory-rank view.
      ! FAMILY records remain the complete continued spectral-group authority.
      complex(wp),intent(in)::poles(:)
      complex(wp),intent(out)::a(:)
      real(wp),intent(out)::f(:)
      integer::indices(size(poles)),n,i,j,tmp
      n=0;a=cmplx(nan,nan,wp);f=nan
      do i=1,size(poles)
        if(aimag(poles(i))>0._wp)then
          n=n+1;indices(n)=i
        endif
      enddo
      do i=2,n
        tmp=indices(i);j=i-1
        do while(j>=1)
          if(aimag(poles(indices(j)))<=aimag(poles(tmp)))exit
          indices(j+1)=indices(j);j=j-1
        enddo
        indices(j+1)=tmp
      enddo
      do i=1,min(n,size(a))
        a(i)=-conjg(poles(indices(i)));f(i)=aimag(poles(indices(i)))
      enddo
    end subroutine
    subroutine solve_point(rpm,point,istat)
      real(wp),intent(in)::rpm
      type(spectral_point),intent(out)::point
      integer,intent(out)::istat
      complex(wp),allocatable::kb(:,:),cb(:,:)
      real(lrk)::omega
      integer::fx
      allocate(kb(max_physical,max_physical),cb(max_physical,max_physical))
      omega=real(rpm*acos(-1._wp)/30._wp,lrk);fx=1;if(variable)fx=0
      call prpkc(omega,dm,pp,kb,cb,fx,errmsg,istat);if(istat/=0)return
      kb(1:dm,1:dm)=kb(1:dm,1:dm)+mk1(1:dm,1:dm)
      cb(1:dm,1:dm)=cb(1:dm,1:dm)+omega*mg(1:dm,1:dm)
      call spectral_solve(metric,kb(1:dm,1:dm),cb(1:dm,1:dm),rpm,point,errmsg,istat)
    end subroutine
    subroutine full_spectrum_report(context,point)
      character(len=*),intent(in)::context
      type(spectral_point),intent(in)::point
      complex(wp)::beta(size(point%poles))
      integer::idx(size(point%poles)),j
      beta=cmplx(1._wp,0._wp,wp);idx=[(j,j=1,size(point%poles))]
      call record_generalized_roots(-point%poles,beta,idx,size(idx))
      call spectrum_report(context,point%rpm,-point%poles)
    end subroutine
    logical function topology_changed(a,b) result(changed)
      type(spectral_point),intent(in)::a,b
      integer::g,j,ra,rb,na,nb
      changed=any(a%parent/=b%parent)
      do g=1,ncc
        if(spectral_owner(b%parent,g)/=g)cycle
        ra=0;rb=0
        do j=1,size(a%poles)
          if(spectral_owner(b%parent,a%owner(j))==g.and.aimag(a%poles(j))==0._wp)ra=ra+1
          if(spectral_owner(b%parent,b%owner(j))==g.and.aimag(b%poles(j))==0._wp)rb=rb+1
        enddo
        if(ra/=rb)changed=.true.
      enddo
    end function
    subroutine restore_point(index,point,istat)
      integer,intent(in)::index
      type(spectral_point),intent(out)::point
      integer,intent(out)::istat
      call solve_point(samples(index),point,istat);if(istat/=0)return
      call spectral_restore(point,history(index),parents,ncc,errmsg,istat)
    end subroutine
    subroutine evaluate(rpm,reference,group,rank,order,point,fvalue,freq_value,istat)
      real(wp),intent(in)::rpm,order
      type(spectral_point),intent(in)::reference
      integer,intent(in)::group,rank
      type(spectral_point),intent(out)::point
      real(wp),intent(out)::fvalue,freq_value
      integer,intent(out)::istat
      real(wp),allocatable::f(:)
      integer::j
      call solve_point(rpm,point,istat);if(istat/=0)return
      call spectral_continue(reference,point,errmsg,istat,only_group=group);if(istat/=0)return
      do j=1,ncc
        if(spectral_owner(parents,j)/=spectral_owner(point%parent,j))then
          errmsg='HF03: new group merger inside crossing bracket; refine sampling, no fabricated root'
          istat=-1;return
        endif
      enddo
      call spectral_frequencies(point%poles,point%owner,point%parent,group,f)
      if(rank<1.or.rank>size(f))then
        errmsg='HF03: crossing frequency rank outside group';istat=-1;return
      endif
      freq_value=f(rank);fvalue=freq_value-order*rpm*acos(-1._wp)/30._wp
      call full_spectrum_report('CAMPBELL_ROOT',point)
    end subroutine
    subroutine refine_root(left,right,fl0,fh0,group,rank,order,reference,x,f,width,residual,istat)
      real(wp),intent(in)::left,right,fl0,fh0,order
      integer,intent(in)::group,rank
      type(spectral_point),intent(in)::reference
      real(wp),intent(out)::x,f,width,residual
      integer,intent(out)::istat
      type(spectral_point)::anchor,trial
      real(wp)::lo,hi,fl,fm,tolerance
      integer::it
      lo=left;hi=right;fl=fl0;anchor=reference;istat=-1
      do it=1,80
        x=lo+0.5_wp*(hi-lo)
        if(x<=lo.or.x>=hi)then
          errmsg='HF03: crossing bisection reached representation limit';return
        endif
        call evaluate(x,anchor,group,rank,order,trial,fm,f,istat);if(istat/=0)return
        residual=abs(fm)/max(1._wp,f,order*x*acos(-1._wp)/30._wp)
        width=hi-lo;tolerance=max(1.e-6_wp,1.e-9_wp*max(abs(lo),abs(hi)))
        if(f>0._wp.and.residual<=root_relative_tol.and.width<=tolerance)then
          istat=0;return
        endif
        if(f>0._wp.and.fm==0._wp)then
          width=0;residual=0;istat=0;return
        endif
        if(sign(1._wp,fl)/=sign(1._wp,fm))then
          hi=x
        else
          lo=x;fl=fm;anchor=trial
        endif
      enddo
      errmsg='HF03: family crossing did not converge in 80 eigensolves';istat=-1
    end subroutine
    subroutine refine_minimum(left,right,group,rank,order,reference,x,f,width,residual,istat)
      real(wp),intent(in)::left,right,order
      integer,intent(in)::group,rank
      type(spectral_point),intent(in)::reference
      real(wp),intent(out)::x,f,width,residual
      integer,intent(out)::istat
      type(spectral_point)::point
      real(wp),parameter::golden=0.6180339887498948482_wp
      real(wp)::lo,hi,x1,x2,hv1,hv2,f1,f2
      integer::it
      lo=left;hi=right;x1=hi-golden*(hi-lo);x2=lo+golden*(hi-lo)
      call evaluate(x1,reference,group,rank,order,point,hv1,f1,istat);if(istat/=0)return
      call evaluate(x2,reference,group,rank,order,point,hv2,f2,istat);if(istat/=0)return
      do it=1,70
        if(hi-lo<=max(1.e-6_wp,1.e-9_wp*max(abs(lo),abs(hi))))exit
        if(abs(hv1)<abs(hv2))then
          hi=x2;x2=x1;hv2=hv1;f2=f1;x1=hi-golden*(hi-lo)
          call evaluate(x1,reference,group,rank,order,point,hv1,f1,istat)
        else
          lo=x1;x1=x2;hv1=hv2;f1=f2;x2=lo+golden*(hi-lo)
          call evaluate(x2,reference,group,rank,order,point,hv2,f2,istat)
        endif
        if(istat/=0)return
      enddo
      x=x1;f=f1;residual=abs(hv1)/max(1._wp,f,order*x*acos(-1._wp)/30._wp)
      if(abs(hv2)<abs(hv1))then
        x=x2;f=f2;residual=abs(hv2)/max(1._wp,f,order*x*acos(-1._wp)/30._wp)
      endif
      width=hi-lo;istat=1
      if(f>0._wp.and.residual<=root_relative_tol)istat=0
      write(*,'(a,3(1x,es24.16e3),1x,i0)') 'RD_AUDIT_V1 TANGENCY_SEARCH',x,width,residual,istat
    end subroutine
    subroutine store_root(order_index,group,rank,rpm,freq_value,bracket,residual,kind,istat)
      integer,intent(in)::order_index,group,rank
      real(wp),intent(in)::rpm,freq_value,bracket,residual
      character(len=*),intent(in)::kind
      integer,intent(out)::istat
      integer::t,pos,key
      real(wp)::oldrpm
      istat=0;key=64*group+rank
      do t=1,root_count(order_index)
        if(root_keys(order_index,t)/=key)cycle
        oldrpm=real(rcr(order_index,t),wp)*30._wp/acos(-1._wp)
        if(abs(oldrpm-rpm)<=max(1.e-6_wp,1.e-8_wp*abs(rpm)))return
      enddo
      pos=root_count(order_index)+1
      if(pos>max_modes)then
        errmsg='HF03: more than 19 crossings per order; no truncation';istat=-1;return
      endif
      root_count(order_index)=pos;root_keys(order_index,pos)=key;root_slot(order_index,pos)=rank
      rcr(order_index,pos)=real(rpm*acos(-1._wp)/30._wp,lrk);fcr(order_index,pos)=real(freq_value,lrk)
      write(*,'(a,2(1x,i0),1x,a,5(1x,es24.16e3))') 'RD_AUDIT_V1 CROSSING', &
        order_index,group,trim(kind),real(dmp(order_index),wp),rpm,freq_value,bracket,residual
      write(*,'(a,3(1x,i0),1x,a,5(1x,es24.16e3))') 'RD_AUDIT_V1 FAMILY_CROSSING', &
        order_index,group,rank,trim(kind),real(dmp(order_index),wp),rpm,freq_value,bracket,residual
    end subroutine
  end subroutine run_family_campbell
  pure subroutine refine_target(last,rejected,candidate,istat)
    real(wp),intent(in)::last,rejected
    real(wp),intent(out)::candidate
    integer,intent(out)::istat
    candidate=last;istat=-1
    if(.not.ieee_is_finite(last).or..not.ieee_is_finite(rejected))return
    if(last<0._wp.or.rejected<=last)return
    if(rejected-last<=64._wp*spacing(max(1._wp,abs(last),abs(rejected))))return
    candidate=last+0.5_wp*(rejected-last)
    if(candidate<=last.or.candidate>=rejected)return
    istat=0
  end subroutine
  pure logical function root_match(h,f,rpm,order)
    real(wp),intent(in)::h,f,rpm,order
    root_match=f>0._wp.and.abs(h)<=root_relative_tol*max(1._wp,f,order*rpm*acos(-1._wp)/30._wp)
  end function
end module rd_campbell_families
