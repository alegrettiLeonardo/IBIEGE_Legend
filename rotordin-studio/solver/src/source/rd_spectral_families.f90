! HF03: full-spectrum ownership and invariant-subspace continuation.
! Real second-order systems with positive-definite physical mass only.
! q=L**(-T) z, tau=omega_ref*t. No explicit mass inverse is formed.
! The physical poles are s (not the legacy stored a=-s).
module rd_spectral_families
  use rd_kinds, only: wp
  use rd_modal_tracking, only: optimal_assignment,tracking_min_mac
  use, intrinsic :: ieee_arithmetic, only: ieee_is_finite
  implicit none
  private
  real(wp), parameter :: repeated_tol=1.e-7_wp, residual_limit=1.e-10_wp
  integer, parameter :: max_selected_cluster=64
  type, public :: spectral_metric
    real(wp), allocatable :: lower(:,:)
    real(wp) :: mass_scale=1._wp,omega_ref=1._wp
    integer :: n=0
  end type
  type, public :: spectral_point
    real(wp), allocatable :: operator(:,:),schur(:,:),vectors(:,:)
    complex(wp), allocatable :: poles(:)
    integer, allocatable :: owner(:),parent(:)
    real(wp) :: rpm=0._wp,min_cos2=1._wp,max_residual=0._wp,min_separation=1._wp
    integer :: nselected=0
  end type
  type, public :: spectral_checkpoint
    complex(wp), allocatable :: poles(:)
    integer, allocatable :: owner(:)
    real(wp) :: rpm=0._wp
  end type
  public :: spectral_initialize_metric,spectral_solve,spectral_seed,spectral_continue
  public :: spectral_checkpoint_of,spectral_restore,spectral_basis,spectral_owner
  public :: spectral_frequencies,spectral_report,spectral_is_selected
  interface
    subroutine dpotrf(uplo,n,a,lda,info)
      import wp
      character :: uplo
      integer :: n,lda,info
      real(wp) :: a(lda,*)
    end subroutine
    subroutine dtrsm(side,uplo,trans,diag,m,n,alpha,a,lda,b,ldb)
      import wp
      character :: side,uplo,trans,diag
      integer :: m,n,lda,ldb
      real(wp) :: alpha,a(lda,*),b(ldb,*)
    end subroutine
    subroutine dgees(jobvs,sort,select,n,a,lda,sdim,wr,wi,vs,ldvs,work,lwork,bwork,info)
      import wp
      character :: jobvs,sort
      logical, external :: select
      integer :: n,lda,sdim,ldvs,lwork,info
      real(wp) :: a(lda,*),wr(*),wi(*),vs(ldvs,*),work(*)
      logical :: bwork(*)
    end subroutine
    subroutine dtrsen(job,compq,select,n,t,ldt,q,ldq,wr,wi,m,s,sep,work,lwork,iwork,liwork,info)
      import wp
      character :: job,compq
      logical :: select(*)
      integer :: n,ldt,ldq,m,lwork,iwork(*),liwork,info
      real(wp) :: t(ldt,*),q(ldq,*),wr(*),wi(*),s,sep,work(*)
    end subroutine
    subroutine dsyev(jobz,uplo,n,a,lda,w,work,lwork,info)
      import wp
      character :: jobz,uplo
      integer :: n,lda,lwork,info
      real(wp) :: a(lda,*),w(*),work(*)
    end subroutine
  end interface
contains
  logical function select_none(wr,wi)
    real(wp), intent(in) :: wr,wi
    select_none=.false.
  end function

  subroutine spectral_initialize_metric(mass,omega,metric,errmsg,ok)
    real(wp), intent(in) :: mass(:,:),omega
    type(spectral_metric), intent(out) :: metric
    character(len=*), intent(out) :: errmsg
    integer, intent(out) :: ok
    integer :: n,j
    ok=-1; errmsg=''; n=size(mass,1)
    if(n<1.or.size(mass,2)/=n) then
      errmsg='HF03: mass dimension mismatch'; return
    end if
    if(.not.all(ieee_is_finite(mass)).or..not.ieee_is_finite(omega)) then
      errmsg='HF03: nonfinite mass or reference frequency'; return
    end if
    if(omega<=0._wp) then
      errmsg='HF03: reference frequency must be positive'; return
    end if
    metric%mass_scale=maxval(abs(mass));metric%omega_ref=omega;metric%n=n
    if(metric%mass_scale<=tiny(1._wp)) then
      errmsg='HF03: mass scale is zero';return
    end if
    allocate(metric%lower(n,n));metric%lower=mass/metric%mass_scale
    if(maxval(abs(metric%lower-transpose(metric%lower)))>1.e-10_wp) then
      errmsg='HF03: mass is not symmetric';return
    end if
    call dpotrf('L',n,metric%lower,n,ok)
    if(ok/=0) then
      errmsg='HF03: mass not positive definite; no artificial mass added';ok=-1;return
    end if
    do j=1,n
      metric%lower(1:j-1,j)=0._wp
    end do
  end subroutine

  subroutine spectral_solve(metric,stiffness,damping,rpm,point,errmsg,ok)
    type(spectral_metric), intent(in) :: metric
    complex(wp), intent(in) :: stiffness(:,:),damping(:,:)
    real(wp), intent(in) :: rpm
    type(spectral_point), intent(out) :: point
    character(len=*), intent(out) :: errmsg
    integer, intent(out) :: ok
    real(wp), allocatable :: k(:,:),d(:,:),wr(:),wi(:),work(:)
    logical, allocatable :: bwork(:)
    real(wp) :: query(1)
    integer :: n,nn,j,sdim,lwork
    ok=-1;errmsg='';n=metric%n;nn=2*n
    if(n<1.or.any(shape(stiffness)/=[n,n]).or.any(shape(damping)/=[n,n])) then
      errmsg='HF03: second-order matrix dimensions inconsistent';return
    end if
    if(.not.all(ieee_is_finite(real(stiffness))).or..not.all(ieee_is_finite(aimag(stiffness))).or. &
       .not.all(ieee_is_finite(real(damping))).or..not.all(ieee_is_finite(aimag(damping)))) then
      errmsg='HF03: nonfinite stiffness or damping';return
    end if
    if(any(aimag(stiffness)/=0._wp).or.any(aimag(damping)/=0._wp)) then
      errmsg='HF03: complex physical matrix requires the preserved complex-pencil path';return
    end if
    if(.not.ieee_is_finite(rpm).or.rpm<0._wp) then
      errmsg='HF03: invalid prescribed rpm';return
    end if
    k=real(stiffness,wp)/metric%mass_scale;d=real(damping,wp)/metric%mass_scale
    call dtrsm('L','L','N','N',n,n,1._wp,metric%lower,n,k,n)
    call dtrsm('R','L','T','N',n,n,1._wp,metric%lower,n,k,n)
    call dtrsm('L','L','N','N',n,n,1._wp,metric%lower,n,d,n)
    call dtrsm('R','L','T','N',n,n,1._wp,metric%lower,n,d,n)
    allocate(point%operator(nn,nn),point%schur(nn,nn),point%vectors(nn,nn), &
      point%poles(nn),point%owner(nn),point%parent(nn),wr(nn),wi(nn),bwork(nn))
    point%operator=0._wp
    do j=1,n
      point%operator(j,n+j)=1._wp
    end do
    point%operator(n+1:nn,1:n)=-(k/metric%omega_ref)/metric%omega_ref
    point%operator(n+1:nn,n+1:nn)=-d/metric%omega_ref
    if(.not.all(ieee_is_finite(point%operator))) then
      errmsg='HF03: mass normalization overflow';return
    end if
    point%schur=point%operator
    call dgees('V','N',select_none,nn,point%schur,nn,sdim,wr,wi,point%vectors,nn,query,-1,bwork,ok)
    if(ok/=0) then
      errmsg='HF03: real Schur workspace query failed';return
    end if
    if(.not.ieee_is_finite(query(1)).or.query(1)>real(huge(lwork),wp)) then
      errmsg='HF03: invalid Schur workspace size';ok=-1;return
    end if
    lwork=max(3*nn,ceiling(query(1)));allocate(work(lwork));point%schur=point%operator
    call dgees('V','N',select_none,nn,point%schur,nn,sdim,wr,wi,point%vectors,nn,work,lwork,bwork,ok)
    if(ok/=0) then
      errmsg='HF03: real Schur decomposition failed';return
    end if
    point%poles=cmplx(wr,wi,wp)*metric%omega_ref
    if(.not.all(ieee_is_finite(real(point%poles))).or..not.all(ieee_is_finite(aimag(point%poles)))) then
      errmsg='HF03: nonfinite physical pole';ok=-1;return
    end if
    point%rpm=rpm;point%owner=0
    point%parent=[(j,j=1,nn)]
  end subroutine

  integer function spectral_owner(parent,id) result(root)
    integer, intent(in) :: parent(:),id
    integer :: step
    root=id
    do step=1,size(parent)
      if(root<1.or.root>size(parent)) then
        root=0;return
      end if
      if(parent(root)==root) return
      root=parent(root)
    end do
    root=0
  end function

  subroutine join_groups(parent,a,b)
    integer, intent(inout) :: parent(:)
    integer, intent(in) :: a,b
    integer :: x,y,j
    x=spectral_owner(parent,a);y=spectral_owner(parent,b)
    if(x==0.or.y==0.or.x==y) return
    parent(max(x,y))=min(x,y)
    do j=1,size(parent)
      parent(j)=spectral_owner(parent,j)
    end do
  end subroutine

  logical function spectral_is_selected(point,id)
    type(spectral_point), intent(in) :: point
    integer, intent(in) :: id
    integer :: j
    spectral_is_selected=.false.
    do j=1,point%nselected
      if(spectral_owner(point%parent,j)==id) spectral_is_selected=.true.
    end do
  end function

  subroutine close_blocks(point,labels,parent)
    type(spectral_point), intent(in) :: point
    integer, intent(in) :: labels(:)
    integer, intent(inout) :: parent(:)
    integer :: j,k,n
    real(wp) :: scale
    n=size(labels);j=1
    do while(j<=n)
      if(j<n) then
        if(point%schur(j+1,j)/=0._wp) then
          call join_groups(parent,labels(j),labels(j+1));j=j+2;cycle
        end if
      end if
      j=j+1
    end do
    ! Numerically inseparable poles from different historical groups are not
    ! forced to retain an individual identity. Their UNION remains observable.
    do j=1,n
      do k=j+1,n
        scale=max(1._wp,abs(point%poles(j)),abs(point%poles(k)))
        if(abs(point%poles(j)/scale-point%poles(k)/scale)<=repeated_tol) &
          call join_groups(parent,labels(j),labels(k))
      end do
    end do
  end subroutine

  subroutine spectral_seed(point,nselected,errmsg,ok)
    type(spectral_point), intent(inout) :: point
    integer, intent(in) :: nselected
    character(len=*), intent(out) :: errmsg
    integer, intent(out) :: ok
    integer, allocatable :: pairs(:)
    integer :: n,j,k,m,id,tmp,info
    real(wp), allocatable :: basis(:,:)
    logical, allocatable :: mask(:)
    real(wp) :: sep,res
    ok=-1;errmsg='';n=size(point%poles);allocate(pairs(n));m=0
    j=1
    do while(j<n)
      if(point%schur(j+1,j)/=0._wp) then
        m=m+1;pairs(m)=j;j=j+2
      else
        j=j+1
      end if
    end do
    if(nselected<1.or.nselected>m) then
      errmsg='HF03: requested initial oscillatory families unavailable';return
    end if
    do j=2,m
      tmp=pairs(j);k=j-1
      do while(k>=1)
        if(abs(aimag(point%poles(pairs(k))))<=abs(aimag(point%poles(tmp)))) exit
        pairs(k+1)=pairs(k);k=k-1
      end do
      pairs(k+1)=tmp
    end do
    point%nselected=nselected;point%owner=0
    do j=1,nselected
      point%owner(pairs(j):pairs(j)+1)=j
    end do
    id=nselected;j=1
    do while(j<=n)
      if(point%owner(j)>0) then
        j=j+1;cycle
      end if
      id=id+1;point%owner(j)=id
      if(j<n) then
        if(point%schur(j+1,j)/=0._wp) then
          point%owner(j+1)=id;j=j+2;cycle
        end if
      end if
      j=j+1
    end do
    call close_blocks(point,point%owner,point%parent)
    do j=1,n
      point%owner(j)=spectral_owner(point%parent,point%owner(j))
    end do
    allocate(mask(n))
    point%min_cos2=1._wp;point%min_separation=1._wp;point%max_residual=0._wp
    do id=1,n
      if(.not.spectral_is_selected(point,id)) cycle
      mask=point%owner==id
      if(count(mask)>max_selected_cluster) then
        errmsg='HF03: initial selected cluster exceeds 64 poles';return
      end if
      call spectral_basis(point,mask,basis,sep,res,errmsg,info)
      if(info/=0) then
        ok=info;return
      end if
      point%min_separation=min(point%min_separation,sep)
      point%max_residual=max(point%max_residual,res)
    end do
    ok=0
  end subroutine

  subroutine pole_assignment(old,new,assignment,errmsg,ok)
    complex(wp), intent(in) :: old(:),new(:)
    integer, intent(out) :: assignment(:),ok
    character(len=*), intent(out) :: errmsg
    real(wp), allocatable :: score(:,:)
    real(wp) :: scale,dist
    integer :: n,i,j
    errmsg='';n=size(old);ok=-1
    if(size(new)/=n.or.size(assignment)/=n) then
      errmsg='HF03: spectrum dimension changed';return
    end if
    allocate(score(n,n))
    do j=1,n
      do i=1,n
        scale=max(1._wp,abs(old(i)),abs(new(j)))
        dist=abs(old(i)/scale-new(j)/scale)
        score(i,j)=1._wp-dist
      end do
    end do
    call optimal_assignment(score,assignment,ok)
    if(ok/=0) errmsg='HF03: complete-spectrum assignment failed'
  end subroutine

  subroutine spectral_basis(point,mask,basis,separation,residual,errmsg,ok)
    type(spectral_point), intent(in) :: point
    logical, intent(in) :: mask(:)
    real(wp), allocatable, intent(out) :: basis(:,:)
    real(wp), intent(out) :: separation,residual
    character(len=*), intent(out) :: errmsg
    integer, intent(out) :: ok
    real(wp), allocatable :: t(:,:),q(:,:),wr(:),wi(:),work(:),r(:,:)
    integer, allocatable :: iw(:)
    integer :: n,m,k,lw,liw,j
    real(wp) :: cond,sep,norm_a
    n=size(point%poles);m=count(mask);ok=-1;errmsg='';separation=0;residual=huge(1._wp)
    if(m<1.or.size(mask)/=n) then
      errmsg='HF03: invalid invariant-subspace selection';return
    end if
    do j=1,n-1
      if(point%schur(j+1,j)/=0._wp) then
        if(mask(j).neqv.mask(j+1)) then
          errmsg='HF03: selection splits a real Schur 2x2 block';return
        end if
      end if
    end do
    t=point%schur;q=point%vectors;allocate(wr(n),wi(n))
    lw=max(n,2*m*(n-m),1);liw=max(1,m*(n-m));allocate(work(lw),iw(liw))
    call dtrsen('B','V',mask,n,t,n,q,n,wr,wi,k,cond,sep,work,lw,iw,liw,ok)
    if(ok/=0.or.k/=m) then
      errmsg='HF03: Schur group cannot be isolated';ok=-2;return
    end if
    basis=q(:,1:m)
    r=matmul(point%operator,basis)-matmul(basis,t(1:m,1:m))
    norm_a=max(tiny(1._wp),maxval(sum(abs(point%operator),dim=1)))
    residual=maxval(sum(abs(r),dim=1))/(norm_a*max(1._wp,maxval(sum(abs(basis),dim=1))))
    separation=sep/norm_a
    if(.not.ieee_is_finite(residual).or..not.ieee_is_finite(separation)) then
      errmsg='HF03: nonfinite Schur residual/separation';ok=-1;return
    end if
    if(residual>residual_limit) then
      errmsg='HF03: invariant-subspace residual exceeds 1e-10';ok=-1;return
    end if
    ! Not the individual-eigenvector floor. A poorly separated GROUP must not
    ! be advertised as uniquely isolated from the remaining spectrum.
    if(m<n.and.separation<=64._wp*epsilon(1._wp)) then
      errmsg='HF03: invariant group is not separated from remaining spectrum';ok=-2;return
    end if
    ok=0
  end subroutine

  subroutine overlap(left,right,score,ok)
    real(wp), intent(in) :: left(:,:),right(:,:)
    real(wp), intent(out) :: score
    integer, intent(out) :: ok
    real(wp), allocatable :: cross(:,:),gram(:,:),eigenvalues(:),work(:)
    integer :: m
    ok=-1;score=0._wp;m=size(left,2)
    if(m<1.or.any(shape(left)/=shape(right))) return
    cross=matmul(transpose(left),right)
    if(.not.all(ieee_is_finite(cross))) return
    ! lambda_min(X**T X) = cos(theta_max)**2. Only an absolute accuracy
    ! decision near the 0.90 acceptance boundary is needed, not relative
    ! accuracy for tiny singular values. A symmetric eigensolve also avoids
    ! dqds divisions in some DGESVD builds under strict IEEE debug traps.
    ! No floating-point trap is disabled and no acceptance floor is reduced.
    gram=matmul(transpose(cross),cross)
    allocate(eigenvalues(m),work(max(1,3*m)))
    call dsyev('N','U',m,gram,m,eigenvalues,work,size(work),ok)
    if(ok/=0) return
    if(.not.all(ieee_is_finite(eigenvalues))) then
      ok=-1;return
    end if
    score=min(1._wp,max(0._wp,minval(eigenvalues)))
  end subroutine

  subroutine spectral_continue(old,new,errmsg,ok,only_group)
    type(spectral_point), intent(in) :: old
    type(spectral_point), intent(inout) :: new
    character(len=*), intent(out) :: errmsg
    integer, intent(out) :: ok
    ! Optional target is used ONLY by crossing evaluations, not accepted
    ! Campbell grid points. All poles remain computed and owned; only this
    ! family's subspace is qualified at those extra root evaluations.
    integer, intent(in), optional :: only_group
    integer, allocatable :: assignment(:),old_labels(:)
    real(wp), allocatable :: qb(:,:),qa(:,:)
    logical, allocatable :: mask_old(:),mask_new(:)
    real(wp) :: separation,residual,score
    integer :: n,j,id,info,dim_group
    ok=-1;errmsg='';n=size(old%poles)
    if(size(new%poles)/=n) then
      errmsg='HF03: state dimension changed along continuation';return
    end if
    allocate(assignment(n),old_labels(n),mask_old(n),mask_new(n))
    call pole_assignment(old%poles,new%poles,assignment,errmsg,ok);if(ok/=0) return
    new%parent=old%parent;new%nselected=old%nselected
    do j=1,n
      new%owner(assignment(j))=old%owner(j)
    end do
    call close_blocks(new,new%owner,new%parent)
    call close_blocks(old,old%owner,new%parent)
    do j=1,n
      old_labels(j)=spectral_owner(new%parent,old%owner(j))
      new%owner(j)=spectral_owner(new%parent,new%owner(j))
    end do
    new%min_cos2=1._wp;new%max_residual=0._wp;new%min_separation=1._wp
    do id=1,n
      if(.not.spectral_is_selected(new,id)) cycle
      if(present(only_group)) then
        if(id/=spectral_owner(new%parent,only_group)) cycle
      end if
      mask_old=old_labels==id;mask_new=new%owner==id;dim_group=count(mask_new)
      if(count(mask_old)/=dim_group.or.dim_group<1) then
        errmsg='HF03: spectral-family algebraic dimension was not conserved';ok=-1;return
      end if
      if(dim_group>max_selected_cluster) then
        errmsg='HF03: group exceeds 64 poles; identity not resolved, no truncation';ok=-1;return
      end if
      call spectral_basis(old,mask_old,qa,separation,residual,errmsg,info)
      if(info/=0) then
        ok=info;return
      end if
      new%min_separation=min(new%min_separation,separation)
      new%max_residual=max(new%max_residual,residual)
      call spectral_basis(new,mask_new,qb,separation,residual,errmsg,info)
      if(info/=0) then
        ok=info;return
      end if
      new%min_separation=min(new%min_separation,separation)
      new%max_residual=max(new%max_residual,residual)
      call overlap(qa,qb,score,info)
      if(info/=0) then
        errmsg='HF03: principal-angle symmetric eigensolve failed';ok=-1;return
      end if
      new%min_cos2=min(new%min_cos2,score)
      if(score<tracking_min_mac) then
        write(errmsg,'(a,i0,a,es12.4)') 'HF03: family ',id,' minimum subspace cosine squared ',score
        ok=-2;return
      end if
    end do
    ok=0
  end subroutine

  function spectral_checkpoint_of(point) result(checkpoint)
    type(spectral_point), intent(in) :: point
    type(spectral_checkpoint) :: checkpoint
    checkpoint%poles=point%poles;checkpoint%owner=point%owner;checkpoint%rpm=point%rpm
  end function

  subroutine spectral_restore(point,checkpoint,parent,nselected,errmsg,ok)
    type(spectral_point), intent(inout) :: point
    type(spectral_checkpoint), intent(in) :: checkpoint
    integer, intent(in) :: parent(:),nselected
    character(len=*), intent(out) :: errmsg
    integer, intent(out) :: ok
    integer, allocatable :: assignment(:)
    integer :: j,n
    real(wp) :: relative
    n=size(point%poles);allocate(assignment(n));point%parent=parent;point%nselected=nselected
    call pole_assignment(checkpoint%poles,point%poles,assignment,errmsg,ok);if(ok/=0) return
    do j=1,n
      relative=abs(checkpoint%poles(j)-point%poles(assignment(j)))/max(1._wp,abs(checkpoint%poles(j)))
      if(relative>1.e-6_wp) then
        errmsg='HF03: regenerated checkpoint spectrum changed';ok=-1;return
      end if
      point%owner(assignment(j))=spectral_owner(parent,checkpoint%owner(j))
    end do
    call close_blocks(point,point%owner,point%parent)
    do j=1,n
      point%owner(j)=spectral_owner(point%parent,point%owner(j))
    end do
  end subroutine

  subroutine spectral_frequencies(poles,labels,parent,id,frequencies)
    ! Sorted nonnegative oscillation frequencies are an ORDERED MULTISET of
    ! a group, not unique pole genealogies. Zeros retain aperiodic slots.
    complex(wp), intent(in) :: poles(:)
    integer, intent(in) :: labels(:),parent(:),id
    real(wp), allocatable, intent(out) :: frequencies(:)
    integer :: i,j,k,d,root
    real(wp) :: value
    d=0;root=spectral_owner(parent,id)
    do i=1,size(poles)
      if(spectral_owner(parent,labels(i))==root) d=d+1
    end do
    allocate(frequencies(d/2));frequencies=0._wp;k=0
    do i=1,size(poles)
      if(spectral_owner(parent,labels(i))/=root) cycle
      if(aimag(poles(i))>0._wp) then
        k=k+1
        if(k<=size(frequencies)) frequencies(k)=aimag(poles(i))
      end if
    end do
    do i=2,size(frequencies)
      value=frequencies(i);j=i-1
      do while(j>=1)
        if(frequencies(j)<=value) exit
        frequencies(j+1)=frequencies(j);j=j-1
      end do
      frequencies(j+1)=value
    end do
  end subroutine

  subroutine spectral_report(point,parent)
    type(spectral_point), intent(in) :: point
    integer, intent(in), optional :: parent(:)
    integer, allocatable :: roots(:)
    real(wp), allocatable :: freq(:)
    integer :: id,j,m,real_count,osc_count,k,l
    real(wp) :: scale
    logical :: coalescent
    character(len=24) :: regime
    roots=point%parent;if(present(parent)) roots=parent
    do id=1,size(point%poles)
      if(spectral_owner(roots,id)/=id) cycle
      k=0
      do j=1,point%nselected
        if(spectral_owner(roots,j)==id) k=k+1
      end do
      if(k==0) cycle
      m=0;real_count=0;osc_count=0
      do j=1,size(point%poles)
        if(spectral_owner(roots,point%owner(j))/=id) cycle
        m=m+1
        if(aimag(point%poles(j))==0._wp) real_count=real_count+1
        if(aimag(point%poles(j))>0._wp) osc_count=osc_count+1
      end do
      regime='OSCILLATORY'
      if(real_count==m) regime='APERIODIC'
      if(real_count>0.and.osc_count>0) regime='MIXED_SPECTRAL'
      coalescent=.false.
      do j=1,size(point%poles)
        if(spectral_owner(roots,point%owner(j))/=id) cycle
        do l=j+1,size(point%poles)
          if(spectral_owner(roots,point%owner(l))/=id) cycle
          scale=max(1._wp,abs(point%poles(j)),abs(point%poles(l)))
          if(abs(point%poles(j)/scale-point%poles(l)/scale)<=repeated_tol) coalescent=.true.
        end do
      end do
      if(coalescent) regime='COALESCENT_UNRESOLVED'
      write(*,'(a,1x,es24.16e3,3(1x,i0),1x,a,3(1x,es24.16e3))') &
        'RD_AUDIT_V1 FAMILY',point%rpm,id,m,k,trim(regime),point%min_cos2, &
        point%min_separation,point%max_residual
      do j=1,point%nselected
        if(spectral_owner(roots,j)==id) write(*,'(a,1x,es24.16e3,2(1x,i0))') &
          'RD_AUDIT_V1 FAMILY_MEMBER',point%rpm,id,j
      end do
      k=0
      do j=1,size(point%poles)
        if(spectral_owner(roots,point%owner(j))/=id) cycle
        k=k+1
        write(*,'(a,1x,es24.16e3,2(1x,i0),2(1x,es24.16e3))') &
          'RD_AUDIT_V1 FAMILY_POLE',point%rpm,id,k,real(point%poles(j),wp),aimag(point%poles(j))
      end do
      call spectral_frequencies(point%poles,point%owner,roots,id,freq)
      do k=1,size(freq)
        write(*,'(a,1x,es24.16e3,2(1x,i0),1x,es24.16e3)') &
          'RD_AUDIT_V1 FAMILY_FREQUENCY',point%rpm,id,k,freq(k)
      end do
    end do
  end subroutine
end module rd_spectral_families
