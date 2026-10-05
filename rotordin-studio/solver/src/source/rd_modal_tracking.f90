! F05: physical-coordinate, mass-weighted MAC and rectangular global assignment.
! Close/repeated roots are compared as subspaces, not as unique eigenvectors.
module rd_modal_tracking
  use rd_kinds, only: wp
  use rd_modal_metrics, only: finite_complex
  use, intrinsic :: ieee_arithmetic, only: ieee_is_finite
  implicit none
  private
  real(wp), parameter, public :: tracking_min_mac=0.90_wp
  public :: mass_factor, track_modes, optimal_assignment, same_cluster
  interface
    subroutine dpotrf(uplo,n,a,lda,info)
      import wp
      character(len=1), intent(in) :: uplo
      integer, intent(in) :: n,lda
      real(wp), intent(inout) :: a(lda,*)
      integer, intent(out) :: info
    end subroutine
    subroutine zgesvd(jobu,jobvt,m,n,a,lda,s,u,ldu,vt,ldvt,work,lwork,rwork,info)
      import wp
      character(len=1), intent(in) :: jobu,jobvt
      integer, intent(in) :: m,n,lda,ldu,ldvt,lwork
      complex(wp), intent(inout) :: a(lda,*)
      real(wp), intent(out) :: s(*),rwork(*)
      complex(wp), intent(out) :: u(ldu,*),vt(ldvt,*),work(*)
      integer, intent(out) :: info
    end subroutine
  end interface
contains
  subroutine mass_factor(mass,l,errmsg,ok)
    real(wp), intent(in) :: mass(:,:)
    real(wp), allocatable, intent(out) :: l(:,:)
    character(len=*), intent(out) :: errmsg
    integer, intent(out) :: ok
    real(wp) :: scale
    integer :: n,i,j,info
    ok=-1; errmsg=''; n=size(mass,1)
    if(n<1.or.size(mass,2)/=n) then
      errmsg='tracking: physical mass dimension mismatch'; return
    end if
    if(.not.all(ieee_is_finite(mass))) then
      errmsg='tracking: physical mass is nonfinite'; return
    end if
    scale=maxval(abs(mass))
    if(scale<=tiny(1._wp)) then
      errmsg='tracking: physical mass has no positive scale'; return
    end if
    if(maxval(abs(mass/scale-transpose(mass)/scale))>1.e-10_wp) then
      errmsg='tracking: physical mass is not symmetric'; return
    end if
    allocate(l(n,n)); l=mass/scale
    call dpotrf('L',n,l,n,info)
    if(info/=0) then
      errmsg='tracking: mass is not positive definite; no artificial mass added'; return
    end if
    do j=1,n
      do i=1,j-1
        l(i,j)=0._wp
      end do
    end do
    ok=0
  end subroutine

  pure logical function same_cluster(a,b)
    complex(wp), intent(in) :: a,b
    real(wp) :: scale
    scale=max(1._wp,abs(a),abs(b))
    same_cluster=abs(a/scale-b/scale)<=1.e-7_wp
  end function

  subroutine optimal_assignment(score,assignment,ok)
    ! Rectangular Hungarian shortest-augmenting-path algorithm, O(rows^2*cols).
    ! Dummies are not returned: every requested row must have a distinct column.
    real(wp), intent(in) :: score(:,:)
    integer, intent(out) :: assignment(:),ok
    real(wp), allocatable :: u(:),v(:),minv(:)
    integer, allocatable :: p(:),way(:)
    logical, allocatable :: used(:)
    integer :: nr,nc,i,j,j0,j1,i0
    real(wp) :: cur,delta
    ok=-1; assignment=0; nr=size(score,1); nc=size(score,2)
    if(nr<1.or.nc<nr.or.size(assignment)/=nr) return
    if(.not.all(ieee_is_finite(score))) return
    allocate(u(0:nr),v(0:nc),minv(nc),p(0:nc),way(nc),used(0:nc))
    u=0._wp; v=0._wp; p=0; way=0
    do i=1,nr
      p(0)=i; j0=0; minv=huge(1._wp); used=.false.
      do
        used(j0)=.true.; i0=p(j0); delta=huge(1._wp); j1=0
        do j=1,nc
          if(used(j)) cycle
          cur=(1._wp-score(i0,j))-u(i0)-v(j)
          if(cur<minv(j)) then
            minv(j)=cur; way(j)=j0
          end if
          if(minv(j)<delta) then
            delta=minv(j); j1=j
          end if
        end do
        if(j1==0.or..not.ieee_is_finite(delta)) return
        do j=0,nc
          if(used(j)) then
            u(p(j))=u(p(j))+delta; v(j)=v(j)-delta
          else if(j>0) then
            minv(j)=minv(j)-delta
          end if
        end do
        j0=j1
        if(p(j0)==0) exit
      end do
      do
        j1=way(j0); p(j0)=p(j1); j0=j1
        if(j0==0) exit
      end do
    end do
    do j=1,nc
      if(p(j)>0) assignment(p(j))=j
    end do
    if(all(assignment>0)) ok=0
  end subroutine

  subroutine orthonormalize(a,q,ok)
    complex(wp), intent(in) :: a(:,:)
    complex(wp), allocatable, intent(out) :: q(:,:)
    integer, intent(out) :: ok
    integer :: j,k,pass
    real(wp) :: normv
    allocate(q(size(a,1),size(a,2))); q=a; ok=-1
    do j=1,size(q,2)
      do pass=1,2
        do k=1,j-1
          q(:,j)=q(:,j)-dot_product(q(:,k),q(:,j))*q(:,k)
        end do
      end do
      normv=sqrt(real(dot_product(q(:,j),q(:,j)),wp))
      if(normv<1.e-12_wp.or..not.ieee_is_finite(normv)) return
      q(:,j)=q(:,j)/normv
    end do
    ok=0
  end subroutine

  subroutine subspace_quality(a,b,quality,ok)
    complex(wp), intent(in) :: a(:,:),b(:,:)
    real(wp), intent(out) :: quality
    integer, intent(out) :: ok
    complex(wp), allocatable :: qa(:,:),qb(:,:),overlap(:,:),work(:)
    complex(wp) :: dummy_u(1,1),dummy_v(1,1)
    real(wp), allocatable :: singular(:),rwork(:)
    integer :: na,nb,info,lwork
    quality=0._wp; ok=-1; na=size(a,2); nb=size(b,2)
    if(na<1.or.na>nb) return
    call orthonormalize(a,qa,info); if(info/=0) return
    call orthonormalize(b,qb,info); if(info/=0) return
    allocate(overlap(na,nb),singular(na),rwork(5*na))
    overlap=matmul(conjg(transpose(qa)),qb)
    lwork=max(1,2*na+nb); allocate(work(lwork))
    call zgesvd('N','N',na,nb,overlap,na,singular,dummy_u,1,dummy_v,1,work,lwork,rwork,info)
    if(info/=0) return
    quality=min(1._wp,max(0._wp,minval(singular)**2)); ok=0
  end subroutine

  subroutine track_modes(l,fi,avl,valid,previous,first,sel,qmac,cluster,errmsg,ok)
    real(wp), intent(in) :: l(:,:)
    complex(wp), intent(in) :: fi(:,:),avl(:),previous(:,:)
    logical, intent(in) :: valid(:),first
    integer, intent(out) :: sel(:),cluster(:),ok
    real(wp), intent(out) :: qmac(:)
    character(len=*), intent(out) :: errmsg
    integer, allocatable :: candidates(:),group(:),assignment(:),members(:),rows(:)
    complex(wp), allocatable :: w(:,:),old(:,:),basis(:,:)
    real(wp), allocatable :: score(:,:),rawscore(:,:)
    integer :: np,nm,nc,i,j,k,g,ng,info,nn,nr,t
    real(wp) :: scale,normv,quality,alt
    ok=-1; errmsg=''; sel=0; cluster=0; qmac=0._wp
    np=size(l,1); nm=size(sel); nc=0
    if(nm<1.or.size(l,2)/=np.or.size(fi,1)<np.or.size(previous,1)<np.or. &
       size(previous,2)/=nm.or.size(qmac)/=nm.or.size(cluster)/=nm) then
      errmsg='tracking: inconsistent physical coordinates'; return
    end if
    allocate(candidates(size(avl)),group(size(avl)))
    do j=1,min(size(avl),size(fi,2),size(valid))
      if(.not.valid(j).or..not.finite_complex(avl(j))) cycle
      if(aimag(avl(j))<=max(1.e-12_wp,64._wp*epsilon(1._wp)*abs(avl(j)))) cycle
      if(.not.all(finite_complex(fi(1:np,j)))) cycle
      nc=nc+1; candidates(nc)=j
    end do
    if(nc<nm) then
      errmsg='tracking: insufficient valid oscillatory physical modes'; return
    end if
    ! Deterministic seed by damped frequency (never by pole magnitude).
    do i=2,nc
      t=candidates(i); j=i-1
      do while(j>=1)
        if(aimag(avl(candidates(j)))<=aimag(avl(t))) exit
        candidates(j+1)=candidates(j); j=j-1
      end do
      candidates(j+1)=t
    end do
    allocate(w(np,nc),old(np,nm),score(nm,nc),rawscore(nm,nc),assignment(nm))
    do j=1,nc
      scale=maxval(abs(fi(1:np,candidates(j))))
      if(scale<=tiny(1._wp)) then
        errmsg='tracking: eigenvector has no physical displacement'; return
      end if
      w(:,j)=matmul(transpose(l),fi(1:np,candidates(j))/scale)
      normv=sqrt(real(dot_product(w(:,j),w(:,j)),wp))
      if(normv<=tiny(1._wp)) then
        errmsg='tracking: zero modal mass'; return
      end if
      w(:,j)=w(:,j)/normv
    end do
    group=0; ng=0
    do j=1,nc
      if(group(j)/=0) cycle
      ng=ng+1; group(j)=ng
      do k=j+1,nc
        if(group(k)==0.and.same_cluster(avl(candidates(j)),avl(candidates(k)))) group(k)=ng
      end do
    end do
    if(first) then
      sel=candidates(1:nm); cluster=group(1:nm); qmac=1._wp; ok=0; return
    end if
    do i=1,nm
      scale=maxval(abs(previous(1:np,i)))
      if(scale<=tiny(1._wp)) then
        errmsg='tracking: zero continuation reference'; return
      end if
      old(:,i)=matmul(transpose(l),previous(1:np,i)/scale)
      normv=sqrt(real(dot_product(old(:,i),old(:,i)),wp))
      if(normv<=tiny(1._wp).or..not.ieee_is_finite(normv)) then
        errmsg='tracking: invalid reference modal mass'; return
      end if
      old(:,i)=old(:,i)/normv
      do j=1,nc
        rawscore(i,j)=min(1._wp,abs(dot_product(old(:,i),w(:,j)))**2)
      end do
    end do
    score=rawscore
    do g=1,ng
      members=pack([(j,j=1,nc)],group(1:nc)==g); nn=size(members)
      if(nn<2) cycle
      call orthonormalize(w(:,members),basis,info)
      if(info/=0) then
        errmsg='tracking: repeated-root subspace is rank deficient'; return
      end if
      do i=1,nm
        quality=min(1._wp,sum(abs(matmul(conjg(transpose(basis)),old(:,i)))**2))
        score(i,members)=max(score(i,members),quality)
      end do
      deallocate(basis)
    end do
    call optimal_assignment(score,assignment,info)
    if(info/=0) then
      errmsg='tracking: global assignment failed'; return
    end if
    do i=1,nm
      j=assignment(i); sel(i)=candidates(j); qmac(i)=score(i,j); cluster(i)=group(j)
      if(qmac(i)<tracking_min_mac) then
        write(errmsg,'(a,i0,a,f8.5)') 'tracking: reduce continuation step, mode ',i,' MAC=',qmac(i)
        ok=-2; return
      end if
      alt=-1._wp
      do k=1,nc
        if(group(k)/=group(j)) alt=max(alt,score(i,k))
      end do
      if(abs(alt-qmac(i))<=1.e-6_wp) then
        errmsg='tracking: ambiguous distinct-root identity; refine step'; ok=-2; return
      end if
    end do
    do g=1,ng
      members=pack([(j,j=1,nc)],group(1:nc)==g)
      if(size(members)<2) cycle
      rows=pack([(i,i=1,nm)],cluster==g); nr=size(rows)
      if(nr==0) cycle
      call subspace_quality(old(:,rows),w(:,members),quality,info)
      if(info/=0.or.quality<tracking_min_mac) then
        errmsg='tracking: subspace continuity below threshold; refine step'; ok=-2; return
      end if
      qmac(rows)=quality
      ! If only part of a multiple root is requested, identity is explicitly
      ! subspace-only; no arbitrary rotated vector is called a unique branch.
      write(*,'(a,3(1x,i0),1x,es24.16e3)') 'RD_AUDIT_V1 TRACK_SUBSPACE', &
        g,nr,size(members),quality
    end do
    ok=0
  end subroutine
end module rd_modal_tracking
