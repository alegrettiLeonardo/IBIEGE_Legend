!     $Id$
!     ==================================================================
!
!>    @file modos.f
!>    @author francisco
!>    @brief modes of vibration, last changes:<br>
!>    new module, get the vibration mode shapes - francisco - 03/12/2008
!>    added new argument pp in modos - francisco 05/11/2010<br>
!>    added dl dimension check - francisco 22/02/2011<br>
!>    added standard i/o - francisco - 16/02/2012<br>
!>    added time response angle rangle on mdd block - francisco 01/10/20
!>    added HPGL plot support - francisco - jun/15<br>
!>    added central messages, conversion functions - francisco - apr-19<
!>    added eigenvalue check for qsi^2 to filter invalid modes - francis
!
!     ==================================================================
!>    @brief mode shape analysis.
!
!>    @param[in] rpm speed for bearing dependent parameters (rpm)
!>    @param[in] pp number of effective bearings with support
!>    @param[in] std standard input output
!>    @param[in] plt HPGL output
!>    @param[out] errmsg return an error message
!>    @param[out] ok return flag. unsuccessful if <0.
!
subroutine modos(rpm,pp,std,plt,errmsg,ok)
  use com_epm, only: fi,avl,ddm
  use com_mdd, only: qtd_modos
  use com_sec, only: y
  use rd_kinds, only: lrk,wp
  use rd_modal_metrics, only: modal_metrics_t,prescribed_metrics,orbit_component,whirl_code,finite_complex
  implicit none
  real(lrk) :: rpm
  integer :: pp,ok
  logical :: std,plt
  character(len=99) :: errmsg
  integer, parameter :: mte=1000,xmd=19,mts=999,mtf=250,npo=50
  integer :: i,j,k,l,nm,npv,nc,sdm,oki,t,shaft_ndof_f,indices(mte)
  real(lrk) :: u1(xmd,mtf),w1(xmd,mtf),fmd(xmd)
  real(lrk), allocatable :: orx(:,:,:),orz(:,:,:)
  real(wp) :: phase,pi,mode_amplitude
  complex(wp) :: x(mtf),z(mtf)
  type(modal_metrics_t) :: metrics
  character(len=2) :: di(xmd),local_code
  ok=-1; errmsg=''; nm=qtd_modos; pi=acos(-1._wp)
  sdm=shaft_ndof_f(); npv=sdm/4
  if(nm<1.or.nm>xmd.or.ddm<1.or.ddm>mte) then
    errmsg='modos: expected 1..19 physical modes and a valid eigensolution'; return
  end if
  if(mod(sdm,4)/=0.or.npv<1.or.npv>mtf.or.sdm>ddm/2) then
    errmsg='modos: shaft coordinates do not match the physical eigenspace'; return
  end if
  nc=0
  do k=1,ddm
    metrics=prescribed_metrics(avl(k))
    if(.not.metrics%valid.or..not.metrics%oscillatory.or.aimag(avl(k))<=0._wp) cycle
    nc=nc+1; indices(nc)=k
  end do
  do i=2,nc
    t=indices(i); j=i-1
    do while(j>=1)
      if(aimag(avl(indices(j)))<=aimag(avl(t))) exit
      indices(j+1)=indices(j); j=j-1
    end do
    indices(j+1)=t
  end do
  if(nc<nm) then
    errmsg='modos: requested oscillatory mode unavailable; inspect complete-spectrum audit'; return
  end if
  allocate(orx(xmd,mts,npo),orz(xmd,mts,npo))
  u1=0._lrk; w1=0._lrk; orx=0._lrk; orz=0._lrk; fmd=0._lrk; di='ND'
  call progress_begin('LATERAL_MODES',nm,'reconstruct prescribed-spin physical mode orbits')
  do i=1,nm
    k=indices(i)
    call modal_require_pair_counts(k,nm,0,avl,mte,errmsg,oki)
    if(oki/=0) return
    metrics=prescribed_metrics(avl(k)); fmd(i)=real(metrics%wd/(2._wp*pi),lrk)
    do j=1,npv
      x(j)=fi(4*j-3,k); z(j)=fi(4*j-2,k)
    end do
    if(.not.all(finite_complex(x(1:npv))).or..not.all(finite_complex(z(1:npv)))) then
      errmsg='modos: nonfinite physical eigenvector'; return
    end if
    di(i)=whirl_code(x(1:npv),z(1:npv),real(rpm,wp))
    mode_amplitude=max(maxval(abs(x(1:npv))),maxval(abs(z(1:npv))))
    do j=1,npv
      u1(i,j)=real(orbit_component(x(j),0._wp),lrk)
      w1(i,j)=real(orbit_component(z(j),0._wp),lrk)
      do l=1,npo
        phase=2._wp*pi*real(l-1,wp)/real(npo-1,wp)
        orx(i,j,l)=real(orbit_component(x(j),phase),lrk)
        orz(i,j,l)=real(orbit_component(z(j),phase),lrk)
      end do
      local_code='ND'
      if(mode_amplitude>tiny(1._wp)) then
        if(max(abs(x(j)),abs(z(j)))/mode_amplitude>1.e-10_wp) &
          local_code=whirl_code(x(j:j),z(j:j),real(rpm,wp))
      end if
      write(*,'(a,1x,es24.16e3,2(1x,i0),1x,a)') &
        'RD_AUDIT_V1 WHIRL_NODE',real(rpm,wp),i,j,local_code
    end do
    write(*,'(a,1x,es24.16e3,2(1x,i0),5(1x,es24.16e3),1x,a)') &
      'RD_AUDIT_V1 MODE',real(rpm,wp),i,k,real(fmd(i),wp),metrics%radius, &
      metrics%growth,metrics%zeta,metrics%delta,di(i)
    call progress_update('LATERAL_MODES',i,nm,fmd(i),'HZ')
  end do
  call progress_stage('LATERAL_MODES','write mode-shape output')
  call s_modos(rpm,nm,npv,fmd,y,u1,w1,orx,orz,di,npo,xmd,mts,mtf,std,plt,errmsg,ok)
  if(ok==0) call progress_end('LATERAL_MODES','OK')
end subroutine modos
!
