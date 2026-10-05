!     $Id$
!     ==================================================================
!
!>    @file tcouprops.f
!>    @brief torsional coupling properties.
!>    last changes:<br>
!>    new file dec-19 - f.
!
!     ==================================================================
!>    @brief coupling equivalent section properties
!
!>    @param[in] cplgid coupling section ids
!>    @param[in] cplgst coupling stiffness
!>    @param[in] cplgdp coupling damping
!>    @param[in] cplgin coupling inertia
!>    @param[in] cplgsr coupling speed ratio
!>    @param[in] n number of divisions
!>    @param[in] Y main section positions
!>    @param[in] dely nodal distance
!>    @param[in] Ie y mass inertia
!>    @param[in] rs density
!>    @param[in] gs shear modulus
!>    @param[in] tsl torsional division length add
!>    @param[in] ps section positions
!>    @param[out] rssec values of density to result on coupling inertia
!>    @param[out] gssec values of shear module to result on coupling sti
!>    @param[out] ratsec speed ratio
!>    @param[out] dmpsec coupling damping
!>    @param[out] idsec index of coupling sections
!>    @param[out] ncsp number of couplinc division properties
!>    @param[in] ncp number of couplings
!>    @param[in] nn number of number of divisions n
!>    @param[in] mts divisions vectors dimension
!>    @param[in] mtt coupling division vetors dimension
!>    @param[in] mxs sections vector dimension
!>    @param[in] mxc coupling data vector dimension
!>    @param[out] errmsg return an error message
!>    @param[out] ok return flag. unsuccessful if <0.
subroutine tcouprops(cplgid,cplgst,cplgdp,cplgin,cplgsr,&
&n,Y,dely,Ie,rs,gs,tsl,ps,&
&rssec,gssec,ratsec,dmpsec,idsec,&
&ncsp,ncp,nn,mts,mtt,mxs,mxc,errmsg,ok)
  use rd_kinds, only: lrk, wp
  implicit none
!
  character(len=99) :: errmsg
  integer :: ncp, nn, ncsp, mts, mtt, mxs, mxc, ok
  integer :: cplgid
  real(lrk) :: cplgst, cplgdp, cplgin, cplgsr
  dimension cplgid(mxc),cplgst(mxc),cplgdp(mxc),&
  &cplgin(mxc),cplgsr(mxc)
  real(lrk) :: n, Y, dely, Ie, rs, gs, tsl, ps
  dimension n(mts),Y(mts),dely(mts),Ie(mts),rs(mts),&
  &gs(mts),tsl(mtt),ps(mxs)
!
  integer :: nsc, idsec
  real(lrk) :: ratsec, dmpsec
  real(wp) :: rssec, gssec
  dimension idsec(mtt),rssec(mtt),gssec(mtt),&
  &ratsec(mtt),dmpsec(mtt)
!
  real(lrk) :: fcp, icp, dtl
  real(wp) :: tj, tk, ack, acj
  real(lrk) :: cpk, cpj
!     index functions
  integer :: dividf, indypf
  integer :: ii, jj, kk, mm, cfi, cii, fci, ici

!     indices
  integer :: sci(mtt)
!     inertias,stiffness,distances,coupling fictive density,
!     couplig fictive shear modulus,speed ratio,coupling damping
  real(wp) :: scj, sck, cgs, crs
  real(lrk) :: dyc, spr, cdp
  dimension dyc(mtt),crs(mtt),&
  &scj(mtt),sck(mtt),cgs(mtt),&
  &spr(mtt),cdp(mtt)
!     minimum coupling inertia if not well defined
  real(lrk) :: precj
  parameter(precj = 1e-9_lrk)
!
!     init
!     return init
  ok = -1
!
  ncsp = 0
!     coupling loop
  do ii = 1,ncp
!       coupling section final position index
    jj = cplgid(ii)
    cfi = jj+1
!       coupling section initial position index
    cii = jj
!       final coupling position (m)
    fcp = ps(cfi-1)
!       final coupling index position
    fci = indypf(fcp,errmsg)
    if (fci .lt. 0) then
!         return with error
      return
    end if
!       initial coupling position (m)
    if (cii .gt. 1) then
      icp = ps(cii-1)
    else
      icp = 0
    end if
!       initial coupling index position
    ici = indypf(icp,errmsg)
    if (ici .lt. 1) then
!         return with error
      return
    end if
!       number of sections
    nsc = fci-ici
!
!       coupling sections
    do jj = 1,nsc
!         global section index
      kk = ici+jj-1
      sci(jj) = kk
!         search division index
      mm = dividf(n,dely,Y(kk+1),nn,mts,errmsg)
      if (mm .lt. 0) then
        !         return with error
        return
      end if
!         section additional length
      dtl = tsl(kk)
      dyc(jj) = dely(mm)+dtl
!         section inertia
!         compensate density here, just dely
      scj(jj) = rs(kk)*dely(mm)*2*Ie(kk)
!         section stiffness
      sck(jj) = gs(kk)*2*Ie(kk)/dyc(jj)
    end do
!       total inertia
    tj = 0
!       equivalent stiffness
    tk = 0
    do jj = 1,nsc
      tj = tj + scj(jj)
      tk = tk + 1/sck(jj)
    end do
    tk = 1/tk
!       coupling stiffness
    cpk = cplgst(ii)
!       coupling inertia
    cpj = cplgin(ii)
    if (cpj  .lt. precj) then
!         warning invalid coupling inertia
      call elmsgw(0,29,'tcouprops')
!         needed on two node inertia matrix
      cpj = precj
    end if
!       stiffness ratio:coupling/shaft
    ack = cpk/tk
!       inertia ratio:coupling/shaft
    acj = cpj/tj
!
    do jj = 1,nsc
!         global section index
      kk = ici+jj-1
      crs(jj) = scj(jj)/dyc(jj)/2/Ie(kk)*acj
      cgs(jj) = sck(jj)/2/Ie(kk)*dyc(jj)*ack
!         speed ratio
      spr(jj) = cplgsr(ii)
!         damping
      cdp(jj) = cplgdp(ii)/nsc
    end do
!       results
    do jj = 1,nsc
      ncsp = ncsp+1
      idsec(ncsp) = sci(jj)
      rssec(ncsp) = crs(jj)
      gssec(ncsp) = cgs(jj)
      ratsec(ncsp) = spr(jj)
      dmpsec(ncsp) = cdp(jj)
    end do
!       coupling loop
  end do
!
  ok = 0
!
  return
!
end subroutine tcouprops
!
