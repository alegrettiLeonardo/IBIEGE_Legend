!     $Id$
!     ==================================================================
!
!>    @file tmatrices.f
!>    @brief torsional matrices.
!>    last changes:<br>
!>    new file dec-19 - f.
!
!     ==================================================================
!>    @brief 2 nodes torsion inertia matrix elements
!
!>    @param[out] tj2 2 nodes torsion inertia matrix elements
!
subroutine tj2s(tj2)
  use rd_kinds, only: wp
  implicit none
!
  real(wp) :: tj2
  dimension tj2(2,2)
!
  integer :: i
  real(wp) :: cj1, cj2
  dimension cj1(2),cj2(2)
  parameter (cj1 = (/1._wp/3._wp,1._wp/6._wp/),cj2 = (/1._wp/6._wp,1._wp/3._wp/))
!
  do i = 1,2
    tj2(i,1) = cj1(i)
    tj2(i,2) = cj2(i)
  end do
!
  return
!
end subroutine tj2s
!
!     ==================================================================
!>    @brief 2 nodes torsion stiffness matrix elements
!
!>    @param[out] tk2 2 nodes torsion stiffness matrix elements
!
subroutine tk2s(tk2)
  use rd_kinds, only: wp
  implicit none
!
  real(wp) :: tk2
  dimension tk2(2,2)
!
  integer :: i
  real(wp) :: ck1, ck2
  dimension ck1(2),ck2(2)
  parameter (ck1 = (/1._wp,-1._wp/),ck2 = (/-1._wp,1._wp/))
!
  do i = 1,2
    tk2(i,1) = ck1(i)
    tk2(i,2) = ck2(i)
  end do
!
  return
!
end subroutine tk2s
!
!     ==================================================================
!>    @brief 3 nodes torsion inertia matrix elements
!
!>    @param[out] tj3 3 nodes torsion inertia matrix elements
!
subroutine tj3s(tj3)
  use rd_kinds, only: wp
  implicit none
!
  real(wp) :: tj3
  dimension tj3(3,3)
!
  integer :: i
  real(wp) :: cj1, cj2, cj3
  dimension cj1(3),cj2(3),cj3(3)
  parameter (cj1 = (/2._wp/15._wp,1._wp/15._wp,-1._wp/30._wp/),cj2 = (/1._wp/15._wp,8._wp/15._wp,1._wp/15._wp/),cj3 = &
    & (/-1._wp/30._wp,1._wp/15._wp,2._wp/15._wp/))
!
  do i = 1,3
    tj3(i,1) = cj1(i)
    tj3(i,2) = cj2(i)
    tj3(i,3) = cj3(i)
  end do
!
  return
!
end subroutine tj3s
!
!     ==================================================================
!>    @brief 3 nodes torsion stiffness matrix elements
!
!>    @param[out] tk3 3 nodes torsion stiffness matrix elements
!
subroutine tk3s(tk3)
  use rd_kinds, only: wp
  implicit none
!
  real(wp) :: tk3
  dimension tk3(3,3)
!
  integer :: i
  real(wp) :: ck1, ck2, ck3
  dimension ck1(3),ck2(3),ck3(3)
  parameter (ck1 = (/2._wp,-2._wp,0._wp/),ck2 = (/-2._wp,4._wp,-2._wp/),ck3 = (/0._wp,-2._wp,2._wp/))
!
  do i = 1,3
    tk3(i,1) = ck1(i)
    tk3(i,2) = ck2(i)
    tk3(i,3) = ck3(i)
  end do
!
  return
!
end subroutine tk3s
!
!     ==================================================================
!>    @brief [J], [C], [K] matrices assembly.
!
!>    @param[in] im torsion model identifier: 1 two, 2 three nodes.
!>    @param[out] pdm final matrix dimension
!>    @param[out] errmsg return an error message
!>    @param[out] ok return flag. unsuccessful if <0. sucess

subroutine tmatrices(im,pdm,errmsg,ok)
  use rd_textfun, only: fomsgf
  use com_conc, only: nmic, psic, vlmc, ixic, iyic, izic
  use com_couplings, only: cplgid, cplgst, cplgdp, cplgin, cplgsr, ncplg
  use com_dis, only: pd, d_d, h_d, rho_d, r2, nd
  use com_disa, only: d_i, i_x, i_y, m_d
  use com_eix, only: l, d, di, ps, e, nu, rho_e, g_e, r1, cs
  use com_eqvlen, only: teql, tsl
  use com_sea, only: rs, es, gs, dely, s, ie, ump
  use com_sec, only: n, y, nt, nn
  use com_sprat, only: crat
  use com_sprat1, only: csrt
  use com_sprat2, only: gsst
  use com_tcoup, only: idsec, cnsc
  use com_tman, only: ntbr, tpc, tkk, tcc, tjj, sct
  use com_tmat, only: tj, tk, tc, dm
  use rd_kinds, only: lrk, wp
  implicit none
!
!     arguments
  integer :: im, pdm, ok
  character(len=99) :: errmsg
!
!     locals
  integer :: ii, jj, kk, m1, ndv, oo, pp, csid, c1, c2, c3, oki, pstep, ptotal
  integer :: indypf
  real(wp) :: scrs, scgs, scdp, fcd, ds1, ds2, ds3
  real(lrk) :: dy, sumv_r
  character(len=3) :: cbd
  character(len=5) :: c5
  character(len=9) :: nmm
  parameter (cbd= 'mtg',c5 = 'ETYPE',nmm = 'tmatrices')
!
!     torsional related
!     elemental matrices
  real(wp) :: kt2(2,2), jt2(2,2), kt3(3,3), jt3(3,3)
!
!     input parameters
  integer :: mxs
  parameter (mxs = 99)
!
!     shaft
!
!     shaft parameters block
!
!     total number of divisions
  integer :: mts
  parameter (mts = 999)
!
!     all divisions
!
!     additional data by section
!
!     bearings
  integer :: mxm
  parameter (mxm = 9)
!     torsion restriction bearings, added mar-21
!
!     disks
  integer :: mxd
  parameter (mxd = 99)
!
!     concentrated masses and inertias
  integer :: mxic
  parameter (mxic = 15)
!
!     torsion couplings
  integer :: mxcplg
  parameter (mxcplg = 20)
!     number of couplings
!     section ids
!     stiffness, damping,inertia,speed ratio
!
  integer :: mtt
  parameter (mtt = mts)
!     section equivalent length,division equivalent length
!
!     coupling divisions properties
  real(wp) :: rssec, gssec
  real(lrk) :: ratsec, dmpsec
  dimension rssec(mtt),gssec(mtt),&
  &ratsec(mtt),dmpsec(mtt)
!
!
!     speed ratio
!
!
!     equivalent G
!
!     maximal number of global matrices elements
  integer :: mtg
  parameter (mtg = 500)
!
!     global torsion matrices:inertia tj, stiffness tk, damping tc
!
  intrinsic :: nint, real
!
!     initialize return
  ok = -1
!
!     sum all divisions
  ndv = nint(sumv_r(n,mts,nt))
!
!     model index, allowed 1 or 2
  if(im .ne. 1 .and. im .ne. 2) then
!       invalid option model index
    errmsg = fomsgf(99, nmm,6,c5,0)
    return
!
  else if (im .eq. 1) then
    call tk2s(kt2)
    call tj2s(jt2)
!       torsion matrix dimension
    dm = ndv+1
  else if (im .eq. 2) then
    call tk3s(kt3)
    call tj3s(jt3)
    dm = 2*ndv+1
  end if
!
!     check matrices dimension, may check crat dimenstion
  if (dm .gt. mtg) then
!       return with error
    errmsg = fomsgf(99, nmm,4,cbd,0)
    return
  end if
!
!     Processing telemetry.
  ptotal=ndv+ntbr+nd+nmic+1
  pstep=0
  call progress_begin('TORSION_MATRIX_ASSEMBLY',ptotal,&
  &'torsional J/C/K physical assembly')
  call progress_stage('TORSION_MATRIX_ASSEMBLY','zero matrices')
!     zero matrices -> tj,tk,tc
  call zermat_d(tj,dm,dm,mtg,mtg)
  call zermat_d(tk,dm,dm,mtg,mtg)
  call zermat_d(tc,dm,dm,mtg,mtg)
!
!     matrix size -> out argument
  pdm = dm
!
  if (ncplg .gt. 0) then
!       get coupling properties
    call tcouprops(cplgid,cplgst,cplgdp,cplgin,cplgsr,&
    &n,Y,dely,Ie,rs,gs,tsl,ps,&
    &rssec,gssec,ratsec,dmpsec,idsec,&
    &cnsc,ncplg,nn,mts,mtt,mxs,&
    &mxcplg,errmsg,oki)
!       check for return code
    if (oki .lt. 0) then
!         return with error
      return
    end if
  else
!       number of coupling properties
    cnsc = 0
  end if
!
!     coupling division id
  csid = 1
!     counter
  c1 = 1
!     initial number of divisions
  m1 = nint(n(1))
!
!     divisions loop
  do ii = 1,ndv
    if (ii .le. m1) then
!         adding fillet effect tsl - additional length
      dy = dely(c1)+tsl(ii)
!         fillet correction factor
!         compensate density
      fcd = dy/dely(c1)
    else
      c1 = c1+1
      m1 = m1+ nint(n(c1))
      dy = dely(c1)+tsl(ii)
      fcd = dy/dely(c1)
    end if
!       submatrix indices
    if  (im .eq. 1) then
      c2 = ii
      c3 = c2+1
    else if(im .eq. 2) then
      c2 = (ii-1)*2+1
      c3 = c2+2
    end if
!
!       section properties
    if (csid .le. cnsc) then
!         have coupling and index is less or equals counter
      if (ii .eq. idsec(csid)) then
        scrs = rssec(csid)
        scgs = gssec(csid)
        scdp = dmpsec(csid)
        crat(ii) = ratsec(csid)
!           coupling index
        csid = csid+1
      else
!           ordinary section
        scrs = rs(ii)
        scgs = gs(ii)
        scdp = 0
        if (csid .gt. 1) then
!             speed ratio
          crat(ii) = ratsec(csid-1)
        else
          crat(ii) = 1
        end if
      end if
    else
      scrs = rs(ii)
      scgs = gs(ii)
      scdp = 0
      if (csid .gt. 1) then
!           speed ratio
        crat(ii) = ratsec(csid-1)
      else
        crat(ii) = 1
      end if
    end if
!       equivalent G
    gsst(ii) = real(scgs, lrk)
!       all divisions speed ratio
    csrt(c2) = crat(ii)
    if (im .eq. 2) then
!         two node model, intermediate section
      csrt(c2+1) = crat(ii)
    end if
!       division property
!       inertia
    ds1 = scrs*fcd*dy*2*ie(ii)
!       stiffness
    ds2 = scgs*2*ie(ii)/dy
!       speed ratio
    ds3 = crat(ii)**2
!       check bound
    if (c2 .gt. mtg .or. c3 .gt. mtg) then
      errmsg = fomsgf(99, nmm,4,cbd,0)
      return
    end if
!       matrices assemble
    do jj = c2,c3
      do kk = c2,c3
!           sub-matrices indices
        oo = jj-c2+1
        pp = kk-c2+1
        if (im .eq. 1) then
!             torsion inertia matrix
          tj(jj,kk) = tj(jj,kk)+ds1*ds3*jt2(oo,pp)
!             torsion stiffness
          tk(jj,kk) = tk(jj,kk)+ds2*ds3*kt2(oo,pp)
        else
          tj(jj,kk) = tj(jj,kk)+ds1*ds3*jt3(oo,pp)
          tk(jj,kk) = tk(jj,kk)+ds2*ds3*kt3(oo,pp)
        end if
!           damping
        tc(jj,kk) = tc(jj,kk)+ds3*scdp
      end do
    end do
!
    pstep=pstep+1
    call progress_update('TORSION_MATRIX_ASSEMBLY',pstep,ptotal,real(ii, lrk),'SHAFT_ELEMENT')
!     end divisions loop
  end do
!
!     last value
  csrt(dm) = crat(ndv)
!
!     torsion restriction bearing
  call progress_stage('TORSION_MATRIX_ASSEMBLY',&
  &'assemble torsional restrictions')
  do ii = 1,ntbr
!       bearing division index
    m1 = indypf(tpc(ii),errmsg)
    if (m1 .lt. 0) then
!         return with not found error
      return
    end if
!       model index
    if (im .eq. 1) then
!         one division
      c2 = m1
    else
!         two divisions
      c2 = (m1-1)*2+1
    end if
!       speed ratio
    if (m1 .gt. 1) then
      ds3 = crat(m1-1)**2
    else
      ds3 = 1
    end if
!       check bound
    if (c2 .gt. mtg) then
      errmsg = fomsgf(99, nmm,4,cbd,0)
      return
    end if
!       stiffness sc = value scaling
    tk(c2,c2) = tk(c2,c2)+ds3*sct*tkk(ii)
!       damping
    tc(c2,c2) = tc(c2,c2)+ds3*sct*tcc(ii)
!       inertia
    tj(c2,c2) = tj(c2,c2)+ds3*tjj(ii)
    pstep=pstep+1
    call progress_update('TORSION_MATRIX_ASSEMBLY',pstep,ptotal,real(ii, lrk),'RESTRICTION')
  end do
!
!     disk torsion
  call progress_stage('TORSION_MATRIX_ASSEMBLY',&
  &'assemble disk inertias')
  do ii = 1,nd
!       disk division index
    m1 = indypf(pd(ii),errmsg)
    if (m1 .lt. 0) then
!         return with not found error
      return
    end if
!       model index
    if  (im .eq. 1) then
      c2 = m1
    else
      c2 = (m1-1)*2+1
    end if
!       speed ratio
    if (m1 .gt. 1) then
      ds3 = crat(m1-1)**2
    else
      ds3 = 1
    end if
!       check bound
    if (c2 .gt. mtg) then
      errmsg = fomsgf(99, nmm,4,cbd,0)
      return
    end if
!       mass inertia
    tj(c2,c2) = tj(c2,c2)+ds3*i_y(ii)
    pstep=pstep+1
    call progress_update('TORSION_MATRIX_ASSEMBLY',pstep,ptotal,real(ii, lrk),'DISK')
  end do
!
!     concentrated torsion
  call progress_stage('TORSION_MATRIX_ASSEMBLY',&
  &'assemble concentrated inertias')
  do ii = 1,nmic
!       concentrated division index
    m1 = indypf(psic(ii),errmsg)
    if (m1 .lt. 0) then
!          return with not found error
      return
    end if
!       model index
    if  (im .eq. 1) then
      c2 = m1
    else
      c2 = (m1-1)*2+1
    end if
!       speed ratio
    if (m1 .gt. 1) then
      ds3 = crat(m1-1)**2
    else
      ds3 = 1
    end if
!       check bound
    if (c2 .gt. mtg) then
      errmsg = fomsgf(99, nmm,4,cbd,0)
      return
    end if
!       inertia
    tj(c2,c2) = tj(c2,c2)+ds3*iyic(ii)
    pstep=pstep+1
    call progress_update('TORSION_MATRIX_ASSEMBLY',pstep,ptotal,real(ii, lrk),'CONCENTRATED_INERTIA')
  end do
!
  pstep=ptotal
  call progress_update('TORSION_MATRIX_ASSEMBLY',pstep,ptotal,real(dm, lrk),'DOF')
  ok = 0
  call progress_end('TORSION_MATRIX_ASSEMBLY','OK')
!
  return
!
end subroutine tmatrices
!
!     ==================================================================
!>    @brief modal damping matrix
!
subroutine tmdamp()
  use com_mdamps, only: tdmrt, tdmmd, ntdmp
  use com_mdampso, only: mdoff
  use com_tepm, only: rfi, rlam, ddm
  use com_tmdm, only: tmd
  use rd_kinds, only: lrk
  implicit none
!
!     modal dampings
  integer :: mxtdmp
  parameter (mxtdmp = 10)
!     number of modal dampings,mode offset
!     mode ids
!     damping ratio
!     mode offset
!
!     maximal number of global matrices elements
  integer :: mtg
  parameter (mtg = 500)
!
!     torsion modal space
!
!     modal damping
!
  integer :: jj, mi
  real(lrk) :: om
!
  intrinsic :: sqrt
!
!     zero modal damping matrix
  call zermat_r(tmd,ddm,ddm,mtg,mtg)
!
  do jj = 1,ntdmp
!       mode id
    mi = tdmmd(jj)
!       check mode id
    if (mi .gt. 0 .and. mi .le. ddm) then
!         natural frequency rad/s
      om = sqrt(rlam(mi))
!         modal damping = 2*w*qsi
      tmd(mi,mi) = 2*om*tdmrt(jj)
    else
!         warning
      call elmsgw(0,25,'tmdamp')
    end if
  end do
!
  return
!
end subroutine tmdamp
!
