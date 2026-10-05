!     $Id$
!     ==================================================================
!
!>    @file matrizes.f
!>    @brief lateral matrices assembly, last changes:<br>
!>    new module - francisco - 03/12/2008<br>
!>    added supports in the matrices - francisco - 20/10/2010<br>
!>    offset support on mass matrix calculation - francisco - feb-19<br>
!>    added nsupp function to determine the number of effective supports
!>    added central messages - francisco - apr-19<br>
!>    kasc now stores the complete Timoshenko shear parameter Phi<br>
!>    added gyr.section comp. equivalent diam. on matrizes - francisco a
!
!     ==================================================================
!>    @brief determines the number of effective bearing supports.
!>    @return number of effective bearing supports
!
integer function nsuppf()
  use com_knd, only: kind => cknd, supr
  use com_sp1, only: ns, bn
  use com_ymc, only: nbrg, rks, pc
  use rd_kinds, only: lrk
  implicit none
!
  integer :: pp, ii, jj, kk
  character(len=1) :: cps
  character(len=6) :: nmm
  parameter (cps = 'S',nmm = 'nsuppf')
!
!     input parameters
  integer :: mxm
  parameter (mxm = 9)
!
!     bearing block
!
!     support data block
!     number of parameters lines
!     bearing number
!
!     calculation kind
!
  kk = 0
!     effective number of supports
  pp = 0
  nsuppf = 0
!
!     calculation will use supports, see entrada.f
  if (supr .eq. cps) then
!       check bearing number = support number
!       bearings
    do ii = 1,nbrg
!         supports
      kk = kk+1
      do jj = 1,ns
        if (bn(jj) .eq. kk) then
          pp = pp+1
        end if
      end do
    end do
  end if
!
  nsuppf = pp
!
  return
!
end function nsuppf
!
!     ==================================================================
!>    @brief [M], [C], [K] and [G] matrices assembly.
!
!>    @param[out] pp number of effective bearing supports
!>    @param[out] pdm final [AA] matrix dimension
!>    @param[out] errmsg return an error message
!>    @param[out] ok return flag. unsuccessful if <0. sucess
!
subroutine matrizes(pp,pdm,errmsg,ok)
  use rd_textfun, only: fomsgf
  use com_conc, only: nmic, psic, vlmc, ixic, iyic, izic
  use com_dflexi, only: dmodel, dfdofx, dfdofz, nfdisk, nshaftfd
  use com_dflexr, only: dkr, df1nd
  use com_dis, only: pd, d_d, h_d, rho_d, r2, nd
  use com_doff, only: off_d
  use com_dsc, only: md, idx, idy
  use com_kasc, only: vkasc
  use com_knd, only: cknd, supr
  use com_mat, only: mm, mg, dm, smn
  use com_mbk, only: mkb
  use com_mtk, only: mk1
  use com_sea, only: rs, es, gs, dely, s, ie, ump
  use com_sec, only: n, y, nt, ni => nn
  use com_seg, only: gfc
  use com_sp1, only: ns, bn
  use com_sp2, only: sup, sps
  use com_ymc, only: nc => nbrg, rks, pc
  use rd_kinds, only: lrk
  use shaft_timoshenko_cowper, only: timo_cowper_stiffness,&
  &timo_cowper_mass,timo_cowper_gyro,ump_element_matrix
  implicit none
!
!     arguments
  integer :: pdm, pp, ok
  character(len=99) :: errmsg
!
!     locals
  character(len=3) :: cbd
  character(len=8) :: nmm
  parameter(cbd = 'mtg',nmm = 'matrizes')
  real(lrk) :: a, kappa, phi_m, phi_g, kappa_m, kappa_g, sumv_r, m, dy
  integer :: c, ii, jj, kk, ll, nn, oo, r, nsuppf, nf, nshaft, nfdof, pstep, ptotal, ielok
!     function
  integer :: inddis, ifidxof, foundation_ndof_f, support_dof_x
  integer :: flexdisk_ndof_f
  integer :: oki
!
!     sub-matrices mass/stiffness/gyroscopic/ump
  real(lrk) :: sm, sk, sg, su
  dimension sm(8,8),sk(8,8),sg(8,8),su(8,8)
!
!     maximal number of global matrices elements
  integer :: mtg
  parameter (mtg = 500)
!
!     global matrices de mass mm, stiffness mk,
!     gyroscopic mg, damping mc
!     base ump matrix
!     base stiffness matrix
!
!     input parameters
  integer :: mxd, mxm
  parameter (mxd = 99,mxm = 9)
!
!     disks
!
!     disks block
!
!     added - francisco - feb-19
!     flexible disk reduced model
!
!     concentrated masses e inertias block
  integer :: mxic
  parameter (mxic = 15)
!
!     total number of sections
  integer :: mts
  parameter (mts = 999)
!
!     all sections (divisions)
!     in other commons ni = nn
!     section shear correction factor
!
!     additional data each section

!     added gyroscopic section compensation for equivalent diameters
!     franciscp - aug-21
!
!     bearing block
!     added pc francisco 13/04/2007
!
!     support data block
!     number of parameters lines
!     bearing number
!     support data, see entrada.f, beasupf
!     1   2   4   3   5   6   8   7   9
!     kxx,kxz,kzz,kzx,cxx,cxz,czz,czx,mas
!     added scale sps - francisco - nov-15
!
!     calculation kind
!
  intrinsic :: nint
!
!     initialize return
  ok = -1
!
!     sum all divisions
  smn = nint(sumv_r(n,mts,nt))
!
!     Shaft DOFs remain the historical contiguous 4-DOF/station block.
  nshaft = 4*(smn+1)
  call flexdisk_assign_dofs(nshaft)
  nfdof = flexdisk_ndof_f()
!     Flexible-disk rotations are appended after shaft DOFs.
  dm = nshaft+nfdof
!
!     effective number of bearings with support
!     pp = 0 if no consider supports
  pp = nsuppf()
!
!     augment pp * support matrix [S] 2x2
  dm = dm+(2*pp)
!
!     optional non-rotating dynamic foundation substructure
  nf = foundation_ndof_f()
  dm = dm+nf
!
!     final AA dimension
  pdm = 2*dm
!
!     check maximal global matrices dimension
  if (dm .gt. mtg) then
    errmsg = fomsgf(99, nmm,4,cbd,0)
    return
  end if
!
!     Processing telemetry.  The total counts physical assembly items;
!     support/foundation phases are also identified explicitly by stage.
  ptotal=smn+nd+nmic+1
  pstep=0
  call progress_begin('MATRIX_ASSEMBLY',ptotal,&
  &'lateral M/K/G physical assembly')
  call progress_stage('MATRIX_ASSEMBLY','zero global matrices')
!
!     put zero in the matrices
!     mass -> mm
  call zermat_r(mm,dm,dm,mtg,mtg)
!     base stiffness -> mk1
  call zermat_r(mk1,dm,dm,mtg,mtg)
!     basic global stiffness matrix mkb
!     without bearing stiffness and UMP -> mkb
  call zermat_r(mkb,dm,dm,mtg,mtg)
!     gyroscopic -> mg
  call zermat_r(mg,dm,dm,mtg,mtg)
!
  c = 1
  m = n(1)
!     loop all divisions
  do r = 1,smn
!       division length
    if (r .le. m) then
      dy = dely(c)
    else
!         section change
      c = c+1
      m = m+n(c)
      dy = dely(c)
    end if
!
!       Complete Timoshenko + Cowper shaft element. K, M and G use the
!       same shear parameter Phi = 12 E I / (kappa G A L^2).
!       PREDAD already owns discretization/conical geometry and supplies
!       effective midpoint A/I for each FE; do not reinterpret nodal ste
!
    call timo_cowper_stiffness(dy,es(r),gs(r),s(r),ie(r),&
    &a,kappa,sk,ielok)
    if (ielok .lt. 0) then
      errmsg = 'Invalid Timoshenko/Cowper shaft element properties'
      return
    end if
    call timo_cowper_mass(dy,es(r),gs(r),rs(r),s(r),ie(r),&
    &sm,phi_m,kappa_m,ielok)
    if (ielok .lt. 0) then
      errmsg = 'Invalid Timoshenko/Cowper shaft element mass data'
      return
    end if
    call timo_cowper_gyro(dy,es(r),gs(r),rs(r),s(r),ie(r),&
    &gfc(r),sg,phi_g,kappa_g,ielok)
    if (ielok .lt. 0) then
      errmsg = 'Invalid Timoshenko/Cowper shaft gyroscopic data'
      return
    end if
!
!       Phi is also used by the static elastic-line interpolation.
    vkasc(r) = a
!
!       UMP is distributed negative stiffness k' [N/m2] and is
!       intentionally kept independent of the physical mass formulation.
    if (ump(r) .gt. 0) then
      call ump_element_matrix(dy,ump(r),su)
    else
      call zermat_r(su,8,8,8,8)
    end if
!
!       assembly global matrices
!
    kk = ifidxof(r,1)
    ll = 4*(r+1)
    do ii = kk,ll
      do jj = kk,ll
!           sub-matrices indices
        nn = ii-kk+1
        oo = jj-kk+1
!
!           mass matrix
        mm(ii,jj) = mm(ii,jj)+sm(nn,oo)
!
!           base stiffness matrix without UMP
        mkb(ii,jj) = mkb(ii,jj)+sk(nn,oo)
!
!           base stiffness matrix with ump
        mk1(ii,jj) = mk1(ii,jj)+sk(nn,oo)-su(nn,oo)
!
!           gyroscopic matrix
        mg(ii,jj) = mg(ii,jj)+sg(nn,oo)
      end do
    end do
    pstep=pstep+1
    call progress_update('MATRIX_ASSEMBLY',pstep,ptotal,real(r, lrk),'SHAFT_ELEMENT')
!       end loop divisions
  end do
!
!     discs
  call progress_stage('MATRIX_ASSEMBLY','assemble disk terms')
!
  do ii=1,nd
!       search disk position
    kk = inddis(pd(ii),errmsg,oki)
!       position found
    if (oki .gt. 0) then
      oo = ifidxof(kk,0)
!         translational disk mass is always carried by the shaft station
      ll = oo+1
      mm(ll,ll) = mm(ll,ll)+md(ii)
      ll = oo+2
      mm(ll,ll) = mm(ll,ll)+md(ii)
!
!         Disk offset couples disk-CG translations to shaft rotations.
      mm(oo+1,oo+4) = mm(oo+1,oo+4)-md(ii)*off_d(ii)
      mm(oo+4,oo+1) = mm(oo+4,oo+1)-md(ii)*off_d(ii)
      mm(oo+2,oo+3) = mm(oo+2,oo+3)+md(ii)*off_d(ii)
      mm(oo+3,oo+2) = mm(oo+3,oo+2)+md(ii)*off_d(ii)
      mm(oo+3,oo+3) = mm(oo+3,oo+3)+md(ii)*off_d(ii)**2
      mm(oo+4,oo+4) = mm(oo+4,oo+4)+md(ii)*off_d(ii)**2
!
      if (dmodel(ii) .eq. 0) then
!           Historical rigid disk: Id and Ip act on shaft rotations.
        mm(oo+3,oo+3) = mm(oo+3,oo+3)+idx(ii)
        mm(oo+4,oo+4) = mm(oo+4,oo+4)+idx(ii)
        ll = oo+3
        nn = oo+4
        mg(ll,nn) = mg(ll,nn)-idy(ii)
        mg(nn,ll) = mg(nn,ll)+idy(ii)
      else
!           Chen flexible-disk reduction.  Id and Ip act on two added
!           disk rotational DOFs.  KR connects disk and shaft rotations.
        ll = dfdofx(ii)
        nn = dfdofz(ii)
        mm(ll,ll) = mm(ll,ll)+idx(ii)
        mm(nn,nn) = mm(nn,nn)+idx(ii)
        mg(ll,nn) = mg(ll,nn)-idy(ii)
        mg(nn,ll) = mg(nn,ll)+idy(ii)
!
!           theta-x spring pair
        mk1(oo+3,oo+3)=mk1(oo+3,oo+3)+dkr(ii)
        mk1(ll,ll)=mk1(ll,ll)+dkr(ii)
        mk1(oo+3,ll)=mk1(oo+3,ll)-dkr(ii)
        mk1(ll,oo+3)=mk1(ll,oo+3)-dkr(ii)
        mkb(oo+3,oo+3)=mkb(oo+3,oo+3)+dkr(ii)
        mkb(ll,ll)=mkb(ll,ll)+dkr(ii)
        mkb(oo+3,ll)=mkb(oo+3,ll)-dkr(ii)
        mkb(ll,oo+3)=mkb(ll,oo+3)-dkr(ii)
!
!           theta-z spring pair
        mk1(oo+4,oo+4)=mk1(oo+4,oo+4)+dkr(ii)
        mk1(nn,nn)=mk1(nn,nn)+dkr(ii)
        mk1(oo+4,nn)=mk1(oo+4,nn)-dkr(ii)
        mk1(nn,oo+4)=mk1(nn,oo+4)-dkr(ii)
        mkb(oo+4,oo+4)=mkb(oo+4,oo+4)+dkr(ii)
        mkb(nn,nn)=mkb(nn,nn)+dkr(ii)
        mkb(oo+4,nn)=mkb(oo+4,nn)-dkr(ii)
        mkb(nn,oo+4)=mkb(nn,oo+4)-dkr(ii)
      endif
!         end if disk position
    else
!         if disk position not found return
      return
    end if
!
    pstep=pstep+1
    call progress_update('MATRIX_ASSEMBLY',pstep,ptotal,real(ii, lrk),'DISK')
!       end loop disks
  end do
!
!     concentrated masses and inertias
  call progress_stage('MATRIX_ASSEMBLY',&
  &'assemble concentrated mass/inertia terms')
!
  do ii = 1,nmic
!
!       search concentrated position
    kk = inddis(psic(ii),errmsg,oki)
!       position found
    if (oki .gt. 0) then
      oo = ifidxof(kk,0)
!         add mass and/or inertia
      ll = oo+1
      mm(ll,ll) = mm(ll,ll)+vlmc(ii)
      ll = oo+2
      mm(ll,ll) = mm(ll,ll)+vlmc(ii)
      ll = oo+3
      mm(ll,ll) = mm(ll,ll)+ixic(ii)
      nn = oo+4
      mm(nn,nn) = mm(nn,nn)+izic(ii)
!         add giroscopic matrix
      mg(ll,nn) = mg(ll,nn)-iyic(ii)
      mg(nn,ll) = mg(nn,ll)+iyic(ii)
!
    else
!         if concentrated position not found return
      return
    end if
!
    pstep=pstep+1
    call progress_update('MATRIX_ASSEMBLY',pstep,ptotal,real(ii, lrk),'CONCENTRATED_MASS')
!       end loop concentrated
  end do
!
!     bearing supports
  call progress_stage('MATRIX_ASSEMBLY',&
  &'assemble bearing support masses')
!
!     dinamica e vibrazioni delle macchine
!     dinamica dei rotori Giorgio Diana
!
  oo = 0
!
!     loop bearings
  do ii = 1,nc
!
!       loop supports on number of supports,
!       ns will be zero if does not consider, see above if supr.
    do jj = 1,ns
!
!         check if bearing has support
      if (bn(jj) .eq. ii) then
!           seach bearing position
!           bearing support, see also parmanv.f
        kk = inddis(pc(ii),errmsg,oki)
        if (oki .gt. 0) then
!
          oo = oo+1
!
!             position in the matrix
!             not needed here
!              ll = ifidxof(kk,1)
!
!             explicit support DOF mapping. Foundation DOFs, if any,
!             are located after all support DOFs.
          nn = support_dof_x(dm,pp,oo)
!
!             add mass on the diagonal
          mm((nn+0),(nn+0)) = sup(jj,9)
          mm((nn+1),(nn+1)) = sup(jj,9)
!
        else
!             if support position not found return
          return
!
!             end if bearing position
        end if
!
!           end if support check
      end if
!
!         end loop supports
    end do
!
!       end loop bearings
  end do
!
!     add user supplied reduced foundation mass matrix [Mf]
  call progress_stage('MATRIX_ASSEMBLY',&
  &'assemble foundation mass contribution')
  call foundation_add_mass(dm,mm,mtg)
  pstep=ptotal
  call progress_update('MATRIX_ASSEMBLY',pstep,ptotal,real(dm, lrk),'DOF')
!
!     return ok
  ok = 0
  call progress_end('MATRIX_ASSEMBLY','OK')
!
  return
!
end subroutine matrizes
!
