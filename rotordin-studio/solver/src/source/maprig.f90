!     $Id$
!     ==================================================================
!
!>    @file maprig.f
!>    @brief undamped critical speed map, last changes:<br>
!>    new global module - francisco - 08/06/2009<br>
!>    changed maprig to maprigs - added speed - francisco 22/02/2010<br>
!>    added standard i/o - francisco - 16/02/2012<br>
!>    added campbell loop - francisco - 25/05/2015<br>
!>    changed stiffnes increment to 1 - francisco jul-15<br>
!>    added prmkmra and mapriga, angular critical speed map - francisco
!>    changed errors to central messaged - francisco - apr-19<br>
!>    all eigen solutions on espmod - francisco - jun-20<br>
!>    updated screen feedback to central routine in saidas - francisco -
!>    changed interpolation algorithm speed and frequency loops - franci
!
!     ==================================================================
!>    @brief assembly big stiffness matrix.
!>    Prepares stiffness matrix with x and z bearing parameters.
!
!>    @param[in] dm matrix dimension
!>    @param[in] rk stiffness value
!>    @param[out] wk return big stiffness matrix
!>    @param[out] errmsg return an error message
!>    @param[out] ok return flag. unsuccessful if <0.
!
subroutine prmkmr(dm,rk,wk,errmsg,ok)
  use com_ymc, only: nbrg, rks, pc
  use rd_kinds, only: lrk
  implicit none
!
!     arguments
  integer :: dm, ok
  real(lrk) :: rk
  character(len=99) :: errmsg
!
!     locals
  integer :: ii, jj, kk, ll, nn
  integer :: oki, inddis, ifidxof
!
!     global matrices maximal number of elements (size)
  integer :: mtg
  parameter (mtg = 500)
!
!     input parameters
  integer :: mxm
  parameter (mxm = 9)
!
!     local matrix
  real(lrk) :: wk(mtg,mtg)
!
!     bearings
!
!     initializes return to stop program
  ok = -1
!
!     zero matrix
  call zermat_r(wk,dm,dm,mtg,mtg)
!
!     assembles amplied matrices
  do ii = 1,nbrg
!       search for bearing position
    kk = inddis(pc(ii),errmsg,oki)
    if (oki .gt. 0) then
      jj = ifidxof(kk,0)
!         stiffness
      ll = jj+1
      nn = jj+2
!         xx
      wk(ll,ll) = rk
!         zz
      wk(nn,nn) = rk
    else
!         not found, return error signal
      return
    end if
  end do
!
!     ok return
  ok = 0
!
  return
!
end subroutine prmkmr
!
!     ==================================================================
!>    @brief assembly big stiffness matrix, angular.
!
!>    @param[in] dm matrix dimension
!>    @param[in] ka angular stiffness value
!>    @param[in] par array with bearing stiffness and damping
!>    @param[out] wk return big stiffness matrix
!>    @param[out] errmsg return an error message
!>    @param[out] ok return flag. unsuccessful if <0.
!
subroutine prmkmra(dm,ka,par,wk,errmsg,ok)
  use com_ymc, only: nbrg, rks, pc
  use rd_kinds, only: lrk
  implicit none
!
!     input parameters
  integer :: mxm
  parameter (mxm = 9)
!
!     arguments
  integer :: dm, ok
  real(lrk) :: ka
!     k11 k12 k21 k22 c11 c12 c21 c22 [kph kth]
  real(lrk) :: par(mxm,10)
  character(len=99) :: errmsg
!
!     locals
  integer :: ii, jj, kk, ll, nn
  integer :: oki, inddis, ifidxof
!
!     global matrices maximal number of elements (size)
  integer :: mtg
  parameter (mtg = 500)
!
!     local matrix
  real(lrk) :: wk(mtg,mtg)
!
!     bearings
!
!     initializes return to stop program
  ok = -1
!
!     zero matrix
  call zermat_r(wk,dm,dm,mtg,mtg)
!
  jj = 0
!     assembles amplied stiffness matrix
  do ii = 1,nbrg
    jj = jj+1
!       search for bearing position
    kk = inddis(pc(ii),errmsg,oki)
    if (oki .gt. 0) then
!         stiffness matrix index x,z,phi,theta
!         |kxx 0   0   0  |   |k11 0   0   0  |
!         |0   kzz 0   0  |   |0   k22 0   0  |
!         |0   0   kph 0  | = |0   0   k33 0  |
!         |0   0   0   kth|   |0   0   0   k44|
      ll = ifidxof(kk,0)
      nn = ll+1
!         kxx
      wk(nn,nn) = par(jj,1)
      nn = ll+2
!         kzz
      wk(nn,nn) = par(jj,4)
!         angular stiffness symmetric values kph = kth = ka
      nn = ll+3
      wk(nn,nn) = ka
      nn = ll+4
      wk(nn,nn) = ka
    else
!         not found, return error signal
      return
    end if
  end do
!
!     ok return
  ok = 0
!
  return
!
end subroutine prmkmra
!
!     ==================================================================
!>    @brief undamped critical speed map.
!
!>    @param[in] pp number of effective bearings with support
!>    @param[in] std use standard input output
!>    @param[in] plt HPGL plot output
!>    @param[in] prv true if bearing variable parameters
!>    @param[out] errmsg return an error message
!>    @param[out] ok return flag. unsuccessful if <0.
!
subroutine maprigs(pp,std,plt,prv,errmsg,ok)
  use rd_textfun, only: femsgf, fomsgf
  use com_cpb, only: nini, nfin, dw, npi, ncc, imt
  use com_cpba, only: nrdc, rgini, rgrpm
  use com_cpbn, only: spdn
  use com_mat, only: mm, mg, dg => dm, smn
  use com_mbk, only: mkb
  use rd_kinds, only: lrk, wp
  implicit none
!
!     arguments
  integer :: pp, ok
  character(len=99) :: errmsg
  logical :: std, plt, prv
!
!     locals
  character(len=1) :: blank
  character(len=3) :: cbd
  character(len=7) :: nmm
  character(len=18) :: cf
  real(lrk) :: rk, sk, ml, rf, rad, spd, spf, flo, fhi, yv, xv, oxv, yi, ndw, intlag, rpm2radf, refine_rad
  integer :: i, j, k, l, m, p, dm, qq, oki, oo, ic, mxk, ncand, ptotal, chklamf, foundation_ndof_f, map_refines
  complex(wp) :: j0
  logical :: go, gyr, mok, rpeqf
  dimension cbd(2)
!     mxk stiffness maximum
!     spd speed divisions at the last crtitcal speed -> ncc
!     spf speed factor at the last crtitcal speed -> ncc
  parameter (blank = ' ',cbd = (/'mtg','mxk'/),nmm = 'maprigs',cf = '(f5.2,''E'',i2,8a)',mxk = 50,spd = 15,spf = &
    & 1.15_lrk,j0 = (0._wp,0._wp))
!
!     global matrices maximum number of elements (size)
  integer :: mtg
  parameter (mtg = 500)
!
!     global matrices: mass mm, gyroscopic mg, stiffness mkb
!
!     base stiffness matrix
!
!     campbell
  integer :: mxc
  parameter (mxc = 19)
!     nominal speed (rpm), added francisco oct-20
!
!
!     internals dimension
!
  integer :: n, nmax, mxm, avp, mxa
  parameter (nmax = 2*mtg,mxm = 9,avp = 6,mxa = mxc+avp)
!     par -> k11 k12 k21 k22 c11 c12 c21 c22 [kph kth]
  real(lrk) :: mk2, rcr, fn, vr, ome, ym, par, cps, oyv
  complex(wp) :: aa, bb, avl, fi, psi, pvec, pbase
  real(wp) :: qmac
  integer :: sel
!
  dimension&
  & mk2(mtg,mtg),rcr(mxc,mxk),fn(mtg,mtg),&
  &vr(mxk),ome(mtg),ym(mtg),par(mxm,10),cps(2),&
  &aa(nmax,nmax),bb(nmax,nmax),avl(nmax),oyv(mxc),&
  &fi(nmax,nmax),psi(nmax,nmax),&
  &pvec(nmax,mxc),pbase(nmax,mxc),&
  &qmac(mxc),sel(mxc)
!     Campbell speed limit extension, under and over
  parameter (cps = (/0.5_lrk,1.5_lrk/))
!
!     dcmplx -> double complex intrinsic
  intrinsic :: abs, int, log10, mod, real, cmplx
!
!     return init
  ok = -1
!     check number of critical speeds
  if (ncc .le. 0) ncc = 4
!     check number of interpolated points
  if (npi .le. 0) npi = 50
!
!     matrices dimension without supports
  dm = dg-2*pp-foundation_ndof_f()
!     global matrix dimension
  n = 2*dm
!     check number of desired critical speeds
  if (dm .lt. ncc) ncc = dm
!     use the same physical-family tracking contract as Campbell
  mok = imt .ne. 0
!
!     parameters check
!
!     speed delta
  if (dw .le. 0) then
!       invalid delta rpm
    errmsg = femsgf(99, nmm,6,4,0)
    return
  end if
!     system speed
  if (rgrpm .lt. 0) then
!       invalid speed
    errmsg = femsgf(99, nmm,6,11,0)
    return
  end if
!     check rgini <= 0
  if (rgini .le. 0) then
!       invalid speed
    errmsg = femsgf(99, nmm,6,11,0)
    return
  end if
!
!     just one speed gyr = false, system at that speed
!     system speed = zero will update each speed, gyr = true
  gyr = rpeqf(rgrpm,0._lrk,1e-6_lrk)
!
!     begin stiffness loop
  p = 1
  !    decade counter
  qq = 1
!     stiffness initial increment
  sk = 1
!     bearing initial stiffness
  rk = 1
!
  i = int(log10(rgini))
!     initial multiplier
  ml = 10**i
!     final stiffness
  rf = ml*(10**nrdc)
!
!     check parameters
  if (rf .le. ml) then
!       final stiffnes <= initial
    errmsg = femsgf(99, nmm,6,12,0)
    return
  end if
!
!     One point at 10^i and nine new points per additional decade.
  ptotal=1+9*nrdc
  call progress_begin('CRITICAL_SPEED_MAP',ptotal,&
  &'solve eigenproblems across bearing stiffness')
  call progress_stage('CRITICAL_SPEED_MAP',&
  &'solve critical speeds for each stiffness')
!
!     clears critical speed matrix
  call zermat_r(rcr,mxc,mxk,mxc,mxk)
!
!     prepares constant part matrix bb, constant part aa
  do i = 1,dm
    l = i+dm
    do j = 1,dm
      m = j+dm
!         constant part bb
      bb(i,m) = j0
      bb(l,j) = j0
      bb(l,m) = cmplx(-mm(i,j),0, kind=wp)
!         constant part aa
      aa(i,m) = cmplx(mm(i,j),0, kind=wp)
      aa(l,j) = cmplx(mm(i,j),0, kind=wp)
      aa(l,m) = j0
    end do
  end do
!
!     stiffness loop
!
  do while (qq .le. nrdc)
!
!       screen stiffness, backspace
    if (.not. std) call bprint(rk,int(log10(ml)),cf,8)
!
!       keeps the declared/output stiffness grid unchanged.  F05-HF04
!       may solve extra stiffness points only to preserve modal identity.
    vr(p) = rk*ml
    if (mok .and. p .gt. 1) then
      if (.not. gyr) then
        rad = rpm2radf(rgrpm)
      else
        rad = 1E-9_lrk
      end if
      call maprig_refine_transverse_stiffness(dm,n,ncc,rad,vr(p-1),vr(p),&
      &aa,bb,fi,psi,avl,mk2,pbase,sel,qmac,errmsg,oki)
      if (oki .lt. 0) then
        call progress_end('CRITICAL_SPEED_MAP','FAILED')
        return
      end if
    end if
!
!       full bearing stiffness matrix->mk2
    call prmkmr(dm,vr(p),mk2,errmsg,oki)
    if (oki .lt. 0) then
      call progress_end('CRITICAL_SPEED_MAP','FAILED')
      return
    end if
!
!       add matrix mkb+mk2->mk2
    call sommat_r(mkb,mk2,mk2,dm,dm,mtg,mtg)
!
!       operational speed rad/s
    if (.not. gyr) then
!         rpm -> rad/s
      rad = rpm2radf(rgrpm)
    else
!         gyroscopic loop start speed
!         todo check this value
      rad = 1E-9_lrk
    end if
!
!       prepares variable matrix bb
    do i = 1,dm
      do j = 1,dm
        bb(i,j) = cmplx(mk2(i,j),0, kind=wp)
      end do
    end do
!
!       omega loop counter
    oo = 1
    map_refines = 0
!       campbell calculation loop
    go = .true.
!
!       gyroscopic loop
    do while(go)
!         speed vector
      ome(oo) = rad
!
!         prepares variable matrix aa
      do i = 1,dm
        do j = 1,dm
          aa(i,j) = cmplx(mg(i,j)*rad,0, kind=wp)
        end do
      end do
!
!         solve eigenvalues problem->avl
      if (mok) then
        call adjeig(aa,bb,fi,psi,avl,n,nmax,nmax,.false.,&
        &errmsg,oki)
      else
        call adjeig2(aa,bb,avl,n,nmax,errmsg,oki)
      end if
      if (oki .lt. 0) then
        call progress_end('CRITICAL_SPEED_MAP','FAILED')
        return
      end if
!
!         Track both dimensions of this map. At the first speed of each
!         stiffness point, pbase continues modal identity across stiffne
!         The subsequent gyroscopic sweep continues from pvec across spe
      if (mok) then
        ncand = ncc+avp
        if (ncand .gt. dm) ncand = dm
        if (ncand .gt. mxa) ncand = mxa
        if (oo .eq. 1) then
          call mactrack(fi,avl,pbase,sel,qmac,n,nmax,ncc,&
          &ncand,mxc,mxa,p .eq. 1,errmsg,oki)
          if (oki .lt. 0) then
            call progress_end('CRITICAL_SPEED_MAP','FAILED')
            return
          end if
          do l = 1,ncc
            do i = 1,n
              pvec(i,l) = pbase(i,l)
            end do
          end do
        else
          call mactrack(fi,avl,pvec,sel,qmac,n,nmax,ncc,&
          &ncand,mxc,mxa,.false.,errmsg,oki)
          if (oki .eq. -2) then
            if (rad .le. ome(oo-1) .or. map_refines .ge. 32) then
              errmsg = 'critical map: no admissible speed refinement step'
              call progress_end('CRITICAL_SPEED_MAP','FAILED')
              return
            end if
            refine_rad = ome(oo-1)+0.5_lrk*(rad-ome(oo-1))
            if (refine_rad .le. ome(oo-1) .or. refine_rad .ge. rad) then
              errmsg = 'critical map: no representable speed refinement step'
              call progress_end('CRITICAL_SPEED_MAP','FAILED')
              return
            end if
            map_refines = map_refines+1
            write(*,'(a,3(1x,es24.16e3),1x,i0)') &
            &'RD_AUDIT_V1 MAP_TRACK_REFINE_SPEED',ome(oo-1),rad,refine_rad,map_refines
            rad = refine_rad
            cycle
          else if (oki .lt. 0) then
            call progress_end('CRITICAL_SPEED_MAP','FAILED')
            return
          end if
        end if
        map_refines = 0
      end if
!
!         frequencies
      do l = 1,ncc
        if (mok) then
          m = sel(l)
        else
!             legacy mode identification by instantaneous frequency rank
          m = chklamf(avl,l,n,nmax)
          if (m .lt. 1) then
            errmsg = 'critical map: oscillatory mode unavailable'
            call progress_end('CRITICAL_SPEED_MAP','FAILED')
            return
          end if
        end if
!           frequency (abs, rad/s)
        fn(oo,l) = real(abs(avl(m)), lrk)
!           used for single speed
        rcr(l,p) = fn(oo,l)
      end do
!
!         next speed
!         increase system speed up
      flo = fn(oo,1)
      fhi = fn(oo,1)
      do l = 2,ncc
        if (fn(oo,l) .lt. flo) flo = fn(oo,l)
        if (fn(oo,l) .gt. fhi) fhi = fn(oo,l)
      end do
      rad = rad+2*fhi/spd
!         check last calculated frequency and gyr status
      go = rad/spf .lt. fhi .and. gyr
!
      oo = oo+1
!         ome vector size check
      if (oo .gt. mtg .and. go) then
        errmsg = fomsgf(99, nmm,4,cbd(1),0)
        call progress_end('CRITICAL_SPEED_MAP','FAILED')
        return
      end if
!         end gyroscopic loop
    end do
!
!       find 1x critical speeds
    if (gyr) then
!         number of calculated frequencies
      j = oo-1
!         check for at least two freq to interpolate
      if (j .lt. 2) then
        errmsg = fomsgf(99, nmm,7,blank,0)
        call progress_end('CRITICAL_SPEED_MAP','FAILED')
        return
      end if
!
!         interpolation
!         zero old interpolated nat. frequencies -> oyv
      call zervec_r(oyv,mxc,mxc)
      flo = fn(1,1)
      do k = 2,ncc
        if (fn(1,k) .lt. flo) flo = fn(1,k)
      end do
      fhi = fn(j,1)
      do k = 2,ncc
        if (fn(j,k) .gt. fhi) fhi = fn(j,k)
      end do
!         first speed
      nini = flo*cps(1)
!         last speed
      nfin = fhi*cps(2)
!         speed steps
      ndw = (nfin-nini)/(npi-1)
!         critical speed counter
      ic = 0
!         intial speed
      oxv = 0._lrk
!         initial speed interpolation
      xv = nini
!
!         interpolated points, search intersection 1x
      do l = 1,npi
!           loop number of desired critical speeds
!           find critical speeds
        do k = 1,ncc
!
!             vector of calc frequencies
          do m = 1,j
            ym(m) = fn(m,k)
          end do
!
!             Lagrange interpolation speed xv -> yv
          yv = intlag(ome,ym,xv,j,mtg,oki,errmsg)
!             check interpolation status
          if (oki .lt. 0) then
            call progress_end('CRITICAL_SPEED_MAP','FAILED')
            return
          end if
!
!             check for 1st point
          if (l .gt. 1) then
!               search for intersection line y=x
            if (yv .le. xv .and. oyv(k) .ge. oxv) then
              call f1camp(oxv,oyv(k),xv,yv,1._lrk,rad,yi)
!                 keeps critical speed
              rcr(k,p) = rad
              ic = ic+1
            end if
          end if
!             last speed/frequency
          oyv(k) = yv
!
!             end loop critical frequencies
        end do
!           check if all found
        if (ic .eq. ncc) then
!             all found, exit speed interpolation loop
          exit
        end if
!
!           last speed
        oxv = xv
!           next speed
        xv = xv+ndw
!
!           end loop interpolation
      end do
!
!         for each stiffness ncc speeds must be calculated
      if (ic .ne. ncc) then
        errmsg = fomsgf(99, nmm,19,blank,0)
        call progress_end('CRITICAL_SPEED_MAP','FAILED')
        return
      end if
!
!         gyroscopic if
    end if
!
!       completed stiffness point
    call progress_update('CRITICAL_SPEED_MAP',p,ptotal,vr(p),&
    &'K_N_PER_M')
!
!       next stiffness
    p = p+1
!
!       check dimension
    if (p .gt. mxk) then
      errmsg = fomsgf(99, nmm,4,cbd(2),0)
      call progress_end('CRITICAL_SPEED_MAP','FAILED')
      return
    end if
!
!       checks increment
    if (mod(rk,10._lrk) .eq. 0) then
      rk = 1
      sk = 1
      qq = qq+1
!         decadic
      ml = ml*10
    end if
!
    rk = rk+sk
!       0 was sk
    sk = 1
!
!       stiffness loop end
  end do
!
!     remove last p increment
!     number of stiffness nk
  j = p-1
!     output critical speed map, will set ok
  call progress_stage('CRITICAL_SPEED_MAP',&
  &'write critical-speed map')
  call s_maprig(j,ncc,rgrpm,spdn,vr,rcr,par,mxk,mxc,.false.,&
  &std,plt,prv,errmsg,ok)
  if (ok .lt. 0) then
    call progress_end('CRITICAL_SPEED_MAP','FAILED')
    return
  end if
  call progress_end('CRITICAL_SPEED_MAP','OK')
!
  return
!
!     sub end
!
end subroutine maprigs
!
!     ==================================================================
!>    @brief undamped angular critical speed map.
!>    displacement stiffness constant, angular variable.
!
!>    @param[in] pp number of efective bearings
!>    @param[in] pv bearing speed dependent param flag
!>    @param[in] std use standard input output
!>    @param[in] plt HPGL plot output
!>    @param[out] errmsg return an error message
!>    @param[out] ok return flag. unsuccessful if <0.
!
subroutine mapriga(pp,pv,std,plt,errmsg,ok)
  use rd_textfun, only: femsgf, fomsgf
  use com_cpb, only: nini, nfin, dw, npi, ncc, imt
  use com_cpba, only: nrdc, rgini, rgrpm
  use com_mat, only: mm, mg, dg => dm, smn
  use com_mbk, only: mkb
  use rd_kinds, only: lrk, wp
  implicit none
!
!     arguments
  integer :: pp, ok
  character(len=99) :: errmsg
  logical :: pv, std, plt
!
!     locals
  character(len=1) :: blank
  character(len=3) :: cbd
  character(len=7) :: nmm
  character(len=18) :: cf
  real(lrk) :: rk, sk, ml, rf, rad, spd, spf, flo, fhi, yv, xv, oxv, yi, ndw, intlag, rpm2radf, refine_rad
  integer :: i, j, k, l, m, p, dm, qq, oki, oo, ic, nbr, mxk, ncand, ptotal, chklamf, foundation_ndof_f, map_refines
  complex(wp) :: j0
  logical :: go, gyr, mok
  dimension cbd(2)
!
!     mxk stiffness maximum
!     spd speed divisions at the last crtitcal speed -> ncc
!     spf speed factor at the last crtitcal speed -> ncc
  parameter (blank = ' ', nmm = 'mapriga',cf = '(f5.2,''E'',i2,8a)',cbd = (/'mtg','mxk'/),mxk = 50,spd = 15._lrk,spf &
    & = 1.15_lrk,j0 = (0._wp,0._wp))
!
!     global matrices maximum number of elements (size)
  integer :: mtg
  parameter (mtg = 500)
!
!     global mass mm, stiffness mk matrices
!     gyroscopic mg
!
!     base stiffness matrix
!
!     campbell
  integer :: mxc
  parameter (mxc = 19)
!
!     mxm input parameter
  integer :: n, nmax, mxm, avp, mxa
  parameter (nmax = 2*mtg,mxm = 9,avp = 6,mxa = mxc+avp)
!     internals dimension
  complex(wp) :: aa, bb, avl, fi, psi, pvec, pbase
  real(wp) :: qmac
  integer :: sel
!     par -> k11 k12 k21 k22 c11 c12 c21 c22 [kph kth]
  real(lrk) :: mk2, rcr, fn, vr, ome, ym, par, cps, oyv
!
  dimension&
  & mk2(mtg,mtg),rcr(mxc,mxk),fn(mtg,mtg),&
  &vr(mxk),ome(mtg),ym(mtg),par(mxm,10),cps(2),&
  &aa(nmax,nmax),bb(nmax,nmax),avl(nmax),oyv(mxc),&
  &fi(nmax,nmax),psi(nmax,nmax),&
  &pvec(nmax,mxc),pbase(nmax,mxc),&
  &qmac(mxc),sel(mxc)
!     Campbell speed limit extension, under and over
  parameter (cps = (/0.5_lrk,1.5_lrk/))
!
!     dcmplx -> double complex intrinsic
  intrinsic :: abs, int, log10, mod, real, cmplx
!
!     init
!
!     return init
  ok = -1
!     compiler may not accept paramter

!     check number of critical speeds
  if (ncc .le. 0) ncc = 4
!     check number of interpolated points
  if (npi .le. 0) npi = 50
!     matrices dimension without supports
  dm = dg-2*pp-foundation_ndof_f()
!     global matrix dimension
  n = 2*dm
!     check number of desired critical speeds
  if (dm .lt. ncc) ncc = dm
!     use the same physical-family tracking contract as Campbell
  mok = imt .ne. 0
!
!     parameters check
!
!     system speed
  if (rgrpm .lt. 0) then
!       invalid speed
    errmsg = femsgf(99, nmm,6,5,0)
    return
  end if
!     check rgini <= 0
  if (rgini .le. 0) then
!       invalid speed
! TODO: sartori - erro de tipo, deve ser arrumado
!       errmsg = fomsgf(nmm,6,11,0)
    errmsg = fomsgf(99, nmm,6,'TODO 11',0)
    return
  end if
!
!     gyroscopic
  gyr = .true.
!
!     begin stiffness loop
  p = 1
  !    decade counter
  qq = 1
!     stiffness initial increment
  sk = 1
!     bearing initial stiffness
  rk = 1
  i = int(log10(rgini))
!     initial multiplier
  ml = 10**i
!     final stiffness
  rf = ml*(10**nrdc)
!
!     check parameters
  if (rf .le. ml) then
!       final stiffnes <= initial
    errmsg = femsgf(99, nmm,6,12,0)
    return
  end if
!
  ptotal=1+9*nrdc
  call progress_begin('ANGULAR_CRITICAL_SPEED_MAP',ptotal,&
  &'solve eigenproblems across angular stiffness')
  call progress_stage('ANGULAR_CRITICAL_SPEED_MAP',&
  &'solve critical speeds for each angular stiffness')
!
!     bearing parameters for a given speed -> par,
!     zero allowed only for constant stiffness bearing
!     rpm -> rad/s
  rad = rpm2radf(rgrpm)
  call parman(rad,pv,mxm,nbr,par,errmsg,oki)
  if (oki .lt. 0) then
    call progress_end('ANGULAR_CRITICAL_SPEED_MAP','FAILED')
    return
  end if
!
!     clears critical speed matrix _> rcr
  call zermat_r(rcr,mxc,mxk,mxc,mxk)
!
!     prepares constant part matrix bb, constant part aa
  do i=1,dm
    l = i+dm
    do j=1,dm
      m = j+dm
!         constant part bb
      bb(i,m) = j0
      bb(l,j) = j0
      bb(l,m) = cmplx(-mm(i,j),0, kind=wp)
!         constant part aa
      aa(i,m) = cmplx(mm(i,j),0, kind=wp)
      aa(l,j) = cmplx(mm(i,j),0, kind=wp)
      aa(l,m) = j0
    end do
  end do
!
!     stiffness loop
!
  do while (qq .le. nrdc)
!
!       screen stiffness, backspace
    if (.not. std) call bprint(rk,int(log10(ml)),cf,8)
!
!       angular stiffness loop begin
!
!       keeps the declared/output angular-stiffness grid unchanged. F05-HF04
!       may solve extra stiffness points only to preserve modal identity.
    vr(p) = rk*ml
    if (mok .and. p .gt. 1) then
      rad = 1E-9_lrk
      call maprig_refine_angular_stiffness(dm,n,ncc,rad,vr(p-1),vr(p),par,&
      &aa,bb,fi,psi,avl,mk2,pbase,sel,qmac,errmsg,oki)
      if (oki .lt. 0) then
        call progress_end('ANGULAR_CRITICAL_SPEED_MAP','FAILED')
        return
      end if
    end if
!
!       full bearing stiffness matrix->mk2
    call prmkmra(dg,vr(p),par,mk2,errmsg,oki)
    if (oki .lt. 0) then
      call progress_end('ANGULAR_CRITICAL_SPEED_MAP','FAILED')
      return
    end if
!
!       add matrix mkb+mk2->mk2
    call sommat_r(mkb,mk2,mk2,dm,dm,mtg,mtg)
!
!       campbell calculation loop
    go = .true.
!
!       gyroscopic loop start speed
!
!       todo check this
!
    rad = 1E-9_lrk
!
!       prepares variable part matrix bb
    do i = 1,dm
      do j = 1,dm
        bb(i,j) = cmplx(mk2(i,j),0, kind=wp)
      end do
    end do
!
!       omega loop counter
    oo = 1
    map_refines = 0
!       gyroscopic loop
    do while(go)
!         speed vector
      ome(oo) = rad
!
!         prepares variable part matrix aa
      do i = 1,dm
        do j = 1,dm
          aa(i,j) = cmplx((mg(i,j)*rad),0, kind=wp)
        end do
      end do
!
!         solve eigenvalues problem->avl
      if (mok) then
        call adjeig(aa,bb,fi,psi,avl,n,nmax,nmax,.false.,&
        &errmsg,oki)
      else
        call adjeig2(aa,bb,avl,n,nmax,errmsg,oki)
      end if
      if (oki .lt. 0) then
        call progress_end('ANGULAR_CRITICAL_SPEED_MAP','FAILED')
        return
      end if
!
!         Track across angular-stiffness points and across each speed sw
      if (mok) then
        ncand = ncc+avp
        if (ncand .gt. dm) ncand = dm
        if (ncand .gt. mxa) ncand = mxa
        if (oo .eq. 1) then
          call mactrack(fi,avl,pbase,sel,qmac,n,nmax,ncc,&
          &ncand,mxc,mxa,p .eq. 1,errmsg,oki)
          if (oki .lt. 0) then
            call progress_end('ANGULAR_CRITICAL_SPEED_MAP','FAILED')
            return
          end if
          do l = 1,ncc
            do i = 1,n
              pvec(i,l) = pbase(i,l)
            end do
          end do
        else
          call mactrack(fi,avl,pvec,sel,qmac,n,nmax,ncc,&
          &ncand,mxc,mxa,.false.,errmsg,oki)
          if (oki .eq. -2) then
            if (rad .le. ome(oo-1) .or. map_refines .ge. 32) then
              errmsg = 'angular critical map: no admissible speed refinement step'
              call progress_end('ANGULAR_CRITICAL_SPEED_MAP','FAILED')
              return
            end if
            refine_rad = ome(oo-1)+0.5_lrk*(rad-ome(oo-1))
            if (refine_rad .le. ome(oo-1) .or. refine_rad .ge. rad) then
              errmsg = 'angular critical map: no representable speed refinement step'
              call progress_end('ANGULAR_CRITICAL_SPEED_MAP','FAILED')
              return
            end if
            map_refines = map_refines+1
            write(*,'(a,3(1x,es24.16e3),1x,i0)') &
            &'RD_AUDIT_V1 MAP_TRACK_REFINE_SPEED_ANGULAR',ome(oo-1),rad,refine_rad,map_refines
            rad = refine_rad
            cycle
          else if (oki .lt. 0) then
            call progress_end('ANGULAR_CRITICAL_SPEED_MAP','FAILED')
            return
          end if
        end if
        map_refines = 0
      end if
!
!         frequencies
      do l = 1,ncc
        if (mok) then
          m = sel(l)
        else
          m = chklamf(avl,l,n,nmax)
          if (m .lt. 1) then
            errmsg = 'critical map: oscillatory mode unavailable'
            call progress_end('ANGULAR_CRITICAL_SPEED_MAP','FAILED')
            return
          end if
        end if
!           frequency (abs, rad/s)
        fn(oo,l) = real(abs(avl(m)), lrk)
!           used for single speed
        rcr(l,p) = fn(oo,l)
      end do
!
!         next speed
!         increase system speed up
      flo = fn(oo,1)
      fhi = fn(oo,1)
      do l = 2,ncc
        if (fn(oo,l) .lt. flo) flo = fn(oo,l)
        if (fn(oo,l) .gt. fhi) fhi = fn(oo,l)
      end do
      rad = rad+2*fhi/spd
!         check last calculated frequency and gyr status
      go = rad/spf .lt. fhi .and. gyr
!
      oo = oo+1
!         ome vector size check
      if (oo .gt. mtg .and. go) then
        errmsg = fomsgf(99, nmm,4,cbd(1),0)
        call progress_end('ANGULAR_CRITICAL_SPEED_MAP','FAILED')
        return
      end if
!         end gyroscopic loop
    end do
!
!       find 1x critical speeds
    if (gyr) then
!         number of calculated frequencies
      j = oo - 1
!         check for at least two freq to interpolate
      if (j .lt. 2) then
        errmsg = fomsgf(99, nmm,7,blank,0)
        call progress_end('ANGULAR_CRITICAL_SPEED_MAP','FAILED')
        return
      end if
!
!         interpolation
!         zero old interpolated nat. frequencies -> oyv
      call zervec_r(oyv,mxc,mxc)
      flo = fn(1,1)
      do k = 2,ncc
        if (fn(1,k) .lt. flo) flo = fn(1,k)
      end do
      fhi = fn(j,1)
      do k = 2,ncc
        if (fn(j,k) .gt. fhi) fhi = fn(j,k)
      end do
!         first speed
      nini = flo*cps(1)
!         last speed
      nfin = fhi*cps(2)
!         speed steps
      ndw = (nfin-nini)/(npi-1)
!         critical speed counter
      ic = 0
!         intial speed
      oxv = 0._lrk
!         initial speed interpolation
      xv = nini
!
!         interpolated points, search intersection 1x
      do l = 1,npi
!           loop number of desired critical speeds
!           find critical speeds
        do k = 1,ncc
!
!             vector of calc frequencies
          do m = 1,j
            ym(m) = fn(m,k)
          end do

!             Lagrange interpolation
          yv = intlag(ome,ym,xv,j,mtg,oki,errmsg)
!             check interpolation status
          if (oki .lt. 0) then
            call progress_end('ANGULAR_CRITICAL_SPEED_MAP','FAILED')
            return
          end if
!
!             check for 1st point
          if (l .gt. 1) then
!               search for intersection line y=x
            if (yv .le. xv .and. oyv(k) .ge. oxv) then
              call f1camp(oxv,oyv(k),xv,yv,1._lrk,rad,yi)
!                 keeps critical speed
              rcr(k,p) = rad
              ic = ic+1
            end if
          end if
!           end loop critical frequencies
          oyv(k) = yv
!
        end do
!           check if all found
        if (ic .eq. ncc) then
!             all found, exit speed interpolation loop
          exit
        end if
!
!           last speed
        oxv = xv
!           next speed
        xv = xv+ndw
!
!           end loop interpolation
      end do
!
      !       for each stiffness ncc speeds must be calculated
      if (ic .ne. ncc) then
        errmsg = fomsgf(99, nmm,19,blank,0)
        call progress_end('ANGULAR_CRITICAL_SPEED_MAP','FAILED')
        return
      end if
!
!         gyroscopic if
    end if
!
!       completed angular-stiffness point
    call progress_update('ANGULAR_CRITICAL_SPEED_MAP',p,ptotal,&
    &vr(p),'K_NM_PER_RAD')
!
!       next stiffness
    p = p+1
!
!       check dimension
    if (p .gt. mxk) then
      errmsg = fomsgf(99, nmm,4,cbd(2),0)
      call progress_end('ANGULAR_CRITICAL_SPEED_MAP','FAILED')
      return
    end if
!
!       checks increment
    if (mod(rk,10._lrk) .eq. 0) then
      rk = 1
      sk = 1
      qq = qq+1
      ml = ml*10
    end if
!
    rk = rk+sk
    sk = 1
!
!       stiffness loop end
  end do
!
!     remove last p increment
  j = p-1
!
!     output critical speed map
  call progress_stage('ANGULAR_CRITICAL_SPEED_MAP',&
  &'write angular critical-speed map')
  call s_maprig(j,ncc,rgrpm,0._lrk,vr,rcr,par,mxk,mxc,.true.,std,plt,pv,errmsg,ok)
  if (ok .lt. 0) then
    call progress_end('ANGULAR_CRITICAL_SPEED_MAP','FAILED')
    return
  end if
  call progress_end('ANGULAR_CRITICAL_SPEED_MAP','OK')
!
  return
!
end subroutine mapriga
!

!     ==================================================================
!>    @brief Internal F05/HF04 continuation across translational map stiffness.
!>    Intermediate stiffnesses never become user/output map points.
!
subroutine maprig_refine_transverse_stiffness(dm,n,ncc,rad,kleft,kright,&
&aa,bb,fi,psi,avl,mk2,pbase,sel,qmac,errmsg,ok)
  use com_mat, only: mm, mg
  use com_mbk, only: mkb
  use rd_kinds, only: lrk, wp
  implicit none
  integer, parameter :: mtg=500,nmax=2*mtg,mxc=19,avp=6,mxa=mxc+avp
  integer, parameter :: max_refines=32
  integer, intent(in) :: dm,n,ncc
  real(lrk), intent(in) :: rad,kleft,kright
  complex(wp), intent(inout) :: aa(nmax,nmax),bb(nmax,nmax)
  complex(wp), intent(inout) :: fi(nmax,nmax),psi(nmax,nmax),avl(nmax)
  real(lrk), intent(inout) :: mk2(mtg,mtg)
  complex(wp), intent(inout) :: pbase(nmax,mxc)
  integer, intent(out) :: sel(mxc)
  real(wp), intent(out) :: qmac(mxc)
  character(len=99), intent(out) :: errmsg
  integer, intent(out) :: ok
  real(lrk) :: left,trial,mid
  complex(wp), parameter :: j0=(0._wp,0._wp)
  integer :: i,j,l,m,info,ncand,refines
  logical :: target_trial

  ok=-1
  if(dm<1 .or. n/=2*dm .or. ncc<1 .or. ncc>mxc) then
    errmsg='critical map: invalid continuation dimensions'
    return
  end if
  if(kleft<=0._lrk .or. kright<=kleft) then
    errmsg='critical map: stiffness continuation requires increasing positive K'
    return
  end if
  left=kleft
  trial=kright
  target_trial=.true.
  refines=0
  do
    call prmkmr(dm,trial,mk2,errmsg,info)
    if(info<0) return
    call sommat_r(mkb,mk2,mk2,dm,dm,mtg,mtg)
    do i=1,dm
      l=i+dm
      do j=1,dm
        m=j+dm
        aa(i,j)=cmplx(mg(i,j)*rad,0._lrk,kind=wp)
        aa(i,m)=cmplx(mm(i,j),0._lrk,kind=wp)
        aa(l,j)=cmplx(mm(i,j),0._lrk,kind=wp)
        aa(l,m)=j0
        bb(i,j)=cmplx(mk2(i,j),0._lrk,kind=wp)
        bb(i,m)=j0
        bb(l,j)=j0
        bb(l,m)=cmplx(-mm(i,j),0._lrk,kind=wp)
      end do
    end do
    call adjeig(aa,bb,fi,psi,avl,n,nmax,nmax,.false.,errmsg,info)
    if(info<0) return
    ncand=min(mxa,dm,ncc+avp)
    call mactrack(fi,avl,pbase,sel,qmac,n,nmax,ncc,ncand,mxc,mxa,&
    &.false.,errmsg,info)
    if(info==0) then
      if(target_trial) then
        ok=0
        return
      end if
      left=trial
      trial=kright
      target_trial=.true.
      cycle
    end if
    if(info/=-2) return
    if(refines>=max_refines) then
      errmsg='critical map: stiffness refinement limit reached'
      return
    end if
    mid=sqrt(left)*sqrt(trial)
    if(mid<=left .or. mid>=trial) then
      errmsg='critical map: no representable stiffness refinement step'
      return
    end if
    refines=refines+1
    target_trial=.false.
    write(*,'(a,4(1x,es24.16e3),1x,i0)') &
    &'RD_AUDIT_V1 MAP_TRACK_REFINE_STIFFNESS',left,trial,mid,rad,refines
    trial=mid
  end do
end subroutine maprig_refine_transverse_stiffness

!     ==================================================================
!>    @brief Internal F05/HF04 continuation across angular map stiffness.
!
subroutine maprig_refine_angular_stiffness(dm,n,ncc,rad,kleft,kright,par,&
&aa,bb,fi,psi,avl,mk2,pbase,sel,qmac,errmsg,ok)
  use com_mat, only: mm, mg, dg => dm
  use com_mbk, only: mkb
  use rd_kinds, only: lrk, wp
  implicit none
  integer, parameter :: mtg=500,nmax=2*mtg,mxc=19,mxm=9,avp=6,mxa=mxc+avp
  integer, parameter :: max_refines=32
  integer, intent(in) :: dm,n,ncc
  real(lrk), intent(in) :: rad,kleft,kright,par(mxm,10)
  complex(wp), intent(inout) :: aa(nmax,nmax),bb(nmax,nmax)
  complex(wp), intent(inout) :: fi(nmax,nmax),psi(nmax,nmax),avl(nmax)
  real(lrk), intent(inout) :: mk2(mtg,mtg)
  complex(wp), intent(inout) :: pbase(nmax,mxc)
  integer, intent(out) :: sel(mxc)
  real(wp), intent(out) :: qmac(mxc)
  character(len=99), intent(out) :: errmsg
  integer, intent(out) :: ok
  real(lrk) :: left,trial,mid
  complex(wp), parameter :: j0=(0._wp,0._wp)
  integer :: i,j,l,m,info,ncand,refines
  logical :: target_trial

  ok=-1
  if(dm<1 .or. n/=2*dm .or. ncc<1 .or. ncc>mxc) then
    errmsg='angular critical map: invalid continuation dimensions'
    return
  end if
  if(kleft<=0._lrk .or. kright<=kleft) then
    errmsg='angular critical map: stiffness continuation requires increasing positive K'
    return
  end if
  left=kleft
  trial=kright
  target_trial=.true.
  refines=0
  do
    call prmkmra(dg,trial,par,mk2,errmsg,info)
    if(info<0) return
    call sommat_r(mkb,mk2,mk2,dm,dm,mtg,mtg)
    do i=1,dm
      l=i+dm
      do j=1,dm
        m=j+dm
        aa(i,j)=cmplx(mg(i,j)*rad,0._lrk,kind=wp)
        aa(i,m)=cmplx(mm(i,j),0._lrk,kind=wp)
        aa(l,j)=cmplx(mm(i,j),0._lrk,kind=wp)
        aa(l,m)=j0
        bb(i,j)=cmplx(mk2(i,j),0._lrk,kind=wp)
        bb(i,m)=j0
        bb(l,j)=j0
        bb(l,m)=cmplx(-mm(i,j),0._lrk,kind=wp)
      end do
    end do
    call adjeig(aa,bb,fi,psi,avl,n,nmax,nmax,.false.,errmsg,info)
    if(info<0) return
    ncand=min(mxa,dm,ncc+avp)
    call mactrack(fi,avl,pbase,sel,qmac,n,nmax,ncc,ncand,mxc,mxa,&
    &.false.,errmsg,info)
    if(info==0) then
      if(target_trial) then
        ok=0
        return
      end if
      left=trial
      trial=kright
      target_trial=.true.
      cycle
    end if
    if(info/=-2) return
    if(refines>=max_refines) then
      errmsg='angular critical map: stiffness refinement limit reached'
      return
    end if
    mid=sqrt(left)*sqrt(trial)
    if(mid<=left .or. mid>=trial) then
      errmsg='angular critical map: no representable stiffness refinement step'
      return
    end if
    refines=refines+1
    target_trial=.false.
    write(*,'(a,4(1x,es24.16e3),1x,i0)') &
    &'RD_AUDIT_V1 MAP_TRACK_REFINE_STIFFNESS_ANGULAR',left,trial,mid,rad,refines
    trial=mid
  end do
end subroutine maprig_refine_angular_stiffness
