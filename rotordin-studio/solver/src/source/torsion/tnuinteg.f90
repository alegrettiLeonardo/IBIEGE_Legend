!     $Id$
!     ==================================================================
!
!>    @file tnuinteg.f
!>    @author francisco
!>    @date 06-feb-20
!>    @brief torsion numerical time integration routines, last changes:<
!>     new file - feb-20.
!
!     ==================================================================
!>    @brief generate result time vector
!
!>    @param[in] telap elapsed intergration time (s)
!>    @param[in] rsst result time step (s), if zero use load time steps
!>    @param[in] tload time load steps matrix (s)
!>    @param[in] ttrdsz number of time load steps
!>    @param[in] mxttrd input time torque matrix rows (time) dimension
!>    @param[in] mxttra input time torque matrix columns (positions) dim
!>    @param[in] mxttrs output time torque matrix columns (positions) di
!>    @param[out] nrstps number of result time step
!>    @param[out] restm  result time step vector
!>    @param[out] timev input time vector
!>    @param[out] ok return flag. unsuccessful if <0.
!
subroutine rtsteps(telap,rsst,tload,&
&ttrdsz,mxttrd,mxttra,mxttrs,nrstps,restm,timev,ok)
  use rd_kinds, only: lrk
  implicit none
!
  integer :: ttrdsz, nrstps, mxttrd, mxttra, mxttrs, ok
  real(lrk) :: telap, rsst, tload, restm, timev
  dimension tload(mxttrd,mxttra+1),restm(mxttrs),timev(mxttrd)
!
  integer :: ii
  real(lrk) :: tt
!
  intrinsic :: nint
!
!     initialize return
  ok = -1
!
!     extract input time vector
  do ii = 1,ttrdsz
    timev(ii) = tload(ii,1)
  end do
!
  if (rsst .ge. 0) then
!       number of result time steps
    nrstps = nint(telap/rsst)+1
!       check bounds
    if (nrstps .gt. mxttrs) return
!       result time step vector
!       first time
    tt = tload(1,1)
    restm(1) = tt
    do ii = 2,nrstps
      tt = tt+rsst
      restm (ii) = tt
    end do
  else
    nrstps = ttrdsz
!       result time step vector
!       copy time load steps
    do ii = 1,nrstps
      restm(ii) = tload(ii,1)
    end do
  end if
!
!     return ok
  ok = 0
!
  return
!
end subroutine rtsteps
!
!     ==================================================================
!     @brief SPLINE integrpolation.
!>     use: given an 1D array of X data and an array of Y data, both of
!>     length N, this routine computes the 2nd derivatives, Y2 at each
!>     X data point.<br>
!>     This routine called once, prior to using routine SPLINT, as a
!>     set up for using routine SPLINT, which performs the actual
!>      interpolation.<br>
!>    \verbatim
!>    IMPORTANT NOTE: the X data values in array X must be in ascending
!>    order or the interpolation will fail.
!>    \endverbatim
!
!>    @param[in] x x data array
!>    @param[in] y y data array
!>    @param[in] n number of array elements.
!>    @param[in] mxttra input time torque matrix columns (positions) dim
!>    @param[out] y2 y derivatives
subroutine splined(x,y,n,mxttra,y2)
  use rd_kinds, only: lrk, wp
  implicit none
!
  integer :: n, mxttra
  real(lrk) :: x, y
  real(wp) :: y2
  dimension x(mxttra),y(mxttra),y2(mxttra)
!
  character(len=7) :: nmm
  real(wp) :: p, u, sig, dl1, dl2, dl3, prec
  dimension u(n)
  parameter (nmm = 'splined',prec = 1e-12_lrk)
  integer :: i
!
  intrinsic :: abs
!
!     zero vector u
  call zervec_d(u,n,n)
!
!     store intermediate values of terms in the expansion series
  do i = 2,n-1
!       division deltas
    dl1 = x(i+1)-x(i-1)
    dl2 = x(i+1)-x(i)
    dl3 = x(i)-x(i-1)
!       near zero division, stop
    if (abs(dl1) .lt. prec .or. abs(dl2) .lt. prec .or.&
    &abs(dl3) .lt. prec) call elmsge(1,7,nmm)
!
    sig = dl3/dl1
    p = sig*y2(i-1)+2.0_lrk
    y2(i) = (sig-1.0_lrk)/p
    u(i) = (6._lrk*((y(i+1)-y(i))/dl2-(y(i)-y(i-1))/dl3)/dl1-sig*u(i-1))/p
  end do
!
!     compute the Y2 from the 2nd order expansion series
  do i = n-1,1,-1
    y2(i) = y2(i)*y2(i+1)+u(i)
  end do
!
  return
!
end subroutine splined
!
!     ==================================================================
!>    @brief prepare load matrix interpolation derivatives
!
!>    @param[in] timev input time vector
!>    @param[in] tload time load matrix [{t},{l1},{l2}...}] ntloads x nl
!>    @param[in] ttrdsz size of time vector
!>    @param[in] nlind number of load indices
!>    @param[in] mxttrd input time torque matrix rows (time) dimension
!>    @param[in] mxttra input time torque matrix columns (positions) dim
!>    @param[out] itdxv last interpolation interval indices
!>    @param[out] y2mat derivatives matrix [{d1},{d2},...}] ntloads x nl
!
subroutine lminterp(timev,tload,ttrdsz,&
&nlind,mxttrd,mxttra,itdxv,y2mat)
  use rd_kinds, only: lrk, wp
  implicit none
!
  integer :: ttrdsz, nlind, itdxv, mxttrd, mxttra
  real(lrk) :: timev, tload
  real(wp) :: y2mat
  dimension timev(mxttrd),tload(mxttrd,mxttra+1),&
  &y2mat(mxttrd,mxttra),itdxv(mxttra)
!
  integer :: ii, jj
  real(lrk) :: valuev
  real(wp) :: y2
  dimension valuev(mxttrd),y2(mxttrd)
!
!     prepare intepolation index vector
  do jj = 1,nlind
    itdxv(jj) = 1
  end do
!
  do jj = 1,nlind
    do ii = 1,ttrdsz
!         load value vector
      valuev(ii) = tload(ii,jj+1)
    end do
    y2(1) = 0._wp
    y2(ttrdsz) = 0._wp
!       get derivatives vector each load -> y2
    call splined(timev,valuev,ttrdsz,mxttrd,y2)
!       prepare intepolation derivatives matrix
    do ii = 1,ttrdsz
      y2mat(ii,jj) = y2(ii)
    end do
  end do
!
  return
!
end subroutine lminterp
!
!     ==================================================================
!     @brief initialize displacement, velocity vectors to zero
!
!>    @param[out] uo initial displacementes
!>    @param[out] vo initial velocities
!>    @param[in] dm size of structural matrices stiffness and mass
!>    @param[in] mtg structural matrices mm,kk,cc dimension
!
subroutine initval(uo,vo,dm,mtg)
  use rd_kinds, only: wp
  implicit none
!
  integer :: dm, mtg
  real(wp) :: uo, vo
  dimension uo(mtg),vo(mtg)
!
!     zero initial values

  call zervec_d(uo,dm,mtg)
  call zervec_d(vo,dm,mtg)
!
  return
!
end subroutine initval
!
!     ==================================================================
!     @brief assemble load vector
!
!>    @param[in] dm size of structural matrices
!>    @param[in] lind section indices to apply loads and initial values
!>     {id1,id2,...} nlind_ x 1
!>    @param[in] nlind number of load indices
!>    @param[in] loadv load dof load vector
!>    @param[in] mxttra input time torque matrix columns (positions)
!>     dimension
!>    @param[in] mtg structural matrices mm,kk,cc dimension
!>    @param[out] fo return load vector
!
subroutine loadvf(dm,lind,nlind,loadv,mxttra,mtg,fo)
  use rd_kinds, only: lrk, wp
  implicit none
!
  integer :: dm, lind, nlind, mxttra, mtg
  real(lrk) :: loadv
  real(wp) :: fo
  dimension lind(mxttra),loadv(mxttra),fo(mtg)
!
  integer :: jj, id
!
!     zeros -> fo
  call zervec_d(fo,dm,mtg)
!
  do jj = 1,nlind
    id = lind(jj)
    fo(id) =  loadv(jj)
  end do
!
  return
!
end subroutine loadvf
!
!     ==================================================================
!>    @brief calculate acceleration.
!>     a = 1/m * (f - k*x - c*v)
!
!>    @param[in] mm inertia matrix
!>    @param[in] kk stiffness matrix
!>    @param[in] cc viscous damping matrix
!>    @param[in] uo displacements vector
!>    @param[in] vo speed vector
!>    @param[in] fo force vector
!>    @param[in] dm size of structural matrices
!>    @param[in] mtg structural matrices mm,kk,cc dimension
!>    @param[out] wo acceleration vector
!>    @param[out] errmsg return an error message
!>    @param[out] ok return flag. unsuccessful if <0.
!
subroutine accel0f(mm,kk,cc,uo,vo,fo,dm,mtg,wo,errmsg,ok)
  use rd_textfun, only: fomsgf
  use rd_kinds, only: lrk, wp
  implicit none
!
  integer :: dm, mtg, ok
  real(wp) :: mm, kk, cc, uo, vo, fo, wo
  dimension mm(mtg,mtg),kk(mtg,mtg),cc(mtg,mtg),&
  &uo(mtg),vo(mtg),fo(mtg),wo(mtg)
  character(len=99) :: errmsg
!
  integer :: ipiv
  real(wp) :: mmd, ku, cv, dv
  dimension ipiv(mtg),mmd(mtg,mtg),&
  &ku(mtg,1),cv(mtg,1),dv(mtg,1)
!
  integer :: ii
  character(len=1) :: c1
  character(len=3) :: erc
  character(len=7) :: nmm
  parameter (c1 = 'N',nmm = 'accel0f')
!
  intrinsic :: real
!
!     initialize return
  ok = -1
!
!     copy torsion inertia matrix mm -> mmd
  call copmat_d(mm,mmd,dm,mtg,mtg)
!     LU -> mmd
  call dgetrf(dm,dm,mmd,mtg,ipiv,ii)
  if(ii .ne. 0) then
    write(erc,5) ii
    errmsg = fomsgf(99, nmm,28,erc,0)
    return
  end if
!
!     multiplies (kk * dv(uo)) -> ku
  call DGEMM (c1,c1, dm, 1, dm, real(1._lrk, wp), kk, mtg,uo, mtg, real(0._lrk, wp), ku, mtg)
!
!     multiplies (cc * dv(vo)) -> cv
  call DGEMM (c1,c1, dm, 1, dm, real(1._lrk, wp), cc, mtg,vo, mtg, real(0._lrk, wp), cv, mtg)
!
!     difference ku(kk*uo)-cv(cc*vo) -> ku
  call difmat_d(ku,cv,ku,dm,1,mtg,1)
!
!     difference cv(fo)-ku((kk*uo)-(cc*vo)) -> dv
  call difmat_d(fo,ku,dv,dm,1,mtg,1)
!
!     copy force vector to solution dv->wo
  call copv_dd(dv,wo,dm,mtg,mtg)
!     solve system inverse of mass times force -> wo
!     force dv = fo-(kk*uo)-(cc*vo)
!     lapack system solver -> wo
  call dgetrs(c1,dm,1,mmd,mtg,ipiv,wo,dm,ii)
  if(ii .ne. 0) then
    write(erc,5) ii
    errmsg = fomsgf(99, nmm,32,erc,0)
    return
  end if
!
  ok = 0
  return
!
5 format(i3)
!
end subroutine accel0f
!
!     ==================================================================
!>    @brief initialize result matrices u,du/dt,d2u/dt2 []nrstps x nrind
!
!>    @param[in] rind result indices vector
!>    @param[in] nrind number of result indices
!>    @param[in] uo initial displacements
!>    @param[in] vo initial velocities
!>    @param[in] wo initial accelerations
!>    @param[in] mxttrs output time torque matrix rows (time) dimension
!>    @param[in] mxo result vector dimension
!>    @param[in] mtg structural matrices mm,kk,cc dimension
!>    @param[out] resum displacements matrix [] nrstps x nrind
!>    @param[out] resvm velocities matrix [] nrstps x nrind
!>    @param[out] resam accelerations matrix [] nrstps x nrind
!
subroutine preprm(rind,nrind,uo,vo,wo,mxttrs,mxo,mtg,&
&resum,resvm,resam)
  use rd_kinds, only: lrk, wp
  implicit none
!
  integer :: rind, nrind, mxttrs, mxo, mtg
  real(lrk) :: resum, resvm, resam
  real(wp) :: uo, vo, wo
  dimension rind(mxo),uo(mtg),vo(mtg),wo(mtg),&
  &resum(mxttrs,mxo),resvm(mxttrs,mxo),resam(mxttrs,mxo)
  integer :: jj, id
!
  intrinsic :: real
!
!     initial results
!
  do jj = 1,nrind
!       result index
    id = rind(jj)
!       displacement
    resum(1,jj) = real(uo(id), lrk)
!       velocity
    resvm(1,jj) = real(vo(id), lrk)
!       acceleration
    resam(1,jj) = real(wo(id), lrk)
  end do
!
  return
!
end subroutine preprm
!
!     ==================================================================
!>    @brief integration constants
!
!>    @param[in] imt numerical integration method
!>     0 = Newmark 1 = Wilson-theta
!>    @param[in] intst integration time step (s)
!>    @param[out] a integration constans vector as per imt
!>    @param[out] tet Wilson - theta amplitude decay factor
!>    @param[in] mxc integration constans vector appropriate dimension
!
subroutine itegcte(imt,intst,a,tet,mxc)
  use rd_kinds, only: lrk, wp
  implicit none
!
  integer :: imt, mxc
  real(lrk) :: intst, tet
  real(wp) :: a
  dimension a(0:mxc)
!
  character(len=7) :: nmm
!     alfa,delta -> Newmark amplitude decay factor
!     theta -> Wilson - theta amplitude decay factor
  real(lrk) :: alfa, delta, theta
  parameter (nmm = 'itegcte',alfa = 0.254_lrk, delta = 0.5_lrk,theta = 1.4_lrk)
!
!     integration constants
  if (imt .eq. 0) then
!       check dimension stop if not enough
    if (mxc .lt. 5) call elmsge(1,4,nmm)
!       Newmark
    a(0) = 1/(alfa*intst**2)
    a(1) = 1*delta/(alfa*intst)
    a(2) = a(0)*intst
    a(3) = 1/(2*alfa)-1
    a(4) = 1*delta/alfa-1
    a(5) = 1*(delta/(2*alfa)-1)*intst
  else if (imt .eq. 1) then
    if (mxc .lt. 8) call elmsge(1,4,nmm)
!       Wilson - theta
    a(0) = 6/(theta*intst)**2
    a(1) = 3/(theta*intst)
    a(2) = 2*a(1)
    a(3) = theta*intst/2
    a(4) = a(0)/theta
    a(5) = -1*a(2)/theta
    a(6) = 1-3/theta
    a(7) = intst/2
    a(8) = intst**2/6
    tet = theta
  else
!       stop if invalid integration kind
    call elmsge(1,6,nmm)
  end if
!
  return
!
end subroutine itegcte
!
!     ==================================================================
!>    @brief calculate effective stiffness matrix.
!>     ke = kk+a0*mm+a1*cc
!
!>    @param[in] kk stiffness matrix [kk]dm x dm
!>    @param[in] mm inertia matrix [mm]dm x dm
!>    @param[in] cc viscous damping matrix [cc]dm x dm
!>    @param[in] a0 integration constant
!>    @param[in] a1 integration constant
!>    @param[in] dm size of structural matrices
!>    @param[in] mtg structural matrices mm,kk,cc dimension
!>    @param[out] ke effective stiffness matrix
!
subroutine effstif(kk,mm,cc,a0,a1,dm,mtg,ke)
  use rd_kinds, only: wp
  implicit none
!
  integer :: dm, mtg
  real(wp) :: kk, mm, cc, ke, a0, a1
  dimension kk(mtg,mtg),mm(mtg,mtg),cc(mtg,mtg),ke(mtg,mtg)
!
  real(wp) :: aom, a1c
  dimension aom(mtg,mtg),a1c(mtg,mtg)
!
!     a0*mm -> aom
  call escmat_d(mm,aom,a0,dm,dm,mtg,mtg)
!
!     a1*cc -> a1c
  call escmat_d(cc,a1c,a1,dm,dm,mtg,mtg)
!
!     a0*mm(aom)+a1*cc(a1c) -> aom
  call sommat_d(aom,a1c,aom,dm,dm,mtg,mtg)
!
!     kk+(a0*mm+a1*cc)(aom) -> ke
  call sommat_d(kk,aom,ke,dm,dm,mtg,mtg)
!
  return
!
end subroutine effstif
!
!     ==================================================================
!>    @brief two last time load buffer
!
!>    @param[in] tid integration time index
!>    @param[in] nlind number of load indices
!>    @param[in] loadv load dof load vector
!>    @param[in,out] lb load dof load vector buffer
!>    @param[in] mxttra input time torque matrix columns (positions) dim
!
subroutine ipldbf(tid,nlind,loadv,lb,mxttra)
  use rd_kinds, only: lrk
  implicit none
!
  integer :: tid, nlind, mxttra
  real(lrk) :: loadv, lb
  dimension loadv(mxttra),lb(2,mxttra)
!
  integer :: jj
!
  if (tid .gt. 2) then
!       after first time
    do jj = 1,nlind
      lb(1,jj) = lb(2,jj)
      lb(2,jj) = loadv(jj)
    end do
  else
!       first time
    do jj = 1,nlind
      lb(2,jj) = loadv(jj)
    end do
  end if
!
  return
!
end subroutine ipldbf
!
!     ==================================================================
!>    @brief two last time results buffer
!
!>    @param[in] tid integration time index
!>    @param[in] rind result indices
!>    @param[in] nrind number of result indices
!>    @param[in] uo last displacement result index 2
!>    @param[in] vo last velocity result index 2
!>    @param[in] wo last acceleration result index 2
!>    @param[in] mxo result vector dimension
!>    @param[in] mtg structural matrices mm,kk,cc dimension
!>    @param[in,out] ub displacement buffer
!>    @param[in,out] vb velocity buffer
!>    @param[in,out] wb acceleration buffer
!
subroutine resbuf(tid,rind,nrind,uo,vo,wo,mxo,mtg,ub,vb,wb)
  use rd_kinds, only: wp
  implicit none
!
  integer :: tid, rind, nrind, mxo, mtg
  real(wp) :: ub, vb, wb, uo, vo, wo
  dimension rind(mxo),uo(mtg),vo(mtg),wo(mtg),&
  &ub(2,mtg),vb(2,mtg),wb(2,mtg)
!
  integer :: jj, id
!
  if (tid .gt. 2) then
!       after first time
    do jj = 1,nrind
!         result index
      id = rind(jj)
!         displacement
      ub(1,jj) = ub(2,jj)
      ub(2,jj) = uo(id)
!         velocity
      vb(1,jj) = vb(2,jj)
      vb(2,jj) = vo(id)
!         acceleration
      wb(1,jj) = wb(2,jj)
      wb(2,jj) = wo(id)
    end do
  else
!       first time
    do jj = 1,nrind
!         result index
      id = rind(jj)
!         displacement
      ub(2,jj) = uo(id)
!         velocity
      vb(2,jj) = vo(id)
!         acceleration
      wb(2,jj) = wo(id)
    end do
  end if
!
  return
!
end subroutine resbuf
!
!     ==================================================================
!>    @brief  SPLINE interpolation.
!>     SPLINT use: given an 1D array of XA data, an array of YA
!>     data, and an array of the 2nd derivatives Y2A, all of length N,
!>     this routine performs cubic spline interpolation, returning the
!>     interpolated value Y at the user input value X.
!>     The Y2A are computed in routine SPLINE, which is called once
!>     before calling SPLINT.
!>    \verbatim
!>     IMPORTANT NOTE: the X data values in array X must be in ascending
!>     order or the interpolation will fail.
!>    \endverbatim
!>    @param[in] xa input x data array
!>    @param[in] ya input y data array
!>    @param[in] y2 2nd order derivatives
!>    @param[in] x input x to interpolate y
!>    @param[in] n number of input data array elements
!>    @param[in] mxd dimension of vectors
!>    @param[out] y interpolated output
!>    @param[in,out] klo index, speed search
!
subroutine splint(xa,ya,y2,x,n,mxd,y,klo)
  use rd_kinds, only: lrk, wp
  implicit none
!
  integer :: n, klo, mxd
  real(lrk) :: xa, ya, x, y
  real(wp) :: y2
  dimension xa(mxd),ya(mxd),y2(mxd)
!
  character(len=6) :: nmm
  logical :: rpeqf, xeq
  integer :: k, khi
  real(wp) :: dl, a, b, yd
  real(lrk) :: eps
  parameter (eps = 1e-12_lrk,nmm = 'splint')
!
  intrinsic :: abs, int, real
!
  k = 0
  khi = n
!     determine the indices of array XA that bracket the input X value
  do while (khi-klo .gt. 1)
    k = int((khi+klo)/2)
    if (xa(k) .gt. x) then
      khi = k
    else
      klo = k
    end if
  end do
!
!     determine the finite difference along the X dimension
!     true means both x are too close
  xeq = rpeqf(xa(khi),xa(klo),eps)
  if (xeq) then
!       stop if x are too close, invalid data
    call elmsge(1,8,nmm)
  else
    if (abs(xa(khi)-x) .lt. eps) then
      y = ya(khi)
    else if (abs(xa(klo)-x) .lt. eps) then
      y = ya(klo)
    else
      dl = xa(khi)-xa(klo)
!         interpolate
      a = (xa(khi)-x)/dl
      b = (x-xa(klo))/dl
      yd = a*ya(klo)+b*ya(khi)+((a**3-a)*y2(klo)+(b**3-b)*y2(khi))*(dl**2)/6._lrk
      y = real(yd, lrk)
    end if
!
  end if
!
!     start new search before
  klo = k-1
!
  return
!
end subroutine splint
!
!     ==================================================================
!>    @brief interpolate time loads
!
!>    @param[in] tper input time period
!>    @param[in] tt interpolation time
!>    @param[in] timev input time vector
!>    @param[in] y2mat time load derivatives matrix
!>    @param[in,out] itdxv intepolation index vector
!>    @param[in] tload time loads matrix, first column time
!>     [{t},{l1},{l2}...]ntloads x lint
!>    @param[in] ttrdsz size of time vector
!>    @param[in] nlind number of load indices
!>    @param[in] nper number of input time period
!>    @param[in] mxttrd input time torque matrix rows (time) dimension
!>    @param[in] mxttra input time torque matrix columns (positions) dim
!>    @param[out] loadv load vector interpolated to integration time ste
!
subroutine ldinterp(tper,tt,timev,y2mat,itdxv,tload,&
&ttrdsz,nlind,nper,mxttrd,mxttra,loadv)
  use rd_kinds, only: lrk, wp
  implicit none
!
  integer :: itdxv, ttrdsz, nlind, nper, mxttrd, mxttra
  real(lrk) :: tload, tper, tt, timev, loadv
  real(wp) :: y2mat
  dimension timev(mxttrd),itdxv(mxttra),tload(mxttrd,mxttra+1),&
  &y2mat(mxttrd,mxttra),loadv(mxttra)
!
  real(lrk) :: prec
!     precision
  parameter (prec = 5e-4_lrk)
!
  integer :: ii, jj, itper
  real(lrk) :: ttim, valuev, pert
  real(wp) :: y2vec
  dimension valuev(mxttrd),y2vec(mxttrd)
!
  intrinsic :: abs, int
!
!     period time
  pert = tt/tper
!     number of input time period
  itper = int(pert)
!
  if (itper .lt. 1) then
!       time is the same as input
    ttim = tt
  else
!       time is in next periods input time
    if (abs(tt-itper*tper) .le. prec) then
!         if exact period of time, belogns previous period
      itper = itper-1
    end if
!       input time to interpolate
    ttim = tt-(itper*tper)
    if (itper .ne. nper) then
!         next period of input time
!         init interpolation indices
      do ii = 1,nlind
        itdxv(ii) = 1
      end do
    end if
  end if
!
!     interpolate load on time
  do jj = 1,nlind
    do ii = 1,ttrdsz
!         extract time step load values
      valuev(ii) = tload(ii,jj+1)
!         extract derivatives
      y2vec(ii) = y2mat(ii,jj)
    end do
!      interpolate each load on time step
    call splint(timev,valuev,y2vec,ttim,ttrdsz,mxttrd,&
    &loadv(jj),itdxv(jj))
  end do
!
!     return period index
  nper = itper
!
  return
!
end subroutine ldinterp
!
!     ==================================================================
!>    @brief time force vector
!
!>    @param[in] mm inertia matrix [mm]dm x dm
!>    @param[in] cc viscous damping matrix [cc]dm x dm
!>    @param[in] uo initial displacements
!>    @param[in] vo initial velocities
!>    @param[in] wo initial accelerations
!>    @param[in] a integration constants vector
!>    @param[in] ff current force vector
!>    @param[in] fo previous force vector
!>    @param[out] fn calculated force vector
!>    @param[out] pp new force vector
!>    @param[in] tet Wilson-theta theta constant
!>    @param[in] imt numerical integration method
!>     0 = Newmark 1 = Wilson-theta
!>    @param[in] dm size of structural matrices
!>    @param[in] mtg structural matrices mm,kk,cc dimension
!>    @param[in] nci integration constants vector dimension
!
subroutine tforvec(mm,cc,uo,vo,wo,a,ff,fo,fn,pp,tet,imt,&
&dm,mtg,nci)
  use rd_kinds, only: lrk, wp
  implicit none
!
  integer :: imt, dm, mtg, nci
  real(lrk) :: tet
  real(wp) :: mm, cc, a, pp, uo, vo, wo, ff, fo, fn
  dimension mm(mtg,mtg),cc(mtg,mtg),uo(mtg),vo(mtg),wo(mtg),&
  &a(0:nci),ff(mtg),fo(mtg),fn(mtg),pp(mtg)
!
  character(len=1) :: c1
  parameter (c1 = 'N')
  integer :: jj
  real(wp) :: v1, v2, p1, p2
  dimension v1(mtg,1),v2(mtg,1),p1(mtg,1),p2(mtg,1)
!
  intrinsic :: real
!
  do jj = 1,dm
    if (imt .eq. 0) then
!         Newmark
      fn(jj) = ff(jj)
      v1(jj,1) = a(0)*uo(jj)+a(2)*vo(jj)+a(3)*wo(jj)
      v2(jj,1) = a(1)*uo(jj)+a(4)*vo(jj)+a(5)*wo(jj)
    else if (imt .eq. 1) then
!         Wilson-theta
      fn(jj) = fo(jj)+tet*(ff(jj)-fo(jj))
      v1(jj,1) = a(0)*uo(jj)+a(2)*vo(jj)+2*wo(jj)
      v2(jj,1) = a(1)*uo(jj)+2*vo(jj)+a(3)*wo(jj)
    end if
  end do
!
!     multiplies (mm * v1) -> p1
!      call DGEMM ('N','N', dm, 1, dm, dble(1.), mm, mtg,
  call DGEMM (c1,c1, dm, 1, dm, real(1._lrk, wp), mm, mtg,v1, mtg, real(0._lrk, wp), p1, mtg)
!
!     multiplies (cc * v2) -> p2
!      call DGEMM ('N','N', dm, 1, dm, dble(1.), cc, mtg,
  call DGEMM (c1,c1, dm, 1, dm, real(1._lrk, wp), cc, mtg,v2, mtg, real(0._lrk, wp), p2, mtg)
!
!     result force vector
  do jj = 1,dm
    pp(jj) = fn(jj)+p1(jj,1)+p2(jj,1)
  end do
!
  return
!
end subroutine tforvec
!
!     ==================================================================
!>    @brief new displacements, velocities and acceleration
!
!>    @param[in] uu calculated displacements
!>    @param[in] uo previous displacement
!>    @param[in] vo previous velocity
!>    @param[in] wo previous acceleration
!>    @param[out] un new displacement
!>    @param[out] vn new velocity
!>    @param[out] wn new acceleration
!>    @param[in] a integration constants vector
!>    @param[in] intst integration time step (s)
!>    @param[in] imt numerical integration method
!>     0 = Newmark 1 = Wilson-theta
!>    @param[in] dm size of structural matrices
!>    @param[in] mtg structural matrices mm,kk,cc dimension
!>    @param[in] nci integration constants vector dimension
!
subroutine newdisp(uu,uo,vo,wo,un,vn,wn,a,intst,&
&imt,dm,mtg,nci)
  use rd_kinds, only: lrk, wp
  implicit none
!
  integer :: imt, dm, mtg, nci
  real(lrk) :: intst
  real(wp) :: uu, a, uo, vo, wo, un, vn, wn
  dimension uu(mtg),uo(mtg),vo(mtg),wo(mtg),&
  &un(mtg),vn(mtg),wn(mtg),a(0:nci)
!
  integer :: jj
!
  do jj = 1,dm
    if (imt .eq. 0) then
!         Newmark
      un(jj) = uu(jj)
!         new acceleration
      wn(jj) = a(0)*(un(jj)-uo(jj))-a(2)*vo(jj)-a(3)*wo(jj)
!         new speed
      vn(jj) = a(1)*(un(jj)-uo(jj))-a(4)*vo(jj)-a(5)*wo(jj)
    else if (imt .eq. 1) then
!         Wilson-theta
      wn(jj) = a(4)*(uu(jj)-uo(jj))+a(5)*vo(jj)+a(6)*wo(jj)
      vn(jj) = vo(jj)+a(7)*(wn(jj)+wo(jj))
      un(jj) = uo(jj)+intst*vo(jj)+a(8)*(wn(jj)+2*wo(jj))
    end if
  end do
!
  return
!
end subroutine newdisp
!
!     ==================================================================
!>    @brief load time interpolation
!
!>    @param[in] rid result index
!>    @param[in] nlind number of load indices
!>    @param[in] tt time step
!>    @param[in] tx result time step
!>    @param[in] restm result time vector
!>    @param[in] lb load dof load vector buffer
!>    @param[in] loadv load dof load vector
!>    @param[out] iptld result input loads
!>    @param[in] mxttrs output time torque matrix rows (time) dimension
!>    @param[in] mxttra input time torque matrix columns (pos.) dimensio
!>    @param[in] mxo result vector dimension
!
subroutine ldint(rid,nlind,tt,tx,restm,lb,loadv,&
&iptld,mxttrs,mxttra,mxo)
  use rd_kinds, only: lrk, wp
  implicit none
!
  integer :: rid, nlind, mxttrs, mxttra, mxo
  real(lrk) :: tt, tx, restm, lb, loadv, iptld
  dimension restm(mxttrs),lb(2,mxttra),&
  &loadv(mxttra),iptld(mxttrs,mxo)
!
  real(lrk) :: prec
!     equals precision
  parameter (prec = 1E-8_lrk)
  integer :: jj
!     functions
  real(wp) :: lin2pf, a1f, a2f, b2f, f2f
!     local result vectors
  real(wp) :: x1, x2, x3, y1, y2, y3, a1, a2, b2, ld, dtx
!
  intrinsic :: real
!
!     double time for interpolation
  dtx = tt
!
!     load
  do jj = 1,nlind
!
    if (abs(tt-tx) .le. prec)then
!         no need to interpolate
      ld = loadv(jj)
    else
!         interpolate
      if (rid .lt. 3) then
!           linear two points interpolation
        x1 = restm(1)
        x2 = tt
!           loads
        y1 = lb(2,jj)
        y2 = loadv(jj)
        ld = lin2pf(x1,y1,x2,y2,dtx)
      else
!           cubic spline three points interpolation
        x1 = restm(rid-2)
        x2 = restm(rid-1)
        x3 = tt
!           loads
        y1 = lb(1,jj)
        y2 = lb(2,jj)
        y3 = loadv(jj)
        a1 = a1f(x1,x2,x3,y1,y2,y3)
        a2 = a2f(a1,x1,x2,x3)
        b2 = b2f(x1,x2,x3,y1,y2,y3)
        ld = f2f(a2,b2,x3,y3,dtx)
      end if
    end if
!
    iptld(rid,jj) = real(ld, lrk)
!
  end do
!
  return
!
end subroutine ldint
!
!     ==================================================================
!>    @brief interpolated results.
!>     if necessary output results is interpolated using three points
!>     spline interpolation
!
!>    @param[in] rid result index
!>    @param[in] rind section indices to get results {id1,id2,...} nrind
!>    @param[in] nrind number of result indices
!>    @param[in] tt time step
!>    @param[in] tx result time step
!>    @param[in] restm result time vector
!>    @param[in] ub last two displacement integration result buffer
!>    @param[in] vb last two velocity integration result buffer
!>    @param[in] wb last two acceleration integration result buffer
!>    @param[in] uo displacement integration result
!>    @param[in] vo velocity integration result
!>    @param[in] wo acceleration integration
!>    @param[in] mxttrs output time torque matrix rows (time) dimension
!>    @param[in] mxo result vector dimension
!>    @param[in] mtg structural matrices mm,kk,cc dimension
!>    @param[out] resum displacement vector {} nrind x 1
!>    @param[out] resvm resv  velocity vector {} nrind x 1
!>    @param[out] resam acceleration vector {} nrind x 1
!
subroutine rinterp(rid,rind,nrind,tt,tx,restm,&
&ub,vb,wb,uo,vo,wo,mxttrs,mxo,mtg,resum,resvm,resam)
  use rd_kinds, only: lrk, wp
  implicit none
!
  integer :: rid, rind, nrind, mxttrs, mxo, mtg
  real(lrk) :: tt, tx, restm, resum, resvm, resam
  real(wp) :: ub, vb, wb, uo, vo, wo
  dimension rind(mxo),restm(mxttrs),ub(2,mtg),vb(2,mtg),wb(2,mtg),&
  &uo(mtg),vo(mtg),wo(mtg),&
  &resum(mxttrs,mxo),resvm(mxttrs,mxo),resam(mxttrs,mxo)
!
  real(lrk) :: prec
!     equals precision
  parameter (prec = 1E-8_lrk)
!
  integer :: jj, id
!     functions
  real(wp) :: lin2pf, a1f, a2f, b2f, f2f
!     local result vectors
  real(wp) :: x1, x2, x3, y1, y2, y3, a1, a2, b2, ru, rv, ra, dtx
!
  intrinsic :: real
!
  dtx = tx
!     loop on result indices
  do jj = 1,nrind
!       get result id
    id = rind(jj)
!
    if (abs(tt-tx) .le. prec)then
!         no need to interpolate
      ru = uo(id)
      rv = vo(id)
      ra = wo(id)
    else
!         interpolate
      if (rid .lt. 3) then
!           linear two points interpolation
        x1 = restm(1)
        x2 = tt
!           displacement
        y1 = ub(2,jj)
        y2 = uo(id)
        ru = lin2pf(x1,y1,x2,y2,dtx)
!           velocity
        y1 = vb(2,jj)
        y2 = vo(id)
        rv = lin2pf(x1,y1,x2,y2,dtx)
!           acceleration
        y1 = wb(2,jj)
        y2 = wo(id)
        ra = lin2pf(x1,y1,x2,y2,dtx)
      else
!           cubic spline three points interpolation
        x1 = restm(rid-2)
        x2 = restm(rid-1)
        x3 = tt
!           displacement
        y1 = ub(1,jj)
        y2 = ub(2,jj)
        y3 = uo(id)
        a1 = a1f(x1,x2,x3,y1,y2,y3)
        a2 = a2f(a1,x1,x2,x3)
        b2 = b2f(x1,x2,x3,y1,y2,y3)
        ru = f2f(a2,b2,x3,y3,dtx)
!           velocity
        y1 = vb(1,jj)
        y2 = vb(2,jj)
        y3 = vo(id)
        a1 = a1f(x1,x2,x3,y1,y2,y3)
        a2 = a2f(a1,x1,x2,x3)
        b2 = b2f(x1,x2,x3,y1,y2,y3)
        rv = f2f(a2,b2,x3,y3,dtx)
!           acceleration
        y1 = wb(1,jj)
        y2 = wb(2,jj)
        y3 = wo(id)
        a1 = a1f(x1,x2,x3,y1,y2,y3)
        a2 = a2f(a1,x1,x2,x3)
        b2 = b2f(x1,x2,x3,y1,y2,y3)
        ra = f2f(a2,b2,x3,y3,dtx)
      end if
    end if
!
    resum(rid,jj) = real(ru, lrk)
    resvm(rid,jj) = real(rv, lrk)
    resam(rid,jj) = real(ra, lrk)
!
  end do
!
  return
!
end subroutine rinterp
!
!     ==================================================================
!>    @brief torsion numerical integration.
!>    returns angular displacement, velocities and accelerations
!
!>    @param[in] mm inertia matrix [mm]dm x dm
!>    @param[in] cc viscous damping matrix [cc]dm x dm
!>    @param[in] kk stiffness matrix [kk]dm x dm
!>    @param[in] dm size of structural matrices
!>    @param[in] ttrdsz size of time vector
!>    @param[in] tper input time period. used for periodic input
!>     shorter than integration time, same as integration instead.
!>    @param[in] tload time loads matrix, first column time
!>     [{t},{l1},{l2}...]ntloads x lint+1
!>    @param[in] lind section indices to apply loads and
!>     initial values {id1,id2,...} nlind x 1
!>    @param[in] nlind number of load indices
!>    @param[in] initit initial integration time (s)
!>    @param[in] endit final integration time (s)
!>    @param[in] intst integration time step (s)
!>    @param[in] imt numerical integration method
!>     0 = Newmark 1 = Wilson-theta
!>    @param[in] rind section indices to get results
!>     {id1,id2,...} nrind x 1
!>    @param[in] nrind number of result indices
!>    @param[in] rsst results time step (s)
!>    @param[in] nrstps number of result time vector
!>    @param[in] mtg structural matrices mm,kk,cc dimension
!>    @param[in] mxo result vector dimension
!>    @param[in] mxttrd input time torque matrix rows (time) dimension
!>    @param[in] mxttra input time torque matrix columns (pos.) dimensio
!>    @param[in] mxttrs output time torque matrix columns (positions) di
!>    @param[out] restm time matrix
!>    @param[out] resum displacement matrix
!>    @param[out] resvm result velocity matrix
!>    @param[out] resam result acceleration matrix
!>    @param[out] iptld result input loads
!>    @param[out] errmsg return an error message
!>    @param[out] ok return flag. unsuccessful if <0.
!
subroutine tnuinteg(mm,cc,kk,dm,ttrdsz,tper,tload,&
&lind,nlind,initit,endit,intst,imt,rind,nrind,&
&rsst,nrstps,restm,resum,resvm,resam,iptld,&
&mtg,mxo,mxttrd,mxttra,mxttrs,errmsg,ok)
  use rd_textfun, only: fomsgf
  use rd_kinds, only: lrk, wp
  implicit none
!
  integer :: dm, ttrdsz, lind, nlind, imt, rind, nrind, nrstps, mtg, mxo, mxttrd, mxttra, mxttrs, ok
  real(lrk) :: tper, tload, initit, endit, intst, rsst, restm, resum, resvm, resam, iptld
  real(wp) :: mm, cc, kk
  dimension mm(mtg,mtg),cc(mtg,mtg),kk(mtg,mtg),&
  &tload(mxttrd,mxttra+1),&
  &lind(mxttra),rind(mxo),&
!      results
  &restm(mxttrs),resum(mxttrs,mxo),&
  &resvm(mxttrs,mxo),resam(mxttrs,mxo),&
  &iptld(mxttrs,mxo)
  character(len=99) :: errmsg
!
  integer :: nci, jj, oki, itdxv, tid, rid, nper, ipiv, nistps
  real(lrk) :: prec, telap, timev, loadv, tet, tt, tx, lb
  parameter (nci = 8)
  real(wp) :: ke, pp, uu, a, uo, vo, wo, ub, vb, wb, fo, ff, fn, un, vn, wn, y2mat
!     equals precision
  parameter (prec = 5E-4_lrk)
!
  dimension ipiv(mtg),&
  &itdxv(mxttra),timev(mxttrd),y2mat(mxttrd,mxttra),&
  &uo(mtg),vo(mtg),loadv(mxttra),lb(2,mxttra),a(0:nci),&
  &fo(mtg),fn(mtg),wo(mtg),&
  &ke(mtg,mtg),ub(2,mtg),vb(2,mtg),wb(2,mtg),ff(mtg),&
  &pp(mtg),uu(mtg),un(mtg),vn(mtg),wn(mtg)
!
  character(len=1) :: c1
  character(len=3) :: erc
  character(len=6) :: cbd
  character(len=8) :: nmm
  parameter (c1 ='N',cbd = 'mxttrs',nmm = 'tnuinteg')
!
  intrinsic :: int, abs
!
!     initialize return
  ok = -1
!
!     elapsed integration time
  telap = endit-initit
!
!     number of integration steps
  nistps = int(telap/intst)+1
  call progress_begin('TORSION_TRANSIENT',nistps,&
  &'torsional numerical time integration')
  call progress_stage('TORSION_TRANSIENT',&
  &'prepare load interpolation and effective stiffness')
!
!     result time step vector {time} -> restm,timev
  call rtsteps(telap,rsst,tload,&
  &ttrdsz,mxttrd,mxttra,mxttrs,nrstps,restm,timev,oki)
!     check return, <0 time upper bound
  if (oki .lt. 0) then
    errmsg = fomsgf(99, nmm,4,cbd,0)
    return
  end if
!
!     prepare load matrix derivatives and intepolation index vector
!     -> y2mat,itdxv
  call lminterp(timev,tload,ttrdsz,&
  &nlind,mxttrd,mxttra,itdxv,y2mat)
!
!     zero initial values, angle and speed -> uo,vo
  call initval(uo,vo,dm,mtg)
!
!     extract first integration time load values -> loadv
  do jj = 1,nlind
    loadv(jj) = tload(1,jj+1)
  end do
!
!     initial force vector -> fo
  call loadvf(dm,lind,nlind,loadv,mxttra,mtg,fo)
!
!     initial accelerations -> wo
  call accel0f(mm,kk,cc,uo,vo,fo,dm,mtg,wo,errmsg,oki)
  if (oki .lt. 0) return
!
!     results matrices u,du/dt,d2u/dt2 [] nrstps x nrind
!     displacement, velocity, acceleration -> resum,resvm,resam
  call preprm(rind,nrind,uo,vo,wo,mxttrd,mxo,mtg,&
  &resum,resvm,resam)
!
!     integration constants -> a,tet
  call itegcte(imt,intst,a,tet,nci)
!
!     effective stiffness matrix -> ke
  call effstif(kk,mm,cc,a(0),a(1),dm,mtg,ke)
!
!     lapack solver effective stiffness matrix -> ke
  call dgetrf(dm,dm,ke,mtg,ipiv,jj)
  if(jj .ne. 0) then
    write(erc,5) jj
    errmsg = fomsgf(99, nmm,28,erc,0)
    return
  end if
!
!     integration time index
  tid = 1
!
!     initial time
  tt = initit
!
!     result time index
  rid = 2
!
!     buffer initial load values on index 2 - last -> lb
  call ipldbf(tid,nlind,loadv,lb,mxttra)
!
!     buffer initial values on index 2 - last -> ub,vb,wb
  call resbuf(tid,rind,nrind,uo,vo,wo,mxo,mtg,ub,vb,wb)
!
!     time period index
  nper = 0
!
!     time integration loop
  do while (abs(tt-endit) .gt. prec .and. rid .le. nrstps)
!
!       new time index
    tid = tid+1
!
!       new time step
    tt = tt+intst
    call progress_update('TORSION_TRANSIENT',min(tid,nistps),&
    &nistps,tt,'TIME_S')
!
!       interpolate load on time -> loadv
    call ldinterp(tper,tt,timev,y2mat,itdxv,tload,&
    &ttrdsz,nlind,nper,mxttrd,mxttra,loadv)

!
!       load vector -> ff
    call loadvf(dm,lind,nlind,loadv,mxttra,mtg,ff)
!
!       force vector -> pp
    call tforvec(mm,cc,uo,vo,wo,a,ff,fo,fn,pp,tet,imt,&
    &dm,mtg,nci)
!
!       copy force vector to solution pp->uu
    call copv_dd(pp,uu,dm,mtg,mtg)
!       solve new displacements pp/ke -> uu
!       lapack system solver -> uu
    call dgetrs(c1,dm,1,ke,mtg,ipiv,uu,dm,jj)
    if(jj .ne. 0) then
      write(erc,5) jj
      errmsg = fomsgf(99, nmm,32,erc,0)
      return
    end if
!
!       calculate new displacement -> un,vn,wn
    call newdisp(uu,uo,vo,wo,un,vn,wn,a,intst,&
    &imt,dm,mtg,nci)
!
!       hold on previous values
    do jj = 1,dm
!         force
      fo(jj) = fn(jj)
!         displacement
      uo(jj) = un(jj)
!         speed
      vo(jj) = vn(jj)
!         accel
      wo(jj) = wn(jj)
    end do
!
!       results buffer
    tx = restm(rid)
!
    if (abs(tt-tx) .le. prec) then
!         check bounds
      if (rid .gt. mxttrs) then
        errmsg = fomsgf(99, nmm,4,cbd,0)
        return
      end if
!         input load interpolation -> iptld
      call ldint(rid,nlind,tt,tx,restm,lb,loadv,&
      &iptld,mxttrs,mxttra,mxo)
!         interpolate results -> resum,resvm,resam
      call rinterp(rid,rind,nrind,tt,tx,restm,&
      &ub,vb,wb,uo,vo,wo,mxttrs,mxo,mtg,resum,resvm,resam)
!         result time index
      rid = rid+1
!
    end if
!
!       last two loads buffer -> lb
    call ipldbf(tid,nlind,loadv,lb,mxttra)
!
!       last two results buffer -> ub,vb,wb
    call resbuf(tid,rind,nrind,uo,vo,wo,mxo,mtg,ub,vb,wb)
!
!       time integration loop
  end do
!
!     return ok
  ok = 0
!     force an explicit 100% terminal update even when floating-point
!     time accumulation stops one nominal index short of nistps.
  call progress_update('TORSION_TRANSIENT',nistps,nistps,&
  &telap,'TIME_S')
  call progress_end('TORSION_TRANSIENT','OK')
!
  return
!
5 format(i3)
!
end subroutine tnuinteg
!
!     ==================================================================
!>    @brief torsion numerical integration.
!>    returns only angular displacement
!
!>    @param[in] mm inertia matrix [mm]dm x dm
!>    @param[in] cc viscous damping matrix [cc]dm x dm
!>    @param[in] kk stiffness matrix [kk]dm x dm
!>    @param[in] dm size of structural matrices
!>    @param[in] ttrdsz size of time vector
!>    @param[in] tper input time period. used for periodic input
!>     shorter than integration time, same as integration instead.
!>    @param[in] tload time loads matrix, first column time
!>     [{t},{l1},{l2}...]ntloads x lint+1
!>    @param[in] lind section indices to apply loads and
!>     initial values {id1,id2,...} nlind x 1
!>    @param[in] nlind number of load indices
!>    @param[in] initit initial integration time (s)
!>    @param[in] endit final integration time (s)
!>    @param[in] intst integration time step (s)
!>    @param[in] imt numerical integration method
!>     0 = Newmark 1 = Wilson-theta
!>    @param[in] rind section indices to get results
!>     {id1,id2,...} nrind x 1
!>    @param[in] nrind number of result indices
!>    @param[in] rsst results time step (s)
!>    @param[in] nrstps number of result time vector
!>    @param[in] iptld result input loads
!>    @param[in] mtg structural matrices mm,kk,cc dimension
!>    @param[in] mxo result vector dimension
!>    @param[in] mxttrd input time torque matrix rows (time) dimension
!>    @param[in] mxttra input time torque matrix columns (pos.) dimensio
!>    @param[in] mxttrs output time torque matrix columns (positions) di
!>    @param[out] restm time matrix
!>    @param[out] resum displacement matrix
!>    @param[out] errmsg return an error message
!>    @param[out] ok return flag. unsuccessful if <0.
!
subroutine tnuintegu(mm,cc,kk,dm,ttrdsz,tper,tload,&
&lind,nlind,initit,endit,intst,imt,rind,nrind,&
&rsst,nrstps,restm,resum,iptld,&
&mtg,mxo,mxttrd,mxttra,mxttrs,errmsg,ok)
  use rd_kinds, only: lrk, wp
  implicit none
!
  integer :: dm, ttrdsz, lind, nlind, imt, rind, nrind, nrstps, mtg, mxo, mxttrd, mxttra, mxttrs, ok
  real(lrk) :: tper, tload, initit, endit, intst, rsst, restm, resum, iptld
  real(wp) :: mm, cc, kk
  dimension mm(mtg,mtg),cc(mtg,mtg),kk(mtg,mtg),&
  &tload(mxttrd,mxttra+1),&
  &lind(mxttra),rind(mxo),&
!      results
  &restm(mxttrs),resum(mxttrs,mxo),iptld(mxttrs,mxo)
  character(len=99) :: errmsg
!
  real(lrk) :: resvm, resam
  dimension resvm(mxttrs,mxo),resam(mxttrs,mxo)
!
  call tnuinteg(mm,cc,kk,dm,ttrdsz,tper,tload,&
  &lind,nlind,initit,endit,intst,imt,rind,nrind,&
  &rsst,nrstps,restm,resum,resvm,resam,iptld,&
  &mtg,mxo,mxttrd,mxttra,mxttrs,errmsg,ok)
!
  return
!
end subroutine tnuintegu
!
