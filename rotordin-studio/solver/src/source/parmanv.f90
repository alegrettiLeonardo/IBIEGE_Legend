!     $Id$
!     ==================================================================
!
!>    @file parmanv.f
!>    @brief speed dependent bearing parameters, last changes:<br>
!>    new parameters interpolation - francisco - 03/12/2008<br>
!>    new matrix HH assembly for inversion - francisco - 03/12/2008<br>
!>    changed mntmth, acrescentado zermat_c  - francisco - 25/05/2009<br
!>    added prpkc,mntmth bearing plus support parameters - francisco - 0
!>    added support parameters as G Diana - francisco - 05/11/2010<br>
!>    moved matrix HH assembly to espmod.f - francisco - 20/12/2010<br>
!>    added rotational stiffness to bearing - francisco - oct-15<br>
!>    added bearing parameter scale sc - francisco - oct-15<br>
!>    added support parameter scale sps - francisco - nov-15<br>
!>    added parman get bearing stiffness and damping parameters - franci
!>    added central messages, conversion functions - francisco - apr-19<
!>    added torsion restriction check on bearing loops - francisco - jan
!>    moved beamasf from init.f - francisco - oct-20<br>
!>    added btmspf check largest or smallest bearinb table speed - franc
!>    added extrapolation limit on bearing table interpolation parman -
!>    moved beastiff from hplots - francisco - mar-21<br>
!>    added check for kind of variable bearing parameters function, isbr
!>    removed beamasf, no more needed - francisco - mar-21<br>
!>    added speed limits on variable parameters lmvpar - francisco - mar
!>    updated parmanf, removed unused arguments - francisco - sep-21<br>
!>    added eplwrg to hold extrap. warning index - francisco - sep-21<br
!>    changed isbrtbf, added return number of var.bearings - francisco -
!
!     ==================================================================
!>    @brief keep speed extrapolation warning message index.
!
!>    @param[in] iop zero for clear, greater than to hold warning,
!>     less than zero to get last warning message index, see messages.f.
!>    @param[out] oms get warning message or blank if no warning.
!
subroutine eplwrg(iop,oms)
  use rd_textfun, only: warnmsgf
  implicit none
!
  integer :: iop
  character(len=*) :: oms
!
  character(len=1) :: blank
  integer :: iwrl
  parameter (blank = ' ')
  data iwrl/0/
  save iwrl
!
  if (.not. iop .lt. 0) then
    iwrl = iop
  else
    if (iwrl .gt. 0) then
      oms = warnmsgf(50, -iwrl)
    else
      oms = blank
    end if
  end if
!
  return
!
end subroutine eplwrg
!
!     ==================================================================
!>    @brief get bearing tables largest or smallest speed.
!>     Check on used bearing parameters tables for minimum or maximum
!>     defined speed (rpm).
!
!>    @param[in] imx not zero for largest, zero for smallest.
!>    @return bearing tables largest or smallest speed.
!
real(lrk) function btmspf(imx)
  use com_nbc, only: cp, scl, bc
  use com_pmk, only: nt, kmc, cmc, rmc
  use com_ymc, only: nbrg, rks, pc
  use rd_kinds, only: lrk
  implicit none
!
  integer :: imx
!
  integer :: mpm, mxm
  parameter (mxm = 9, mpm = 99)
!     bearings
!
  real(lrk) :: rmn, riff
  logical :: ismx
  integer :: i, j, k, l, m, nf
!
  intrinsic :: nint
!
!     maximum/minimum value
  ismx = imx .ne. 0
  rmn = riff(ismx,1e-12_lrk,1e12_lrk)
!     bearing index
  j = 0
!     loop on static bearing
  do i = 1,nbrg
!       standard bearing index
    j = j+1
!       number of bearing tables
    do k = 1,cp
!         check for bearing number
      nf = nint(bc(k))
      if (j .eq. nf) then
!           table for static bearing index
!           number of parameter lines
        m = nt(k)
!           check speed loop
        do l = 1,m
!             check maximum/minimum value
          if (ismx) then
!               keep largest speed
            if (rmc(k,l) .gt. rmn) rmn = rmc(k,l)
          else
!               keep smallest speed
            if (rmc(k,l) .lt. rmn) rmn = rmc(k,l)
          end if
        end do
      end if
    end do
  end do
!
  btmspf = rmn
!
  return
!
end function btmspf
!
!     ==================================================================
!>    @brief check for kind of variable bearing parameters.
!
!>    @param[in,out] nvb if not nvb is less than zero, returns number
!>     of bearings with variable parameters else no meaning.
!>    @return true if parameters are table type "2",
!>     or false if equation coefficients, type "1"
!
logical function isbrtbf(nvb)
  use rd_textfun, only: femsgf
  use com_knd, only: knd => cknd, supr
  use com_nbc, only: cp, scl, bc
  use rd_kinds, only: lrk
  implicit none
!
  integer :: nvb
!
  logical :: isbrtb, iscfeq
  character(len=1) :: ckd
  character(len=7) :: nmm
  dimension ckd(3)
  character(len=99) :: errmsg
  parameter (ckd = (/'1','2','0'/),nmm = 'isbrtbf')
!
  integer :: mxm
  parameter (mxm = 9)
!     variable speed bearing parameters
!     kind of bearing calculation
!
!     equation coefficients
  iscfeq = knd .eq. ckd(1)
!
!     parameters table
  isbrtb = knd .eq. ckd(2)
!
!     check for one kind or another
  if ( (.not. (iscfeq .or. isbrtb)) .and.&
  &(knd .ne. ckd(3) .and. nvb .lt. 0) ) then
!       invalid kind, stop
!       this should never happens here...
    errmsg = femsgf(99, nmm,6,9,0)
    call lmsg(1,errmsg)
  end if
!     only if requested
  if (.not. nvb .lt. 0) nvb = cp
!
  isbrtbf = isbrtb
!
  return
!
end function isbrtbf
!
!     ==================================================================
!>    @brief bearing stiffness parameters, search for when the current
!>     stiffness is grather (or equal) than the bearing ones, then
!>     generates five interpolated speed points near to the
!>     correspondent current speed.
!
!>    @param[in] stiff current stiffness point
!>    @param[in] rpm current speed point
!>    @param[in] pu per unit points to interpolate vector
!>    @param[out] spd ni output speed points
!>    @param[out] xx ni output bearing kxx stiffness points
!>    @param[out] zz ni output bearing kzz stiffness points
!>    @param[out] bn number of bearings
!>    @param[in] mx output matrices first dimension, second is ni,
!>      should be equal to maximum bearing number, mxm parameter
!>    @param[in] ni number of pu points to interpolate, pu dimension
!>    @param[in] pv variable speed bearing parameters flag
!>    @return <code>true</code> if x and z bearing stiffness points
!>     are ok for all bearings.
!
logical function beastiff(stiff,rpm,pu,spd,xx,zz,bn,mx,ni,pv)
  use rd_textfun, only: femsgf
  use com_bstsv, only: lok
  use com_cfm, only: c11, c12, c21, c22, d11, d12, d21, d22, nl
  use com_knd, only: knd => cknd, supr
  use com_man, only: kxx, kxz, kzz, kzx, cxx, cxz, czz, czx, mm, sc
  use com_nbc, only: cp, scl, bc
  use com_pmk, only: nt, kmc, cmc, rmc
  use com_ymc, only: nbrg, rks, pc
  use rd_kinds, only: lrk
  implicit none
!
  real(lrk) :: stiff, rpm
  integer :: bn, mx, ni
  real(lrk) :: pu, spd, xx, zz
!     pu,speeds,interpolated stiffness x speed
  dimension pu(ni),spd(mx,ni),xx(mx,ni),zz(mx,ni)
  logical :: pv
!
  real(lrk) :: vl
  integer :: i, j
  character(len=99) :: errmsg
  character(len=8) :: nmm
  character(len=1) :: ckd
  dimension ckd(2)
  parameter (ckd = (/'1','2'/),nmm = 'beastiff')
!
!     max number of parameters lines
  integer :: mpm, mxm
  parameter (mpm = 99,mxm = 9)
!     speed variable bearing parameters block
!     variable speed bearing
!     fixed speed bearings
!     scale, sc added francisco - oct-15
!
!     kind of bearing calculation
!
  logical :: init, stok
!     functions
  real(lrk) :: intlag, ecoef
  real(lrk) :: r, a
  real(lrk) :: vx(mpm), vy(mpm)
!
!     saving state common
!
  integer :: ok, np
  character(len=99) :: ems
!
  data init/.false./
  save init
!
!     search for stiffness
!     clear flags and initialize return variables
  if (.not. init) then
    if (pv) then
!         number of berings with variable parameters
      bn = cp
    else
      bn = nbrg
    end if
!
    do i = 1,mx
!         found flags
      do j = 1,2
        lok(i,j) =.false.
      end do
!         return variables
      do j = 1,ni
        spd(i,j) = 0._lrk
        xx(i,j) = 0._lrk
        zz(i,j) = 0._lrk
      end do
    end do
!       just once
    init = .true.
  end if
!
  if (pv) then
!       speed dependent bearing parameters
    if (knd .eq. ckd(2)) then
!         table
!         number of bearings
      do i = 1,nbrg
!           check for torsion restriction bearing
!           x stiffness
        stok = stiff .ge. kmc(i,1,1)
        if (stok .and. (.not. lok(i,1))) then
          np = nt(i)
!             prepare interpolation vectors
          do j = 1,np
!               speed
            vx(j) = rmc(i,j)
!               x stiffness
            vy(j) = kmc(i,j,1)
          end do
!             interpolate
          do j = 1,ni
            spd(i,j) = rpm*pu(j)
            vl = intlag(vx,vy,spd(i,j),np,mpm,ok,ems)
            xx(i,j) = vl*scl(i)
          end do
          lok(i,1) = .true.
!             out of current do loop
          cycle
        end if
!           z stiffness
        stok = stiff .ge. kmc(i,1,4)
        if (stok .and. (.not. lok(i,2))) then
          np = nt(i)
          do j = 1,np
!               speed
            vx(j) = rmc(i,j)
!               z stiffness
            vy(j) = kmc(i,j,4)
          end do
          do j = 1,ni
            spd(i,j) = rpm*pu(j)
            vl = intlag(vx,vy,spd(i,j),np,mpm,ok,ems)
            zz(i,j) = vl*scl(i)
          end do
          lok(i,2) = .true.
          cycle
        end if
      end do
!
    else if (knd .eq. ckd(1)) then
!         inverse equation
      do i = 1,bn
!           check for torsion restriction bearing
!           x stiffness
        a = ecoef(c11,i,rpm)
        r = a*1/rpm*scl(i)
        stok = stiff .ge. pu(2)*r .and. stiff .le. pu(4)*r
        if (stok .and. .not. lok(i,1)) then
          do j = 1,ni
            r = rpm*pu(j)
            spd(i,j) = r
            a = ecoef(c11,i,r)
            vl = a*1/r
            xx(i,j) = vl*scl(i)
          end do
          lok(i,1) = .true.
          cycle
        end if
!           z stiffness
        a = ecoef(c22,i,rpm)
        r = a*1/rpm*scl(i)
        stok = stiff .ge. pu(2)*r .and. stiff .le. pu(4)*r
        if (stok .and. .not. lok(i,2)) then
          do j = 1,ni
            r = rpm*pu(j)
            spd(i,j) = r
            a = ecoef(c22,i,r)
            vl = a*1/r
            zz(i,j) = vl*scl(i)
          end do
          lok(i,2) = .true.
          cycle
        end if
      end do
!
    else
!         not expected kind
!         this should never happens here...
      errmsg = femsgf(99, nmm,6,9,0)
      call emsg(0,1,errmsg)
    end if
  else
!
!       fixed bearing parameters
    do i = 1,bn
!         x stiffness
      stok = stiff .ge. kxx(i)
      if (stok .and. .not. lok(i,1)) then
        do j = 1,ni
          spd(i,j) = rpm*pu(j)
          xx(i,j) = kxx(i)*sc
        end do
        lok(i,1) = .true.
        cycle
      end if
!         z stiffness
      stok = stiff .ge. kzz(i)
      if (stok .and. .not. lok(i,2)) then
        do j = 1,ni
          spd(i,j) = rpm*pu(j)
          zz(i,j) = kzz(i)*sc
        end do
        lok(i,2) = .true.
        cycle
      end if
    end do
!
  end if
!
!     ok if bn = 0
  stok = .true.
!     check if all bearing stiffness are ok
  do i = 1,bn
    stok = lok(i,1) .and. lok(i,2)
    if (.not. stok) exit
  end do
!
  beastiff = stok
!
  return
!
end function beastiff
!
!     ==================================================================
!>    @brief prepares the bearing whole matrices with support parameters
!
!>    @param[in] par vector with the stiffness and damping bearing
!>      parameters, k11 k12 k21 k22 c11 c12 c21 c22 [kph kth]
!>    @param[in] dm matrices dimension
!>    @param[in] pp number of effective bearings with support
!>    @param[out] wk returns  whole stiffness matrix
!>    @param[out] wc returns whole bearing damping matrix
!>    @param[out] errmsg return error message
!>    @param[out] ok return flag. unsuccessful if <0.
!
subroutine prpsp(par,dm,pp,wk,wc,errmsg,ok)
  use com_mtk, only: mk1
  use com_sp1, only: ns, bn
  use com_sp2, only: sup, sps
  use com_ymc, only: nbrg, rks, pc
  use rd_kinds, only: lrk, wp
  implicit none
!
!     locals
  integer :: ii, jj, k, kk, ll, oo, nn
  integer :: support_dof_x
  character(len=5) :: nmm
  character(len=9) :: dsc
  parameter (nmm = 'prpsp',dsc='brg. mass')
  integer :: oki, inddis, ifidxof
!
!     global matrix maximum number of elements
  integer :: mtg
  parameter (mtg = 500)
!     input parameters
  integer :: mxm
  parameter (mxm = 9)
!
!     arguments
  character(len=99) :: errmsg
  integer :: dm, pp, ok
!     k11 k12 k21 k22 c11 c12 c21 c22 [kph kth]
  real(lrk) :: par(mxm,10)
  complex(wp) :: wk(mtg,mtg), wc(mtg,mtg)
!
!     matrices block
!
!     base stiffness matrix
!
!     bearing block
!
!     support block
!     number of supports
!     bearing number
!     support data, see entrada.f, beasupf
!     1   2   4   3   5   6   8   7   9
!     kxx,kxz,kzz,kzx,cxx,cxz,czz,czx,mas
!     added scale sps - francisco - nov-15
!
!     par(i,1) = kxx(i)
!     par(i,2) = kxz(i)
!     par(i,3) = kzx(i)
!     par(i,4) = kzz(i)
!     par(i,5) = cxx(i)
!     par(i,6) = cxz(i)
!     par(i,7) = czx(i)
!     par(i,8) = czz(i)
!     rotational stiffness
!     par(i,9) = kph(i)
!     par(i,10) = kth(i)
!
!     support consideration
!
!     dinamica e vibrazioni delle macchine
!     dinamica dei rotori
!     Giorgio Diana
!
  ok = -1
!
!     suport index
  oo = 0
!
!     lateral bearing index
  k = 0
!     bearing loop
  do ii = 1,nbrg
!       ordinary bearing index
    k = k+1
!       support loop
    do jj = 1,ns
!         check if bearing has support
      if (bn(jj) .eq. k) then
!           seach bearing position index, same as the support
        kk = inddis(pc(k),errmsg,oki)
        if (oki .gt. 0) then
!
          oo = oo+1
!             matrix position index
          ll = ifidxof(kk,1)
!
!             explicit support DOF mapping; foundation DOFs are
!             located after all bearing-support DOFs.
          nn = support_dof_x(dm,pp,oo)
!
!             adds stiffness
!             line at the end x local column: - bearing
          wk((nn+0),(ll+0)) = -par(k,1)
          wk((nn+0),(ll+1)) = -par(k,2)
          wk((nn+1),(ll+0)) = -par(k,3)
          wk((nn+1),(ll+1)) = -par(k,4)
!
!             local line x column at the end: - bearing
          wk((ll+0),(nn+0)) = -par(k,1)
          wk((ll+0),(nn+1)) = -par(k,2)
          wk((ll+1),(nn+0)) = -par(k,3)
          wk((ll+1),(nn+1)) = -par(k,4)
!
!             adds stiffness: support + bearing
!             line at the end x column at the end
          wk((nn+0),(nn+0)) = sup(jj,1)*sps+par(k,1)
          wk((nn+0),(nn+1)) = sup(jj,2)*sps+par(k,2)
          wk((nn+1),(nn+0)) = sup(jj,3)*sps+par(k,3)
          wk((nn+1),(nn+1)) = sup(jj,4)*sps+par(k,4)
!
!             adds damping
!             line at the end x local column: - bearing
          wc((nn+0),(ll+0)) = -par(k,5)
          wc((nn+0),(ll+1)) = -par(k,6)
          wc((nn+1),(ll+0)) = -par(k,7)
          wc((nn+1),(ll+1)) = -par(k,8)
!
!             local line x column at the end: - bearing
          wc((ll+0),(nn+0)) = -par(k,5)
          wc((ll+0),(nn+1)) = -par(k,6)
          wc((ll+1),(nn+0)) = -par(k,7)
          wc((ll+1),(nn+1)) = -par(k,8)
!
!             adds damping: support + bearing
!             line at the end x column at the end
          wc((nn+0),(nn+0)) = sup(jj,5)*sps+par(k,5)
          wc((nn+0),(nn+1)) = sup(jj,6)*sps+par(k,6)
          wc((nn+1),(nn+0)) = sup(jj,7)*sps+par(k,7)
          wc((nn+1),(nn+1)) = sup(jj,8)*sps+par(k,8)
!
!             if this housing/support is mapped to FOUNDATION, the
!             support K/C acts between housing and foundation instead
!             of terminating only at rigid ground. This routine adds
!             the cross terms and the foundation-side diagonal terms.
          call foundation_add_support(jj,oo,dm,pp,nn,wk,wc,mtg)
!
        else
!             could not find bearing section index
          return
!             end if bearing position
        end if
!           end if support check
      end if
!
!         end support loop
    end do
!
!       end bearing loop
  end do
!
!     return ok
  ok = 0
!
  return
!
!     sub end
!
end subroutine prpsp
!
!     ==================================================================
!>    @brief speed limits on variable parameters
!
!>    @param[in] pv bearing speed dependent param flag
!>    @param[in] nini Campbell input initial speed (rpm)
!>    @param[in] nfin Campbell input final speed (rpm)
!>    @param[out] rpmi initial Campbell calculation speed (rpm)
!>    @param[out] rpmf final Campbell calculation speed (rpm)

subroutine lmvpar(pv,nini,nfin,rpmi,rpmf)
  use rd_kinds, only: lrk
  implicit none
  logical :: pv
  real(lrk) :: nini,nfin,rpmi,rpmf
  ! F06: preserve the requested range. PARMAN/PARMANT validate each actual
  ! operating point; an inverse law at zero is rejected, never shifted to 2%.
  rpmi=nini; rpmf=nfin
end subroutine lmvpar
!
!     ==================================================================
!>    @brief interpolates the bearing speed dependent table values.
!
!>    @param[in] rad speed (rad/s).
!>    @param[out] par output vector with bearing stiffness,
!>     damping parameters and addtional rotational stiffnes,
!>     k11 k12 k21 k22 c11 c12 c21 c22 [kph kth].
!>    @param[out] nbr number of lateral bearings, not torsion restrictio
!>    @param[out] errmsg return error message
!>    @param[out] ok return flag. unsuccessful if <0.
!
subroutine parmant(rad,par,nbr,errmsg,ok)
  use rd_kinds, only: lrk,wp
  use com_nbc, only: cp,scl,bc
  use com_pmk, only: nt,kmc,cmc,rmc
  use com_pmt, only: tph,tth
  use com_brgsm, only: mrp
  use com_ymc, only: nbrg
  use rd_bearing_contract, only: bearing_validate_table,bearing_speed_in_range,bearing_has_tilt
  use, intrinsic :: ieee_arithmetic, only: ieee_is_finite
  implicit none
  real(lrk) :: rad,par(9,10)
  integer :: nbr,ok,i,j,k,ns,oki,used(9)
  character(len=99) :: errmsg
  real(lrk) :: rpm,rpx(99),vyk(99),a,intlag
  real(wp) :: tilt(99,2)
  ok=-1; errmsg=''; nbr=nbrg; par=0._lrk; used=0
  if(nbrg<0.or.nbrg>9.or.cp<1.or.cp>9) then
    errmsg='parmant: invalid bearing/table count'; return
  end if
  if(.not.ieee_is_finite(rad).or.rad<0._lrk) then
    errmsg='parmant: expected finite nonnegative speed rad/s'; return
  end if
  rpm=rad*30._lrk/acos(-1._lrk)
  call parmanf(par,9)
  do j=1,cp
    i=bc(j); ns=nt(j)
    if(i<1.or.i>nbrg.or.ns<2.or.ns>99) then
      errmsg='parmant: table references an invalid bearing or row count'; return
    end if
    if(used(i)/=0) then
      errmsg='parmant: two tables target the same bearing'; return
    end if
    used(i)=j
    tilt(1:ns,1)=real(tph(j,1:ns),wp); tilt(1:ns,2)=real(tth(j,1:ns),wp)
    call bearing_validate_table(j,real(rmc(j,1:ns),wp),real(kmc(j,1:ns,1:4),wp), &
      real(cmc(j,1:ns,1:4),wp),tilt(1:ns,:),real(scl(j),wp),errmsg,oki)
    if(oki/=0) return
    call bearing_speed_in_range(real(rpm,wp),real(rmc(j,1),wp),real(rmc(j,ns),wp), &
      real(mrp,wp),errmsg,oki)
    if(oki/=0) return
    if(rpm<rmc(j,1).or.rpm>rmc(j,ns)) then
      write(*,'(a,1x,i0,3(1x,es24.16e3))') 'RD_AUDIT_V1 BEARING_EXTRAP',j,rpm,rmc(j,1),rmc(j,ns)
    end if
    rpx(1:ns)=rmc(j,1:ns)
    do k=1,4
      vyk(1:ns)=kmc(j,1:ns,k)
      a=intlag(rpx,vyk,rpm,ns,99,oki,errmsg); if(oki/=0) return
      par(i,k)=a*scl(j)
      vyk(1:ns)=cmc(j,1:ns,k)
      a=intlag(rpx,vyk,rpm,ns,99,oki,errmsg); if(oki/=0) return
      par(i,k+4)=a*scl(j)
    end do
    ! Historical absent-column behavior is zero; presence with zero values
    ! remains a real table and is interpolated, not dropped.
    par(i,9:10)=0._lrk
    if(bearing_has_tilt(j)) then
      vyk(1:ns)=tph(j,1:ns)
      a=intlag(rpx,vyk,rpm,ns,99,oki,errmsg); if(oki/=0) return
      par(i,9)=a*scl(j)
      vyk(1:ns)=tth(j,1:ns)
      a=intlag(rpx,vyk,rpm,ns,99,oki,errmsg); if(oki/=0) return
      par(i,10)=a*scl(j)
    end if
    if(.not.all(ieee_is_finite(par(i,:)))) then
      errmsg='parmant: interpolated/scaled coefficient is nonfinite'; return
    end if
    write(*,'(a,2(1x,i0),1x,es24.16e3,10(1x,es24.16e3))') &
      'RD_AUDIT_V1 BEARING_COEFFICIENTS',i,j,rpm,par(i,:)
  end do
  ok=0
end subroutine parmant
!
!     ==================================================================
!>    @brief calculates bearing parameter inverse coeficient value a
!>     for y=a*x^-1, a=sum(c(idx)*x^idx), idx=0,2.
!
!>    @param[in] c bearing parameters matrix
!>    @param[in] j bearing index
!>    @param[in] rpm speed
!>    @return parameter inverse coeficient value a
!
real(lrk) function ecoef(c,j,rpm)
  use rd_kinds, only: lrk
  implicit none
!
!     locals
  integer :: i
!     ivcoef matfun.f
  real(lrk) :: lc, ivcoef
  dimension lc(3)
!
!     arguments
  integer :: j
  real(lrk) :: rpm
!
  integer :: mxm
  parameter (mxm = 9)
  real(lrk) :: c(mxm,3)
!
!     copy to vector
  do i = 1,3
    lc(i) = c(j,i)
  end do
!     calculate coefficient
  ecoef = ivcoef(lc,rpm)
!
  return
!
end function ecoef
!
!     ==================================================================
!>    @brief get the bearing speed dependent parameter values from
!>     inverse equation, y=a*x^-1, a=sum(c(idx)*x^idx), idx=0,2.
!
!>    @param[in] rad speed (rad/s)
!>    @param[out] par output vector with bearing stiffness and
!>     damping parameters, k11 k12 k21 k22 c11 c12 c21 c22
!>    @param[out] nbr number of lateral bearings, not torsion restrictio
!>    @param[out] errmsg return error message
!>    @param[out] ok return flag. unsuccessful if <0.
!
subroutine parmanv(rad,par,nbr,errmsg,ok)
  use rd_textfun, only: fomsgf
  use com_cfm, only: c11, c12, c21, c22, d11, d12, d21, d22, nl
  use com_cft, only: eph, eth
  use com_man, only: kxx, kxz, kzz, kzx, cxx, cxz, czz, czx, mm, sc
  use com_nbc, only: cp, scl, bc
  use com_tor, only: kph, kth
  use com_ymc, only: nbrg, rks, pc
  use rd_kinds, only: lrk
  implicit none
!
!     locals
  real(lrk) :: a, rpm
  integer :: i, j, k
  character(len=1) :: blank
  character(len=7) :: nmm
  parameter (blank = ' ',nmm = 'parmanv')
!     function
  real(lrk) :: ecoef, rad2rpmf
!
!     input parameters
  integer :: mxm
  parameter (mxm = 9)
!
!     arguments
  real(lrk) :: rad
!     k11 k12 k21 k22 c11 c12 c21 c22
  real(lrk) :: par(mxm,10)
  character(len=99) :: errmsg
  integer :: nbr, ok
!
!     speed variable bearing parameters block
!     additional rotational parameters - francisco - oct-15
!
!     bearing block
!     scale, sc added francisco - oct-15
!     addeed francisco - oct-2015
!
!     init
  ok = -1
  errmsg = blank
!     speed rad/s -> rpm
  rpm =  rad2rpmf(rad)
!     check speed
  if (rpm .le. 0) then
!       invalid speed
    errmsg = fomsgf(99, nmm,22,blank,0)
    return
  end if
!
  k = 0
!     loop bearings
  do i = 1,nbrg
!       standard bearing index
    k = k+1
!       fixed parameters on input
    par(k,1) = kxx(i)*sc
    par(k,2) = kxz(i)*sc
    par(k,3) = kzx(i)*sc
    par(k,4) = kzz(i)*sc
    par(k,5) = cxx(i)*sc
    par(k,6) = cxz(i)*sc
    par(k,7) = czx(i)*sc
    par(k,8) = czz(i)*sc
!       additional rotational stiffness
    par(k,9) = kph(i)*sc
    par(k,10) = kth(i)*sc
!
!       loop parameters
    do j = 1,cp
!         variable parameters
      if(bc(j) .eq. k) then
!           stiffness k11 k12 k21 k22
        a = ecoef(c11,j,rpm)
        par(k,1) = a*1/rpm*scl(j)
        a = ecoef(c12,j,rpm)
        par(k,2) = a*1/rpm*scl(j)
        a = ecoef(c21,j,rpm)
        par(k,3) = a*1/rpm*scl(j)
        a = ecoef(c22,j,rpm)
        par(k,4) = a*1/rpm*scl(j)
!           damping c11 c12 c21 c22
        a = ecoef(d11,j,rpm)
        par(k,5) = a*1/rpm*scl(j)
        a = ecoef(d12,j,rpm)
        par(k,6) = a*1/rpm*scl(j)
        a = ecoef(d21,j,rpm)
        par(k,7) = a*1/rpm*scl(j)
        a = ecoef(d22,j,rpm)
        par(k,8) = a*1/rpm*scl(j)
!           additional rotational stiffness
        if (nl(j) .gt. 8) then
          a = ecoef(eph,j,rpm)
          par(k,9) = a*1/rpm*scl(j)
          a = ecoef(eth,j,rpm)
          par(k,10) = a*1/rpm*scl(j)
        end if
      end if
!
    end do
!
  end do
!
  nbr = k
  ok = 0
!
  return
!
end subroutine parmanv
!
!     ==================================================================
!>    @brief fixed bearing parameters, not speed dependent.
!
!>    @param[out] par output vector with bearing stiffness and
!>     damping parameters, k11 k12 k21 k22 c11 c12 c21 c22
!>    @param[in] mxi number of rows of par, same as mxm
!
subroutine parmanf(par,mxi)
  use com_man, only: kxx, kxz, kzz, kzx, cxx, cxz, czz, czx, mm, sc
  use com_tor, only: kph, kth
  use com_ymc, only: nbrg, rks, pc
  use rd_kinds, only: lrk
  implicit none
!
!     locals
  integer :: i
!     Canonical bearing parameter order used by all matrix assemblers.
  integer :: ikxx, ikxz, ikzx, ikzz, icxx, icxz, iczx, iczz
  parameter (ikxx=1,ikxz=2,ikzx=3,ikzz=4,&
  &icxx=5,icxz=6,iczx=7,iczz=8)
  character(len=7) :: nmm
  parameter(nmm = 'parmanf')
!
!     arguments
  integer :: mxi
!     k11 k12 k21 k22 c11 c12 c21 c22 [kph kth]
  real(lrk) :: par(mxi,10)
!
!     bearing block
  integer :: mxm
  parameter (mxm = 9)
!
!     addeed rotational parameters - francisco - oct-2015
!     bearing parameters block
!     scale, sc added francisco - oct-15
!
!     loop bearing
  do i = 1,nbrg
    if (i .gt. mxi) then
!         should not happens, ubound, stop
      call elmsge(1,4,nmm)
    end if
!       input fixed parameters
    par(i,ikxx) = kxx(i)*sc
    par(i,ikxz) = kxz(i)*sc
    par(i,ikzx) = kzx(i)*sc
    par(i,ikzz) = kzz(i)*sc
    par(i,icxx) = cxx(i)*sc
    par(i,icxz) = cxz(i)*sc
    par(i,iczx) = czx(i)*sc
    par(i,iczz) = czz(i)*sc
!       additional rotational stiffness
    par(i,9) = kph(i)*sc
    par(i,10) = kth(i)*sc
!
  end do
!
  return
!
end subroutine parmanf
!
!     ==================================================================
!>    @brief check if interpolation speed for bearing parameters table
!>     is inside appropriate speed range.
!
!>    @param[in] rad angular speed (rad/s)
!>    @return true if given speed is inside appropriate speed range,
!>     false otherwise.
!
logical function tsplimf(rad)
  use rd_kinds, only: lrk,wp
  use com_brgsm, only: mrp
  use com_nbc, only: cp
  use com_pmk, only: nt,rmc
  use rd_bearing_contract, only: bearing_speed_in_range
  implicit none
  real(lrk) :: rad
  real(wp) :: rpm
  integer :: j,info
  character(len=99) :: errmsg
  tsplimf=.false.; rpm=real(rad,wp)*30._wp/acos(-1._wp)
  if(cp<1.or.cp>9) return
  do j=1,cp
    if(nt(j)<2.or.nt(j)>99) return
    call bearing_speed_in_range(rpm,real(rmc(j,1),wp),real(rmc(j,nt(j)),wp), &
      real(mrp,wp),errmsg,info)
    if(info/=0) return
  end do
  ! Intersection of all current tables, recalculated on every call.
  tsplimf=.true.
end function tsplimf
!
!     ==================================================================
!>    @brief get bearing parameters.
!
!>    @param[in] rad speed (rad/s)
!>    @param[in] pv bearing speed dependent param flag true for variable
!>    @param[in] mxm output vector number of lines, columns is 10.
!>    @param[out] nbr number of lateral bearings, not torsion restrictio
!>    @param[out] par output vector with bearing parameters
!>    @param[out] errmsg return error message
!>    @param[out] ok return flag. unsuccessful if <0.
!
subroutine parman(rad,pv,mxm,nbr,par,errmsg,ok)
  use rd_kinds, only: lrk
  use com_knd, only: cknd
  use com_ymc, only: nbrg
  use, intrinsic :: ieee_arithmetic, only: ieee_is_finite
  implicit none
  integer :: mxm,nbr,ok,oki
  real(lrk) :: rad,par(mxm,10),local(9,10)
  logical :: pv
  character(len=99) :: errmsg
  ok=-1; errmsg=''; par=0._lrk; local=0._lrk; nbr=nbrg
  if(nbrg<0.or.nbrg>9.or.mxm<nbrg) then
    errmsg='parman: invalid number of bearings/output rows'; return
  end if
  if(.not.ieee_is_finite(rad).or.rad<0._lrk) then
    errmsg='parman: expected finite nonnegative angular speed'; return
  end if
  if(pv.and.cknd/='0') then
    select case(cknd)
    case('1')
      if(rad==0._lrk) then
        errmsg='parman: inverse bearing law is singular at zero rpm'; return
      end if
      call parmanv(rad,local,nbr,errmsg,oki)
    case('2')
      call parmant(rad,local,nbr,errmsg,oki)
    case default
      errmsg='parman: unknown variable-bearing representation'; return
    end select
    if(oki/=0) return
  else
    call parmanf(local,9)
  end if
  if(nbr<0.or.nbr>mxm.or.nbr>9) then
    errmsg='parman: returned bearing count is out of bounds'; return
  end if
  if(.not.all(ieee_is_finite(local(1:nbr,:)))) then
    errmsg='parman: bearing coefficients are nonfinite'; return
  end if
  par(1:nbr,:)=local(1:nbr,:)
  ok=0
end subroutine parman
!
!     ==================================================================
!>    @brief prepares the bearing whole matrices.
!
!>    @param[in] rad speed (rad/s)
!>    @param[in] dm matrices dimension
!>    @param[in] pp number of effective bearings with support
!>     pp = 0 if no consider supports.
!>    @param[out] wk returns whole stiffness matrix
!>    @param[out] wc returns whole bearing damping matrix
!>    @param[in] fx flag fixed speed parameters, 0 = variable
!>    @param[out] errmsg return error message
!>    @param[out] ok return flag. unsuccessful if <0.
!
subroutine prpkc(rad,dm,pp,wk,wc,fx,errmsg,ok)
  use com_knd, only: cknd, supr
  use com_ymc, only: nbrg, rks, pc
  use rd_kinds, only: lrk, wp
  implicit none
!
!     locals
  logical :: pv
  integer :: ii, j, kk, ll, nn, nbr
  integer :: oki, inddis, ifidxof
  character(len=1) :: cs
  character(len=5) :: nmm
  character(len=9) :: dsc
  parameter (cs = 'S',nmm = 'prpkc',dsc = 'brg. mass')
!
!     maximum number of global matrix elements
  integer :: mtg
  parameter (mtg = 500)
!
!     arguments
  real(lrk) :: rad
  integer :: dm, pp, fx, ok
  character(len=99) :: errmsg
  complex(wp) :: wk(mtg,mtg), wc(mtg,mtg)
!
!     input parameter
  integer :: mxm
  parameter (mxm = 9)
!
!     k11 k12 k21 k22 c11 c12 c21 c22 [kph kth]
  real(lrk) :: par(mxm,10)
  complex(wp) :: prc(mxm,10)
!
!     bearings block
!
!     kind of bearing stiffness calculation
!
!     init
  ok = -1
!
!     bearing speed variable parameters
  pv = fx .eq. 0
!     bearing parameters -> par
  nbr = nbrg
  call parman(rad,pv,mxm,nbr,par,errmsg,oki)
  if (oki .lt. 0) return
!
!     copy to complex par -> prc
  call copmat2_rc(par,prc,nbr,10,mxm,10,mxm,10)
!
!     zero complex matrices -> wk, wc
  call zermat_c(wk,dm,dm,mtg,mtg)
  call zermat_c(wc,dm,dm,mtg,mtg)
!
!     assembly amplied matrices
!
!          |kxx kxz  0   0 |
!     [kb]=|kzx kzz  0   0 |
!          | 0   0  kph  0 |
!          | 0   0   0  kth|
!
  j = 0
  do ii = 1,nbrg
    j = j+1
!       search bearing position index
    kk = inddis(pc(ii),errmsg,oki)
    if (oki .gt. 0) then
!         stiffness
      ll = ifidxof(kk,1)
      nn = ll+1
!         2 x 2
!         xx
      wk(ll,ll) = wk(ll,ll)+prc(j,1)
!         zz
      wk(nn,nn) = wk(nn,nn)+prc(j,4)
      wk(ll,nn) = wk(ll,nn)+prc(j,2)
      wk(nn,ll) = wk(nn,ll)+prc(j,3)
!         rotational stiffness - added francisco - oct-15
      wk(nn+1,nn+1) = wk(nn+1,nn+1)+prc(j,9)
      wk(nn+2,nn+2) = wk(nn+2,nn+2)+prc(j,10)
!         damping
      wc(ll,ll) = wc(ll,ll)+prc(j,5)
      wc(nn,nn) = wc(nn,nn)+prc(j,8)
      wc(ll,nn) = wc(ll,nn)+prc(j,6)
      wc(nn,ll) = wc(nn,ll)+prc(j,7)
!
!         optional direct BEARING -> FOUNDATION connection. The
!         ordinary bearing diagonal above is retained and relative
!         bearing cross/ground-side terms are added by the helper.
      call foundation_add_bearing(ii,dm,ll,par,wk,wc,mtg)
    else
!         if index not found return with error
      return
    end if
!       end bearing loop
  end do
!
!     bearing support, case sensitive. see entrada.f
  if(supr(1:1) .eq. cs) then
!       pp should be > 0
    call prpsp(par,dm,pp,wk,wc,errmsg,oki)
    if (oki .lt. 0) return
  end if
!
!     add reduced FOUNDATION [Kf] and [Cf] to its global DOF block.
  call foundation_add_kc(dm,wk,wc,mtg)
!
!     return ok
  ok = 0
!
  return
!
end subroutine prpkc
!
