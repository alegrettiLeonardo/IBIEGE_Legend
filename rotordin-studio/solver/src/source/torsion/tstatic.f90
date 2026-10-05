!     $Id$
!     ==================================================================
!
!>    @file tstatic.f
!>    @brief torsion static angular
!>    last changes:<br>
!>    new file de-19 - f<br>
!>    changed mxb = 99 francisco - apr-20
!>    added equivalent stiffness for static torque - francisco - oct-20.
!
!     ==================================================================
!
!>    @brief calculates angular generalized displacement vector.
!
!>    @param[in] im torsion model identifier: 1 two, 2 three nodes.
!>    @param[in] is dimension of generalized displacements vector,
!>     same as mtg
!>    @param[out] id size of generalized displacements vector
!>    @param[out] dlv generalized displacements vector
!>    @param[out] errmsg return an error message
!>    @param[out] ok return flag. unsuccessful if <0.
!
subroutine tsttor(im,is,id,dlv,errmsg,ok)
  use rd_textfun, only: fomsgf
  use com_sprat, only: crat
  use com_tmat, only: tj, tk, tc, dm
  use com_unb1, only: ndd, mu, ed, tpf, nb
  use rd_kinds, only: lrk, wp
  implicit none
!
!     arguments
  integer :: id, im, is, ok
!     note is = mtg
  real(wp) :: dlv
  character(len=99) :: errmsg
  dimension dlv(is)
!
!     unbalance
  integer :: mxb
  parameter (mxb = 99)
!     kind of exc 0=unbalance 1=concentrated, 3=static torque
!
!     total number of divisions
  integer :: mts
  parameter (mts = 999)
!
  integer :: mtt
  parameter (mtt = mts)
!
!     speed ratio
!
!     maximal number of global matrices elements
  integer :: mtg
  parameter (mtg = 500)
!
!     global torsion matrices:inertia tj, stiffness tk, damping tc
!
!     relative angular displacement
  integer :: ipiv
  dimension ipiv(mtg)

!     stiffness local matrix,torque vector
  real(wp) :: tkm, tqv
  dimension tkm(mtg,mtg),tqv(mtg)
!
  character(len=1) :: cs, blank
  character(len=3) :: erc
  character(len=6) :: nmm
  dimension cs(2)
  parameter (blank = ' ', cs = (/' ','N'/),nmm = 'tsttor')
  integer :: ii, m1, c2, nn
  integer :: indypf
  real(lrk) :: s3, tq
!
  ok = -1
!
!     copy torsion stiffness matrix tk -> tkm
  call copmat_d(tk,tkm,dm,mtg,mtg)
!
!     zero on torque vector 0 -> tqv
  call zervec_d(tqv,dm,mtg)
!
  nn = 0
!     put load torque on torque vector
!     number of unbalances excitation
  do ii = 1,nb
    if (tpf(ii) .eq. 3) then
!         static torque
      nn = nn+1
!         unbalance (torque) position division index
      m1 = indypf(ndd(ii),errmsg)
      if (m1 .lt. 1) then
!          return with not found error
        return
      end if
!         model index
      if  (im .eq. 1) then
        c2 = m1
      else
        c2 = (m1-1)*2+1
      end if
!         check bounds
      if (ii .gt. mxb .or. c2 .gt. mtg .or. m1 .gt. mtt) then
!           'out of bounds' !4
        errmsg = fomsgf(99, nmm,4,blank,0)
        return
      end if
!         speed ratio
      if (m1 .gt. 1) then
        s3 = crat(m1-1)
      else
        s3 = 1
      end if
!         torque
      tq = s3*mu(ii)
      tqv(c2) = tqv(c2)+tq
    end if
  end do
!
!     check for excitation
  if (nn .lt. 1) then
    errmsg = fomsgf(99, nmm,18,cs(1),0)
    return
  end if
!
!     solve on stiffness matrix copy tkm -> tkm UPPER
  call dgetrf(dm,dm,tkm,mtg,ipiv,ii)
  if(ii .ne. 0) then
    write(erc,5) ii
    errmsg = fomsgf(99, nmm,28,erc,0)
    return
  end if
!
!     copy force vector to solution tqv->dlv
  call copv_dd(tqv,dlv,dm,mtg,mtg)
!     solve system, calculate angular displacements -> dlv
!     lapack system solver -> dlv
  call dgetrs(cs(2),dm,1,tkm,mtg,ipiv,dlv,dm,ii)
  if(ii .ne. 0) then
    write(erc,5) ii
    errmsg = fomsgf(99, nmm,32,erc,0)
    return
  end if
!     return dimension
  id = dm
!
  ok = 0
!
  return
!
5 format(i3)
!
end subroutine tsttor
!
!     ==================================================================
!>    @brief torsion static angular displacment and stress
!
!>    @param[in] im torsion model identifier: 1 two, 2 three nodes.
!>    @param[in] std standard input output
!>    @param[in] plt HPGL output
!>    @param[out] errmsg return an error message
!>    @param[out] ok return flag. unsuccessful if <0.
!
subroutine tstatic(im,std,plt,errmsg,ok)
  use rd_kinds, only: lrk, wp
  implicit none
!
!     arguments
  character(len=99) :: errmsg
  integer :: im, ok
  logical :: std, plt
!
!     local
  character(len=7) :: nmm
  parameter (nmm = 'tstatic')
  integer :: dm, oki
!
!     maximal number of global matrices elements
  integer :: mtg
  parameter (mtg = 500)
!
!     relative angular displacement
  integer :: mrc
  parameter (mrc = 1)
  real(lrk) :: tav, trq, tau
  dimension tav(mtg,mrc),trq(mtg,mrc),tau(mtg,mrc)
!
!     equivalent torsion stiffness
  integer :: nsf, mxb
  parameter(mxb = 99)
  real(lrk) :: tsf
  real(wp) :: dlv
  dimension tsf(mxb,3),dlv(mtg)
!
!     initialize return
  ok = -1
  call progress_begin('STATIC_TORSION',3,&
  &'solve static torsional displacement and stress')
  call progress_stage('STATIC_TORSION',&
  &'solve angular generalized displacements')
!
!     calculates angular generalized displacement vector -> dm,dlv
  call tsttor(im,mtg,dm,dlv,errmsg,oki)
  if (oki .lt. 0) then
    call progress_end('STATIC_TORSION','FAILED')
    return
  end if
  call progress_update('STATIC_TORSION',1,3,real(dm, lrk),'DOF')
  call progress_stage('STATIC_TORSION',&
  &'calculate torque, stress and equivalent stiffness')
!
!     calculates angular displacements, torque and stress -> tav,trq,tau
  call ttrqdiv(im,1,dlv,tav,trq,tau,dm,mtg,mrc)
!
!     calculates equivalent stiffness exc-brg -> tsf,nsf
!     if there is a torsion bearing and a torque, default
!     or if definition is set on torsion input. see tentrada.
!     ttrqdiv.f
  call teqstf(1,tav,trq,tsf,nsf,mxb,mtg,mrc,errmsg,oki)
  if (oki .lt. 0) then
    call progress_end('STATIC_TORSION','FAILED')
    return
  end if
  call progress_update('STATIC_TORSION',2,3,real(nsf, lrk),'EQUIV_STIFFNESS')
  call progress_stage('STATIC_TORSION',&
  &'write static torsion output')
!
!     output will set ok
  call ts_static(1,tav,trq,tau,tsf,&
  &nsf,mtg,mrc,mxb,std,plt,errmsg,ok)
  if (ok .lt. 0) then
    call progress_end('STATIC_TORSION','FAILED')
    return
  end if
  call progress_update('STATIC_TORSION',3,3,real(dm, lrk),'DOF')
  call progress_end('STATIC_TORSION','OK')
!
  return
!
end subroutine tstatic
!
