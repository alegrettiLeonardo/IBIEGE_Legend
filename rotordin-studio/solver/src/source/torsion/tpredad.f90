!     $Id$
!     ==================================================================
!
!>    @file tpredad.f
!>    @brief torsional data preparation.
!>    last changes:<br>
!>    new file nov-19 - f<br>
!>    added sum on teqvlen for two fillets on one section may-21 - f.
!
!     ==================================================================
!>    @brief get user section sub-divisions.
!
!>    @param[in] p sections positon vactor
!>    @param[in] ps defined sections position vector
!>    @param[out] sdv user section sub-divisions
!>    @param[in] c number of sections plus one (+ 1)
!>    @param[in] mxp positions vector dimension
!>    @param[in] mxs diameter (d) and main section positions (ps) vector
!>    @param[in] mts vectors sdv dimension
!
subroutine subdiv(p,ps,sdv,c,mxp,mxs,mts)
  use rd_kinds, only: lrk
  implicit none

  integer :: c, mxp, mxs, mts
  real(lrk) :: p(mxp)
  real(lrk) :: ps(mxs)
  real(lrk) :: sdv(mts)
!     locals
  integer :: cc, ii, dv
!     precision
  real(lrk) :: prec
  parameter (prec = 1e-12_lrk)
!
!     search sub-divisions
  cc = 1
  dv = 1
!
  do ii = 1, (c - 1)
    if (abs(p(ii+1)-ps(cc)) .le. prec) then
      sdv(cc) = dv
      cc = cc + 1
      dv = 1
    else
      dv =dv+1
    end if
  end do
!
  return
!
end subroutine subdiv
!
!     ==================================================================
!>    @brief get user section sub-divisions external diameters.
!
!>    @param[in] p sections positon vector
!>    @param[in] ps defined sections position vector
!>    @param[in] d section external diameters vector
!>    @param[out] sde user section sub-divisions external diameters
!>    @param[in] c number of sections plus one (+ 1)
!>    @param[in] mxp positions vector dimension
!>    @param[in] mxs diameter (d) and main section positions (ps) vector
!>    @param[in] mts vectors sde dimension
!
subroutine extdia(p,ps,d,sde,c,mxp,mxs,mts)
  use rd_kinds, only: lrk
  implicit none

  integer :: c, mxp, mxs, mts
  real(lrk) :: p(mxp)
  real(lrk) :: d(mxs), ps(mxs)
  real(lrk) :: sde(mts)
!     locals
  integer :: cc, ii
!     precision
  real(lrk) :: prec
  parameter (prec = 1e-12_lrk)
!
!     search external diameters
  cc = 1
  sde(1) = d(cc)
!
  do ii = 1, (c - 1)
    if (abs(p(ii)-ps(cc)) .le. prec) then
      cc = cc + 1
      sde(cc) = d(cc)
    end if
  end do
!
  return
!
end subroutine extdia
!
!     ==================================================================
!>    @brief get additionl torsional equivalent section length.
!
!>    @param[in] d section external diameters vector
!>    @param[in] tfilrd section fillets
!>    @param[in] tfilid section ids vector
!>    @param[out] teql additional torsional equivalent section length.
!>    @param[in] c number of sections plus one (+ 1)
!>    @param[in] ntfil number of section fillets
!>    @param[in] mxtfil tfilrd and tfilid vectors dimension
!>    @param[in] mxs diameter (d) and main section positions (ps) vector
!
subroutine teqvlen(d,tfilrd,tfilid,teql,c,ntfil,mxtfil,mxs)
  use rd_kinds, only: lrk
  implicit none
!
  integer :: c, ntfil, mxtfil, mxs
  real(lrk) :: d, teql
  dimension d(mxs),teql(mxs)
!     section ids
  integer :: tfilid
!     section fillets
  real(lrk) :: tfilrd
  dimension tfilid(mxtfil),tfilrd(mxtfil)
!
  integer :: i, si, id
  real(lrk) :: di, df, d21, lfr, el, ad
  real(lrk) :: tsecdlf
  character(len=7) :: nmm
  parameter (nmm = 'teqvlen')
!
!     zero additional lengths
  do i = 1,c
    teql(i) = 0
  end do
!
  do i = 1,ntfil
!       check section index
    si = tfilid(i)-1
    if (si .ge. 1 .and. si .lt. c) then
!         previous section diameter
      di = d(si)
!         fillet section diameter
      df = d(si+1)
!         check for smaller diameter
      if (di .lt. df) then
        d21 = df/di
        lfr = tfilrd(i)/(di/2)
      else
        d21 = di/df
        lfr = tfilrd(i)/(df/2)
      end if
!         equivalent length over smaller diameter
      el = tsecdlf(d21,lfr)
!         additional smaller section length
      if (di .lt. df) then
        id = si
        ad = el*di
      else
        id = si+1
        ad = el*df
      end if
      teql(id) = teql(id)+ad
!     removed 02/07 - francisco
!        else
!         invalid index
!          call elmsge(1,27,nmm)
    end if
  end do
!
  return
!
end subroutine teqvlen
!
