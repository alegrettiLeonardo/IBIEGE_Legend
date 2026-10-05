!     $Id$
!     ==================================================================
!
!>    @file tindsec.f
!>    @brief torsional section indices.
!>    last changes:<br>
!>    new file dec-19 - f.
!
!     ==================================================================
!>    @brief search division index for a given position.
!>    What is the division index for a given position.
!
!>    @param[in] n number of divisions
!>    @param[in] dely nodal distances
!>    @param[in] dps desired division position (m)
!>    @param[in] nn number of number of divisions
!>    @param[in] mts division vector dimension
!>    @param[out] errmsg return an error message
!>    @return division index for a given position or -1 if not found
!
integer function dividf(n,dely,dps,nn,mts,errmsg)
  use rd_textfun, only: fomsgf
  use rd_kinds, only: lrk
  implicit none
!
  integer :: nn, mts
  real(lrk) :: dps
!     all sections (divisions)
  real(lrk) :: n, dely
  dimension n(mts),dely(mts)
  character(len=99) :: errmsg
!
  integer :: ii, divid
  real(lrk) :: prec, sgl(mts)
!     precision
  parameter (prec = 1e-6_lrk)
!
!     locals
  character(len=1) :: blank
  character(len=6) :: nmm
  parameter (blank = ' ',nmm = 'dividf')
!
  intrinsic :: abs
!
  divid = -1
  do ii = 1,nn
    if (ii .eq. 1) then
      sgl(ii) = n(ii)*dely(ii)
    else
      sgl(ii) = sgl(ii-1) + n(ii)*dely(ii)
    end if
  end do
!     search division index
  do ii = 1,nn
    if (dps .lt. sgl(ii) .or. abs(dps-sgl(ii)) .le. prec) then
      divid = ii
      exit
    end if
  end do
!     not found
  if (divid .lt. 0) then
    errmsg = fomsgf(99, nmm,14,blank,0)
  end if
  dividf = divid
!
  return
!
end function dividf
!
!     ==================================================================
!>    @brief Search y index for a division position.
!>     What is the y index for a given division position.
!
!>    @param[in] yp desired division y position (m)
!>    @param[out] errmsg return an error message
!>    @return y position index or -1 if not found
!
integer function indypf(yp,errmsg)
  use rd_textfun, only: fomsgf
  use com_sec, only: n, y, nt, nn
  use rd_kinds, only: lrk
  implicit none
!
  real(lrk) :: yp
  character(len=99) :: errmsg
!
!     total number of sections
  integer :: mts
  parameter (mts = 999)
!
!     all sections (divisions)
!
!     locals
  integer :: i, indyp
  character(len=1) :: blank
  character(len=6) :: nmm
  parameter (blank = ' ',nmm = 'indypf')
!
  real(lrk) :: prec
  parameter (prec = 1e-6_lrk)
!
  intrinsic :: abs
!
  indyp = -1
  do i = 1,nt
    if (abs(yp-y(i)) .lt. prec) then
      indyp = i
      exit
    end if
  end do
!
!     not found
  if (indyp .lt. 0) then
    errmsg = fomsgf(99, nmm,14,blank,0)
  end if
!
  indypf = indyp
!
  return
!
end function indypf
!
!     ==================================================================
!>    @brief get division external radius
!
!>    @param[in] idv division id
!>    @param[out] lini division start length or zero
!>    @param[out] lend division end length or zero
!>    @return external radius of section where division belongs or zero
!
real(lrk) function trextf(idv,lini,lend)
  use com_eix, only: l, d, di, ps, e, nu, rho_e, g_e, r1, cs
  use com_sec, only: n, y, nt, nn
  use rd_kinds, only: lrk
  implicit none
!
  integer :: idv
  real(lrk) :: lend
!
  real(lrk) :: trext, lini
!
!     shaft sections
  integer :: mxs
  parameter (mxs = 99)
!
!     all divisions
  integer :: mts
  parameter (mts=999)
!
  integer :: is, indsecf
!
  is = indsecf(idv)
!
  if (is .gt. 0) then
!       section found
    trext = d(is)/2
!       division init
    lini = y(idv-1)
!       section end
    lend = y(idv)
  else
!       section not found
    lini = 0._lrk
    lend = 0._lrk
    trext = 0._lrk
  end if
!
!     set function
  trextf = trext
!
  return
!
end function trextf
!
!     ==================================================================
!>    @brief check if division id is torsion coupling.
!
!>    @param[in] icid torsion coupling division index
!>    @return true if given division id is coupling
!
logical function idcoupf(icid)
  use com_tcoup, only: idsec, cnsc
  implicit none
  integer :: icid
!
!     total number of divisions, see tmatrices
  integer :: mts, mtt
  parameter (mts = 999,mtt = mts)
!
!     coupling divisions properties
!
  integer :: ii
  logical :: idcoup
!
  idcoup = .false.
  do ii = 1,cnsc
    idcoup = icid .eq. idsec(ii)
    if (idcoup) exit
  end do
  idcoupf = idcoup
!
  return
!
end function idcoupf
!
