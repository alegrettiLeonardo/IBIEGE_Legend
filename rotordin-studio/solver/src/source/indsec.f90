!     $Id$
!     ==================================================================
!
!>    @file indsec.f
!>    @brief section index finder, last changes:<br>
!>    new module - francisco - 03/12/2008<br>
!>    changed error messages to global - apr-19 - francisco<br>
!>    added indpos and rextf function - francisco dec-19<br>
!>    added indsecf - francisco jan-20<br>
!>    changed precision to same as prepad.f - francisco oct-20<br>
!>    added prec on position search functions - francisco - jul-21<br>
!>    added ispdpf function - francisco - jul-21<br>
!>    updated indrsp if y<0 set py=y, support index value francisco aug-
!>    added indfd get dist. force position - francisco - sep-21<br>
!>    added function rextof, with offset for rextf - francisco - dec-21.

!
!     ==================================================================
!>    @brief search section index for a given division index.
!
!>    @param[in] idv division index
!>    @return section index or zero if not found
!
integer function indsecf(idv)
  use com_eix, only: l, d, di, ps, e, nu, rho_e, g_e, r1, cs
  use com_sec, only: n, y, nt, nn
  use rd_kinds, only: lrk
  implicit none
!
  integer :: idv
!
!     all divisions
  integer :: mts
  parameter (mts = 999)
!
!     shaft sections
  integer :: mxs
  parameter (mxs = 99)
!
  real(lrk) :: py
  integer :: jj, indsec
!
  indsec = 0
!
  if (idv .gt. 0 .and. idv .le. nt) then
!       idv on divisions bound
!       y position, search section
    py = y(idv)
    do jj = 1,cs
      if (py .le. ps(jj)) then
!           y position on section
        indsec = jj
        exit
      end if
    end do
  end if
!
  indsecf = indsec
!
  return
!
end function indsecf
!
!     ==================================================================
!>    @brief search section position for orbit
!
!>    @param[in] y vector of section positions
!>    @param[in] po orbit positions vector
!>    @param[out] prs returns  the vector of indices of the nearest sect
!>    @param[out] py returns the vector of the nearest section positions
!>    @param[in] no number of orbits
!>    @param[in] nt number of sections
!>    @param[in] mxo vectors prs, py and po dimension
!>    @param[in] mts vector y dimension
!>    @param[out] errmsg return an error message
!>    @param[out] ok return flag. unsuccessful if <0.
!
subroutine indorb(y,po,prs,py,no,nt,mxo,mts,errmsg,ok)
  use rd_textfun, only: femsgf
  use rd_kinds, only: lrk
  implicit none
!
!     arguments
  integer :: no, nt, prs, mxo, mts, ok
  real(lrk) :: y, po, py
  dimension y(mts),po(mxo),py(mxo),prs(mxo)
  character(len=99) :: errmsg
!
!     locals
  integer :: i, j
  real(lrk) :: prec, pmn
  character(len=6) :: nmm
  parameter (prec = 1e8_lrk,nmm = 'indorb')
!
  intrinsic :: abs
!
!     orbits
!
!     return
  ok = -1
!
  if (no .lt. 1) then
    errmsg = femsgf(50, nmm,15,17,0)
    return
  end if
!
  do i = 1,no
!
    ok = -1
    pmn = prec
!
!       sections
    do j = 1, nt
!
      if (abs(y(j)-po(i)) .lt. pmn)  then
        ok = j
        pmn = abs(y(j)-po(i))
      end if
!         sections loop
    end do
!       found
    if (ok.gt. 0) then
!         section position
      prs(i) = ok
      py(i) = y(ok)
    else
      errmsg = femsgf(50, nmm,14,17,0)
      return
    end if
!       orbit loop
  end do
!
  ok = 0
!
  return
!
end subroutine indorb
!
!     ==================================================================
!>    @brief search the positions of unbalance load sections.
!
!>    @param[in] y vector of section positions
!>    @param[in] ndd vector of indices of disks with unbalance
!>    @param[out] pdd returns the vector of nearest section positions
!>    @param[in] nb number of unbalances
!>    @param[in] nt number of sections
!>    @param[in] mxb pd vector dimension
!>    @param[in] mxd  vetors pd and pdd dimension
!>    @param[in] mts vector y dimension
!>    @param[out] errmsg return an error message
!>    @param[out] ok return flag. unsuccessful if <0.
!
subroutine inddbl(y,ndd,pdd,nb,nt,mxb,mxd,mts,errmsg,ok)
  use rd_textfun, only: femsgf
  use rd_kinds, only: lrk
  implicit none
!
!     arguments
  integer :: nb, nt, pdd, mxb, mxd, mts, ok
  real(lrk) :: y, ndd
  dimension y(mts),ndd(mxb),pdd(mxd)
  character(len=99) :: errmsg
!
!     locals
  integer :: i, j
  real(lrk) :: pmn, prec
  character(len=6) :: nmm
  parameter (prec = 1e8_lrk,nmm = 'inddbl')
!
  intrinsic :: abs
!
!     unbalance
!
  if (nb .lt. 1) then
!       ,'position not defined' !15
!       ,'unbalance' !18
    errmsg = femsgf(50, nmm,15,18,0)
    return
  end if
!
  do i = 1, nb
    ok = -1
!       init
    pmn = prec
!       sections with unbaleance
    do j = 1, nt
      if (abs(y(j)-ndd(i)) .lt. pmn) then
        pmn = abs(y(j)-ndd(i))
        ok = j
      end if
!         sections loop
    end do
!
    if (ok .gt. 0) then
!         section position
      pdd(i) = ok
    else
!         ,'position not found' !14
!         ,'unbalance' !18
      errmsg = femsgf(50, nmm,14,18,0)
      return
    end if
!       end unbalance loop
  end do
!
  ok = 0
!
  return
!
end subroutine inddbl
!
!     ==================================================================
!>    @brief search of the index of the pd position element.
!
!>    @param[in] pd object position
!>    @param[out] errmsg return an error message
!>    @param[out] ok return flag. unsuccessful if <0.
!>    @return element position index
!
integer function inddis(pd,errmsg,ok)
  use rd_textfun, only: femsgf
  use com_sec, only: n, y, nt, nn
  use rd_kinds, only: lrk
  implicit none
!
!     arguments
  integer :: ok
  character(len=99) :: errmsg
  real(lrk) :: pd
!
!     locals
  integer :: ii
  real(lrk) :: mn, prec
  character(len=6) :: nmm
  parameter (prec = 1e8_lrk,nmm = 'inddis')
!
!     total number of sections
  integer :: mts
  parameter (mts=999)
!
!     all sections (divisions)
!
  intrinsic :: abs
!
!     search for disk position index
  ok = -1
  mn = prec
!
  do ii = 1,nt
    if (abs(pd-y(ii)) .lt. mn) then
      ok = ii
      mn = abs(pd-y(ii))
    end if
  end do
!     check if found
  if (ok .lt. 0) then
!       could not find
    errmsg = femsgf(50, nmm,14,19,0)
  end if
  inddis = ok
!
  return
!
end function inddis
!
!     ==================================================================
!>    @brief search response nearest position
!
!>    @param[in] y vector of section positions
!>    @param[in] pr vector of response positions
!>    @param[out] prs returns  the vector of indices of the nearest sect
!>    @param[out] py returns the vector of the nearest section positions
!>     if < 0 supoort index
!>    @param[in] np number of responses
!>    @param[in] nt number of sections
!>    @param[in] mxp vectors prs and py dimension
!>    @param[in] mts vector y dimension
!>    @param[out] errmsg return an error message
!>    @param[out] ok return flag. unsuccessful if <0.
!
subroutine indrsp(y,pr,prs,py,np,nt,mxp,mts,errmsg,ok)
  use rd_textfun, only: femsgf
  use rd_kinds, only: lrk
  implicit none
!
!     arguments
  integer :: np, nt, prs, mxp, mts, ok
  real(lrk) :: y, pr, py
  dimension y(mts),pr(mxp),py(mxp),prs(mxp)
  character(len=99) :: errmsg
!
!     locals
  integer :: i, j
  real(lrk) :: pmn, prec, tol
  character(len=6) :: nmm
  parameter (prec = 1e8_lrk,nmm = 'indrsp')
!
  intrinsic :: abs, int
!
!     response
!
  if (np .lt. 1) then
    errmsg = femsgf(50, nmm,15,20,0)
    return
  end if
!
!     response positions
  do i = 1,np
!
    ok = -1
    pmn = prec
!       sections.  Physical response positions must now be actual FEM
!       stations; do not silently snap to the nearest station.
    tol=max(1.0e-7_lrk,1.0e-6_lrk*abs(pr(i)))
    do j = 1,nt
!
!         position <0 number of bearing support
      if (.not. (pr(i) .lt. 0)) then
        if (abs(y(j)-pr(i)) .le. tol) then
          pmn = abs(y(j)-pr(i))
          ok = j
          exit
        end if
      else
!           support <0 (-1,-2...)
        ok = int(abs(pr(i)))
        exit
      end if
!         end sections loop
    end do
!
    if (ok .gt. 0) then
      if (.not. (pr(i) .lt. 0)) then
!           section index
        prs(i) = ok
        py(i) = y(ok)
      else
!           suport
        prs(i) = -ok
        py(i) = pr(i)
      end if
!
    else
      errmsg = femsgf(50, nmm,14,20,0)
      return
    end if
!
!       end response loop
  end do
!
  ok = 0
!
  return
!
end subroutine indrsp
!
!     ==================================================================
!>    @brief search for the "p" nearest position.
!
!>    @param[in] p position
!>    @param[out] errmsg return an error message
!     @return nearest position to p
!
integer function indpos(p,errmsg)
  use rd_textfun, only: fomsgf
  use com_eix, only: l, d, di, ps, e, nu, rho_e, g_e, r1, cs
  use rd_kinds, only: lrk
  implicit none
!
!     arguments
  real(lrk) :: p
  character(len=99) :: errmsg
!
!     locals
  integer :: i
  character(len=1) :: blank
  character(len=6) :: nmm
  parameter (blank = ' ',nmm = 'indpos')
!
  integer :: mxs
  parameter (mxs = 99)
!     shaft
!
  if (p .le. ps(1) .and. p .gt. 0) then
    indpos = 1
    return
!     added check position greater than length
  else if (p .gt. l) then
    indpos = cs
    return
  end if
!
  do i = cs,1,-1
    if (p .gt. ps(i)) then
      indpos = i+1
      return
    end if
  end do
!
!     not found
  errmsg = fomsgf(50, nmm,14,blank,0)
  indpos = -1
!
  return
!
end function indpos
!
!     ==================================================================
!>    @brief return a radius for a given position.
!>     return section radius or disk radius if there
!>     is a disk on the passed position
!
!>    @param[in] p position to get radius
!>    @param[in] io offset to position to get radius if no disk

!>    @return radius for the given position
!
real(lrk) function rextof(p,io)
  use com_dis, only: pd, d_d, h_d, rho_d, r2, nd
  use com_eix, only: l, d, di, ps, e, nu, rho_e, g_e, r1, cs
  use com_eixshape, only: d2, di2, etype
  use rd_kinds, only: lrk
  implicit none
!
  real(lrk) :: p
  integer :: io
!
  integer :: mxs
  parameter (mxs = 99)
!     shaft
!
!     disks
  integer :: mxd
  parameter (mxd = 99)
!
!     same position precision, check prepad.f
  real(lrk) :: prec
  parameter (prec = 1e-3_lrk)
!
  integer :: i
  real(lrk) :: ip, ep, rext, de, din
!
  intrinsic :: abs
!
  rext = -1
  do i = 1,nd
!       initial disk position
    ip = pd(i)-h_d(i)/2-prec
!       end disk position
    ep = pd(i)+h_d(i)/2+prec
    if (p .ge. ip .and. p .le. ep) then
      rext = d_d(i)/2
      exit
    end if
  end do
  if (rext .lt. 0) then
!       disk position not found
!       search section positions
    do i = 1,cs
!         initial
      if (i .eq. 1) then
        ip = -prec
      else
        ip = ps(i-1)-prec
      end if
      ep = ps(i)+prec
      if (p .ge. ip .and. p .le. ep) then
        if (io .eq. 0) then
          call shpdia(i,p,de,din)
          rext = de/2._lrk
        else
          rext = d(i+io)/2._lrk
        end if
        exit
      end if
    end do
  end if
!
  rextof = rext
!
  return
!
end function rextof
!
!     ==================================================================
!>    @brief return a radius for a given position.
!>     return section radius or disk radius if there
!>     is a disk on the passed position
!
!>    @param[in] p position to get radius

!>    @return radius for the given position
!
real(lrk) function rextf(p)
  use rd_kinds, only: lrk
  implicit none
!
  real(lrk) :: p
!
  real(lrk) :: rextof
!
  rextf = rextof(p,0)

end function rextf
!
!     ==================================================================
!>    @brief get division index from section position.
!>    returns -1 if not found.
!
!>    @param[in] nt number of divisions
!>    @param[in] mts dimension of division positions vector
!>    @param[in] y division positions vector
!>    @param[in] sp section position
!>    @return division index or -1 if not found
!
integer function ispdpf(nt,mts,y,sp)
  use rd_kinds, only: lrk
  implicit none
!
  integer :: nt, mts
  real(lrk) :: y, sp
  dimension y(mts)
!
  integer :: ii, ispdp
  real(lrk) :: prec

!     precision
  parameter (prec = 1e-8_lrk)
!
  ispdp = -1
!     loop on all divisions
  do ii = 1,nt
!       same value on all section up to its length
    if (y(ii) .ge. sp-prec) then
      if (ii .gt. 1) then
!           previous index
        ispdp = ii-1
      else
!           first index
        ispdp = 1
      end if
      exit
    end if
  end do
!
!     division index or negative
  ispdpf = ispdp
!
  return
!
end function ispdpf
!
!     ==================================================================
!>    @brief get position initial and final for a dist. force
!>    returns -1 if not found.
!
!>    @param[in] mxb dimension of excitation vectors tpf and ndd
!>    @param[in] mxs dimension of sections vector
!>    @param[in] nb number of excitations
!>    @param[in] cs number of sections
!>    @param[in] ids dist. force excitation index
!>    @param[in] tpf kind ofexcitation:
!>     -1:transient torque,0:unbalance (default),1:concentrated,
!>     2:harmonic torque,3:static torque,4:freq. response,5:dist. force
!>    @param[in] ndd excitation positions vector
!>    @param[in] ps sections positions vector
!>    @param[out] dps dist. force intial and final position (m)
!>    @param[out] errmsg return an error message
!>    @param[out] ok return flag. unsuccessful if <0.
!
subroutine indfd(mxb,mxs,nb,cs,ids,tpf,ndd,ps,dps,errmsg,ok)
  use rd_textfun, only: femsgf
  use rd_kinds, only: lrk
  implicit none
!
  integer :: mxb, mxs, nb, ids, cs, tpf, ok
  real(lrk) :: ndd, ps, dps
  dimension tpf(mxb),ndd(mxb),ps(mxs),dps(2)
  character(len=99) :: errmsg
!
  integer :: ii
  character(len=5) :: nmm
  parameter (nmm = 'indfd')
!
  intrinsic :: nint
!
!     init
  ok = -1
  dps(1) = 0
  dps(2) = 0
!
!     check excitation index
  if (ids .lt. 0 .or. ids .gt. nb) then
!       'invalid data ' !8
!       'excitation' !18
    errmsg = femsgf(50, nmm,8,18,0)
    return
  end if
!     check kind dist. force
  if (tpf(ids) .ne. 5) then
    errmsg = femsgf(50, nmm,8,18,0)
    return
  end if
!     check section index
  ii = nint(ndd(ids))
  if (ii .lt. 1 .or. ii .gt. cs) then
!       'sections number' !16
    errmsg = femsgf(50, nmm,8,16,0)
    return
  end if
!     get positions
  if (ii .eq. 1) then
    dps(1) = 0
    dps(2) = ps(1)
  else
    dps(1) = ps(ii-1)
    dps(2) = ps(ii)
  end if
!
  ok = 0
!
  return
!
end subroutine indfd
!
