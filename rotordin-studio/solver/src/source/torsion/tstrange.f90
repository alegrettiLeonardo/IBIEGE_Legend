!     $Id$
!     ==================================================================
!
!>    @file tstrange.f
!>    @author francisco
!>    @date 27-jan-29
!>    @brief Grouped Frequency Distribution for stress-last changes:<br>
!>     new file 27-jan-29<br>
!>    removed unused variable nel - francisco - sep-21.
!
!     ==================================================================
!>    @brief group frequency distribution for input vector.
!
!>    @param[in] vin input vector
!>    @param[in] nst number of steps
!>    @param[in] nvn number of elements on input vector
!>    @param[in] ndm dimension of input vector
!>    @param[in] nrdm dimension of range
!>    @param[in] lgf true if values vector are "natural" logarithmic
!>    @param[in] lrg true if minimum and maximum range is passed
!>    @param[in] mnv minimum range value if lrg is set
!>    @param[in] mxv maximum range value if lrg is set
!>    @param[out] rnvl range matrix index of vin in each range
!>    @param[out] hval number of elements in each range
!>    @param[out] rgvl range values matrix minimum and maximum each rang
!
subroutine tstrange(vin,nst,nvn,ndm,nrdm,lgf,lrg,&
&mnv,mxv,rnvl,hval,rgvl)
  use rd_kinds, only: lrk
  implicit none
!
  integer :: nst, nvn, ndm, nrdm, hval, rnvl
  real(lrk) :: vin, rgvl, mnv, mxv
  dimension vin(ndm),rnvl(nrdm,ndm),hval(nrdm),rgvl(nrdm,2)
  logical :: lgf, lrg
!
  logical :: okv
  dimension okv(nvn)
!
  integer :: ii, jj, cnt, nre
  real(lrk) :: prc
  parameter (prc = 1e-12_lrk)
  real(lrk) :: mxrf, mnrf
  real(lrk) :: ivl, rgn, rtp, fvl, stv, fnv, val
  logical :: eqs, eqf
!
  intrinsic :: abs, exp, int, mod
!
!     range definition
  if (.not. lrg) then
!       internal
!       minimum value
    ivl = mnrf(vin,nvn,ndm)
!       maximum value
    fvl = mxrf(vin,nvn,ndm)
  else
!       given
    ivl = mnv
    fvl = mxv
  end if
!
!     range
  rgn = fvl-ivl
!     check for range value
  if (rgn .lt. prc) then
!       max = min
    rtp = 0
  else
!       range step
    rtp = rgn/nst
!       number of integral elements
    nre = mod(nvn,nst)
  end if
!
!     init classified flag
  do ii = 1,nvn
    okv(ii) = .false.
  end do
!
!     steps loop
  do ii = 1,nst
!       initial range value
    stv = ivl
    if (ii .gt. 1) then
      stv = stv+(ii-1)*rtp
    end if
!       final range value
    if (ii .eq. nst) then
!         final range value at last
      fnv = fvl
    else
      fnv = stv+rtp
    end if
!       range values
    if (lgf) then
!         log flag
      rgvl(ii,1) = exp(stv)
      rgvl(ii,2) = exp(fnv)
    else
      rgvl(ii,1) = stv
      rgvl(ii,2) = fnv
    end if
!
!       classify loop
    cnt = 0
    jj = 1;
    do while (jj .le. nvn)
      if (okv(jj)) then
!           loop
        jj = jj+1
      else
!           value to classify
        val = vin(jj)
!
        eqs = (abs(val-stv) .le. prc .or.&
        &(val .lt. stv .and. abs(val) .gt. prc)) .and.&
        &ii .eq. 1
        eqf = abs(val-fnv) .le. prc .or.&
        &(val .lt. fnv .and. ii .eq. nst)
!
        if((val .gt. stv .or. eqs) .and.&
        &(val .lt. fnv .or. eqf)) then
!             value classified
          okv(jj) = .true.
          cnt = cnt+1
!
          rnvl(ii,cnt) = jj
!             mark that has element
          hval(ii) = cnt
!
        end if
!           loop
        jj = jj+1
      end if
!
!         end classify loop
    end do
!
    if (cnt .eq. nvn) then
!         exit if all elements are in one range
      exit
    end if
!
!       end steps loop
  end do
!
  return
!
end subroutine tstrange
!
