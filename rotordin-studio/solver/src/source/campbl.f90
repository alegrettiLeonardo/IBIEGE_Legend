!     $Id$
!     ==================================================================
!
!>    @file campbl.f
!>    @brief Campbell diagram calculation, last changes:<br>
!>    added variable speed bearing parameters - francisco - 03/12/2008<b
!>    campbell with bearing and support parameters - francisco - 05/03/2
!>    added std i/o - francisco - 16/02/2012<br>
!>    added clear of fcr matrix - 25/05/2015<br>
!>    moved intlag and f1camp to matfun - francisco - oct-2015<br>
!>    added min speed on intersection calculation - francisco - nov-15<b
!>    added imt Campbell mode tracking flag - francisco - jun-20<br>
!>    updated screen feedback to central routine in saidas - francisco -
!>    changed interpolation algorithm speed and frequency loops - franci
!>    added calculation speed range control when using bearing table - f
!>    added setable crossing lines - francisco - apr-21.<br>
!>    added complex-MAC mode tracking - sep-26.
!
!     ==================================================================
!>    @brief calculates the campbell diagram.
!
!>    @param[in] pp number of effective bearing supports
!>    @param[in] pv bearing speed dependent param flag
!>    @param[in] std standard i/o flag
!>    @param[in] plt HPGL output
!>    @param[out] errmsg return an error message
!>    @param[out] ok return flag. unsuccessful if <0.
!
subroutine campbell(pp,pv,std,plt,errmsg,ok)
  use rd_campbell_engine, only: run_campbell
  use com_cpb, only: dw
  use, intrinsic :: ieee_arithmetic, only: ieee_is_finite
  implicit none
  integer :: pp,ok,ptotal
  logical :: pv,std,plt
  character(len=99) :: errmsg
  ! Validate only the telemetry total here; the engine owns input validation.
  ptotal=1
  if(ieee_is_finite(dw)) then
    if(dw>=2.and.dw<=500) ptotal=nint(dw)+2
  end if
  call progress_begin('CAMPBELL',ptotal,'physical eigensolves and refined crossings')
  call run_campbell(pp,pv,std,plt,errmsg,ok)
  if(ok==0) then
    call progress_end('CAMPBELL','OK')
  else
    call progress_end('CAMPBELL','FAILED')
  end if
end subroutine campbell
!
