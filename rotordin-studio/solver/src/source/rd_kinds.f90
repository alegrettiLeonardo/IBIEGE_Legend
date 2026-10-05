!> @file rd_kinds.f90
!> @brief Kind parameters for RotorDin (Fortran 2018 migration, M8/REAL64).
!>
!> wp : working precision, IEEE binary64 (REAL64). Single authority.
!> lrk: kind used for every quantity that was default REAL in the legacy
!>      Fortran 77 source.  M8 promotes lrk from real32 to real64: every
!>      declaration and literal tagged _lrk across the tree is promoted by
!>      this one line, with no other source change.  lrk is kept as a
!>      distinct name (== wp) only for this M8.1 commit, so the promotion
!>      diff is exactly this file; M8.2 removes the now-redundant alias by
!>      renaming lrk to wp everywhere (mechanical, no numeric effect).
module rd_kinds
  use, intrinsic :: iso_fortran_env, only: real64
  implicit none
  private
  public :: lrk, wp
  integer, parameter :: wp = real64
  integer, parameter :: lrk = real64
end module rd_kinds
