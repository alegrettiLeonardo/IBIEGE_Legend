!     $Id$
!     ==================================================================
!
!>    @file tsecdl.f
!>    @brief torsional equivalent length.
!>    Equivalent length of stepped shafts with abrupt change in cross
!>    section.
!>    \verbatim
!>    Nestorides, E. J. A handbook on torsional vibration. Cambridge:
!>    Cambridge University Press.<br>
!>    \endverbatim
!>    last changed:<br>
!>    new file nov-19 - f.
!
!     ==================================================================
!>    @brief calculate cubic coefficients
!
!>    @param[in] ri fillet over smaller diameter relation
!>    @param[out] ro cubic coefficients
!
subroutine tcubic(ri,ro)
  use rd_kinds, only: lrk
  implicit none
!
  real(lrk) :: ri, ro
  dimension ro(4)
!
!     cubic coefficients
  real(lrk) :: c1, c2, c3, c4
  dimension c1(3),c2(3),c3(3),c4(3)
!
  parameter (c1 = (/-0.39806698_lrk,0.905826203_lrk,-0.374011947_lrk/),c2 = (/0.70609558_lrk,-1.35354548_lrk, &
    & 0.534644181_lrk/),c3 = (/-0.36438888_lrk,0.518083459_lrk,-0.183161384_lrk/),c4 = (/0.05618723_lrk, &
    & -0.065556259_lrk,0.021020194_lrk/))
!
!     cubic coefficient
  ro(4) = c4(1)*ri**2+c4(2)*ri+c4(3)
!     quadratic coefficient
  ro(3) = c3(1)*ri**2+c3(2)*ri+c3(3)
!     linear coefficient
  ro(2) = c2(1)*ri**2+c2(2)*ri+c2(3)
!     constant coefficient
  ro(1) = c1(1)*ri**2+c1(2)*ri+c1(3)
!
  return
!
end subroutine tcubic
!
!     ==================================================================
!>    @brief calculates equivalent length over smaller diameter.
!>    test data d21=1.5,rr1=0.15->0.060469665
!
!>    @param[in] d21 bigger over smaller diameter, diameter ratio
!>    @param[in] rr1 fillet radius over smaller diameter radius
!>    @return equivalent length over smaller diameter
!
real(lrk) function tsecdlf(d21,rr1)
  use rd_kinds, only: lrk
  implicit none
!
  real(lrk) :: d21, rr1
!
  real(lrk) :: d21i, tsecdl, c
  dimension c(4)
  character(len=7) :: nmm
  parameter (nmm = 'tsecdlf')
!
!     check arguments
  if (d21 .lt. 0 .or. rr1 .lt. 0) then
!       invalid arguments
    call elmsge(1,8,nmm)
  end if
!
  if (rr1 .gt. 0.52_lrk) then
!     fillet ratio above 0.52 needs no compensation
    tsecdl = 0
  else
    if (d21 .gt. 3) then
!     limit values for diameter ratio above three
      d21i = 3
    else
      d21i = d21
    end if
!     get equation coefficients
    call tcubic(rr1,c)
    tsecdl = c(4)*d21i**3+c(3)*d21i**2+c(2)*d21i+c(1)
!     check for results less than zero (numerical issues)
    if (tsecdl .lt. 0) tsecdl = 0
  end if
!
  tsecdlf = tsecdl
!
  return
!
end function tsecdlf
!
