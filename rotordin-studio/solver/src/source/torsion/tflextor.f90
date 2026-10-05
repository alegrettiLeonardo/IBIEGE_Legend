!     $Id$
!     ==================================================================
!
!>    @file tflextor.f
!>    @brief flexural-torsion section stress and fatigue calculations.
!>    last changes:<br>
!>    new file jun-21 - f<br>
!>    bug fix on tflxt dimension updated to 31 - francisco - nov - 21.
!>    \verbatim
!>     Added a warning messaging system per section.
!>     Based on octal system with seven levels 0-6.
!>     It is not possible sum same level errors.
!>    \endverbatim
!>     @see flxwarn
!
!     ==================================================================
!>    @brief get curves value by its inverse coefficients.
!>     handled curves crack sensitivity factor:
!>    <ul>
!>    <li>fatigue limit correction factor:mach/cold rol.</li>
!>    <li> fatigue limit correction factor:mach/hot rol.</li>
!>    <li>fatigue limit correction factor:forged.</li>
!>    <li>sizing factor.</li>
!>    <li>reliability factor.</li>
!>    </ul>
!>    see "flextor_equations" excel file for curves and coefficients
!
!>    @param[in] ni curve index:
!>    <ol>
!>     <li>crack sensitivity factor
!>     <li>fatigue limit correction factor:mach/cold rol</li>
!>     <li>fatigue limit correction factor:mach/hot rol</li>
!>     <li>fatigue limit correction factor:forged</li>
!>     <li>sizing factor</li>
!>     <li>reliability factor</li>
!>    </ol>
!>    @param[in] vi input value
!>    @param[out] wr warning level 0
!>    @return selected curve inverse coefficients value.
!
real(lrk) function icfvlf(ni,vi,wr)
  use rd_kinds, only: lrk
  implicit none
!
  integer :: ni, wr
  real(lrk) :: vi
!
  integer :: i, j, w
  character(len=1) :: dd
  character(len=6) :: nmm, fnm
  character(len=15) :: buf
  real(lrk) :: prec, etp, rr, vl, vr, ivcoef, cc, cf, ll, li
  dimension fnm(4),cc(6,3),cf(3),ll(6,2),li(2)
!     prec:zero precision,etp:extrapolation allowed pu
  parameter(nmm = 'icfvlf',dd = ':',fnm=(/'ck.sen','ft.lim','sz.fac','ry.fac'/),prec = 1e-8_lrk,etp = 0.15_lrk)
!     crack sensitivity factor
!     6205.02558,0.012253951,-1.24244E-07
!     fatigue limit correction factor:mach/cold rol
!     89.43964918,0.631260607,-3.70235E-05
!     fatigue limit correction factor:mach/hot rol
!     105.6150504,0.499597185,-0.000153794
!     fatigue limit correction factor:forged
!     107.0794017,0.315145444,-0.000105576
!     linearized sizing factor
!     1.351799646,-0.639354663,0.417002771
!     linearized reliability factor
!     1.424791072,0.579642701,-0.006346514
!
!     transposed data
  data cc/6205.02558_lrk,89.43964918_lrk,105.6150504_lrk,107.0794017_lrk,1.351799646_lrk,1.424791072_lrk, &
    & 0.012253951_lrk,0.631260607_lrk,0.499597185_lrk,0.315145444_lrk,-0.639354663_lrk,0.579642701_lrk, &
    & -1.24244E-07_lrk,-3.70235E-05_lrk,-0.000153794_lrk,-0.000105576_lrk,0.417002771_lrk,-0.006346514_lrk/
!     range (lbf/in^2)
!     50000,240000
!     range (MPa)
!     400,1700
!     range (MPa)
!     400,1700
!     range (MPa)
!     400,1700
!     range (mm)
!     20,1000
!     range percent
!     50,99.9999999
!
!     transposed data
  data ll/50000.0_lrk,400.0_lrk,400.0_lrk,400.0_lrk,20.0_lrk,50.0_lrk,240000.0_lrk,1700.0_lrk,1700.0_lrk,1700.0_lrk, &
    & 1000.0_lrk,99.9999999_lrk/
!
!     see excel, flextor_eq1
!     statement functions
!     sizing factor
!     reliability
!
  intrinsic :: log, log10
!
!     check index range
  if (ni .lt. 1 .or. ni .gt. 6) then
!       invalid option
    call elmsge(1,6,nmm)
  end if
!     get coefficients
  do i = 1,3
    cf(i) = cc(ni,i)
  end do
!     warning
  w = 0
!     get range
  if (ni .eq. 6) then
!       without extrapolation
    li(1) = ll(ni,1)
    li(2) = ll(ni,2)
  else
!       with extrapolation
    li(1) = ll(ni,1)*(1-etp)
    li(2) = ll(ni,2)*(1+etp)
  end if
!     check input against limits
  if (vi .lt. li(1) .or. vi .gt. li(2)) then
!       function name index
    j = ni
    if (ni .eq. 3) j = 2
    if (ni .gt. 3) j = ni-2
!       check buffer size -> function name:value
    write(buf,5) fnm(j),dd,vi
!       value out of expected range
    call elmsgow(0,33,nmm,buf)
!       warning code level 0
    w = ni
  end if
!     linearized ones
  if (ni .eq. 5)then
!       sizing factor
    vl = sf(vi)
  else if (ni .eq. 6) then
!       reliability
    vl = rf(vi)
  else
    vl = vi
  end if
!     check input value to close zero
  if (vl .lt. prec) then
!       invalid data, stop
    call elmsge(1,8,nmm)
  end if
!     final inverse coefficient
  rr = ivcoef(cf,vl)
!     value
  vr = rr*1/vl
!     de-linearizing factor
  if (ni .eq. 5) then
!       sizing factor
    rr = 10**(vr-1)
  else if (ni .eq. 6) then
!       reliability factor
!       note natural logarithm log(x)->exp(x)
    rr = log(vr)+1
  else
    rr = vr
  end if
!     warning level 0
  wr = w
!     final coefficient
  icfvlf = rr
!
  return
!
5 format(2a,e8.4)
!
contains

  !> former statement function sf
  pure real(lrk) function sf(vi)
    real(lrk), intent(in) :: vi
    sf = log10(vi/100.0_lrk+1.5_lrk)+1.0_lrk
  end function sf

  !> former statement function rf
  pure real(lrk) function rf(vi)
    real(lrk), intent(in) :: vi
    rf = 6.0_lrk-log10(1000.0_lrk-vi*9.999999999_lrk)
  end function rf

end function icfvlf
!
!     ==================================================================
!>    @brief get stress equations 0, 0.5 and 1 coefficients.
!
!>    @param[in] ki line line index
!>    @param[in] ni group index, four based
!>    \verbatim
!>    1 torsion stress concentration factor
!>    2 bending stress concentration factor tr <= 2
!>    3 bending stress concentration factor tr > 2
!>    4 tension stress concentration factor tr <= 2
!>    5 tension stress concentration factor tr > 2
!>    6 torsion stress concentration factor u-notch he/re <= 2
!>    7 torsion stress concentration factor u-notch he/re > 2
!>    8 bending stress concentration factor u-notch he/re <= 2
!>    9 bending stress concentration factor u-notch he/re > 2
!>    10 tension (axial) stress concentration factor u-notch he/re <= 2
!>    11 tension (axial) stress concentration factor u-notch he/re > 2
!>    \endverbatim
!>    @param[out] kr 0, 0.5 and 1 coefficients
!
subroutine steqcef(ki,ni,kr)
  use rd_kinds, only: lrk
  implicit none
!
  integer :: ki, ni
  real(lrk) :: kr, k
  dimension kr(3),k(44,3)
  integer :: i, li, ifidxof
  character(len=7) :: nmm
  parameter(nmm = 'steqcef')
!
!     1 torsion stress concentration factor
!     0.905,0.783,-0.075
!     -0.437,-1.969,0.553
!     1.557,1.073,-0.578
!     -1.061,0.171,0.086
!     2 bending stress concentration factor tr <= 2
!     0.947,1.206,-0.131
!     0.022,-3.405,0.915
!     0.869,1.777,-0.555
!     -0.81,0.422,-0.26
!     3 bending stress concentration factor tr > 2
!     1.232,0.832,-0.008
!     -3.813,0.968,-0.26
!     7.423,-4.868,0.869
!     -3.839,3.07,-0.6
!     4 tension stress concentration factor tr <= 2
!     0.926,1.157,-0.099
!     0.012,-3.036,0.961
!     -0.302,3.977,-1.744
!     0.365,-2.098,0.878
!     5 tension stress concentration factor tr > 2
!     1.2,0.86,-0.022
!     -1.805,-0.346,-0.038
!     2.198,-0.486,0.165
!     -0.593,-0.028,-0.106
!     6 stress concentration factor - u-notch
!     torsion stress concentration factor he/re <= 2
!     1.245,0.264,0.491
!     -3.03,3.269,-3.633
!     7.199,-11.286,8.318
!     -4.414,7.753,-5.176
!     7 torsion stress concentration factor he/re > 2
!     1.651,0.614,0.04
!     -4.794,-0.314,-0.217
!     8.457,-0.962,0.389
!     -4.317,0.662,-0.212
!     8 bending stress concentration factor he/re <= 2
!     0.455,3.354,-0.769
!     0.891,-12.721,4.593
!     0.286,15.481,-6.392
!     -0.932,-6.115,2.568
!     9 bending stress concentration factor he/re > 2
!     0.935,1.922,0.004
!     -0.552,-5.327,0.086
!     0.754,6.281,-0.121
!     -0.138,-2.876,0.031
!     10 tension (axial) stress concentration factor he/re <= 2
!     0.455,3.354,-0.769
!     3.129,-15.955,7.404
!     -6.909,29.286,-16.104
!     4.325,-16.685,9.469
!     11 tension (axial) stress concentration factor he/re > 2
!     0.935,1.922,-0.004
!     0.537,-3.708,0.04
!     -2.538,3.438,-0.012
!     2.066,-1.652,-0.031
!
!     transposed data
  data k/0.905_lrk,-0.437_lrk,1.557_lrk,-1.061_lrk,0.947_lrk,0.022_lrk,0.869_lrk,-0.81_lrk,1.232_lrk,-3.813_lrk, &
    & 7.423_lrk,-3.839_lrk,0.926_lrk,0.012_lrk,-0.302_lrk,0.365_lrk,1.2_lrk,-1.805_lrk,2.198_lrk,-0.593_lrk,1.245_lrk, &
    & -3.03_lrk,7.199_lrk,-4.414_lrk,1.651_lrk,-4.794_lrk,8.457_lrk,-4.317_lrk,0.455_lrk,0.891_lrk,0.286_lrk, &
    & -0.932_lrk,0.935_lrk,-0.552_lrk,0.754_lrk,-0.138_lrk,0.455_lrk,3.129_lrk,-6.909_lrk,4.325_lrk,0.935_lrk, &
    & 0.537_lrk,-2.538_lrk,2.066_lrk,0.783_lrk,-1.969_lrk,1.073_lrk,0.171_lrk,1.206_lrk,-3.405_lrk,1.777_lrk, &
    & 0.422_lrk,0.832_lrk,0.968_lrk,-4.868_lrk,3.07_lrk,1.157_lrk,-3.036_lrk,3.977_lrk,-2.098_lrk,0.86_lrk,-0.346_lrk, &
    & -0.486_lrk,-0.028_lrk,0.264_lrk,3.269_lrk,-11.286_lrk,7.753_lrk,0.614_lrk,-0.314_lrk,-0.962_lrk,0.662_lrk, &
    & 3.354_lrk,-12.721_lrk,15.481_lrk,-6.115_lrk,1.922_lrk,-5.327_lrk,6.281_lrk,-2.876_lrk,3.354_lrk,-15.955_lrk, &
    & 29.286_lrk,-16.685_lrk,1.922_lrk,-3.708_lrk,3.438_lrk,-1.652_lrk,-0.075_lrk,0.553_lrk,-0.578_lrk,0.086_lrk, &
    & -0.131_lrk,0.915_lrk,-0.555_lrk,-0.26_lrk,-0.008_lrk,-0.26_lrk,0.869_lrk,-0.6_lrk,-0.099_lrk,0.961_lrk, &
    & -1.744_lrk,0.878_lrk,-0.022_lrk,-0.038_lrk,0.165_lrk,-0.106_lrk,0.491_lrk,-3.633_lrk,8.318_lrk,-5.176_lrk, &
    & 0.04_lrk,-0.217_lrk,0.389_lrk,-0.212_lrk,-0.769_lrk,4.593_lrk,-6.392_lrk,2.568_lrk,0.004_lrk,0.086_lrk, &
    & -0.121_lrk,0.031_lrk,-0.769_lrk,7.404_lrk,-16.104_lrk,9.469_lrk,-0.004_lrk,0.04_lrk,-0.012_lrk,-0.031_lrk/
!
!     check group index
  if (ni .lt. 1 .or. ni .gt. 11) then
!       invalid option
    call elmsge(1,6,nmm)
  end if
!     line index
  li = ifidxof(ni,ki)
  do i = 1,3
    kr(i) = k(li,i)
  end do
!
  return
!
end subroutine steqcef
!
!     ==================================================================
!>    @brief get value of stress concentration
!
!>    @param[in] rr mean diameter (t) over fillet radius (r) = (t/r)
!>     or notch height (he) over fillet radius (re) = (he/re)
!>    @param[in] tt two times mean diameter (t) over bigger diameter
!>     (dd) = (2*t/dd) or two times notch height (he) over outer diamter
!>     (d0) = (2*he/d0)
!>    @param[in] gi group index
!>    \verbatim
!>    1 torsion stress concentration factor
!>    2 bending stress concentration factor, tr <= 2
!>    3 bending stress concentration factor, tr > 2
!>    4 tension stress concentration factor, tr <= 2
!>    5 tension stress concentration factor, tr > 2
!>    6 stress concentration factor - u-notch,
!>    torsion stress concentration factor, he/re <= 2
!>    7 torsion stress concentration factor, he/re > 2
!>    8 bending stress concentration factor, he/re <= 2
!>    9 bending stress concentration factor, he/re > 2
!>    10 tension (axial) stress concentration factor, he/re <= 2
!>    11 tension (axial) stress concentration factor, he/re > 2
!>    \endverbatim
!>    @return value of stress concentration
!
real(lrk) function stcoef(rr,tt,gi)
  use rd_kinds, only: lrk
  implicit none
!
  integer :: gi
  real(lrk) :: rr, tt
!
  integer :: i
  real(lrk) :: ec, cr, vr
  character(len=6) :: nmm
  parameter(nmm = 'stcoef')
  dimension ec(3),cr(4)
!
  intrinsic :: sqrt
!
!     get coefficients -> ec
  do i = 1,4
    call steqcef(i,gi,ec)
!      power equation coefficients
    cr(i) = ec(1)+ec(2)*sqrt(rr)+ec(3)*rr
  end do
!     power equation
  vr = cr(1)+cr(2)*tt+cr(3)*tt**2+cr(4)*tt**3
!     check for values less than one
  if (vr .lt. 1) then
!       invalid option
    call elmsge(1,6,nmm)
  end if
!
  stcoef = vr
!
  return
!
end function stcoef
!
!     ==================================================================
!>    @brief stress concentration due to keyway proximity
!
!>    @param[in] du outer diameter (m)
!>    @param[in] sl smaller distance between keyway and step change (m)
!>    @param[in] fr step change concordance radius (fillet) (m)
!>    @param[in] kd kind = 1:step change, 2:keyway "A"
!>    @param[out] wr warning level 6
!>    @return stress concentration due to keyway proximity
!
real(lrk) function kpstf(du,sl,fr,kd,wr)
  use rd_kinds, only: lrk
  implicit none
!
  integer :: kd, wr
  real(lrk) :: du, sl, fr
!
  integer :: w
  real(lrk) :: kf, nl, dm, r
  character(len=5) :: nmm
  dimension kf(2)
!     kf = correction factors
  parameter (nmm = 'kpstf',kf = (/1.3_lrk,1.1_lrk/))
!
!     check kind
  if (kd .ne. 1 .and. kd .ne. 2) then
!       invalid option
    call elmsge(1,6,nmm)
  end if
!     warning level 6
  w = 0
!     near limit
  nl = du/10
!     dimension to check
  dm = sl-fr
!     check
  if (dm .lt. nl) then
    r = kf(kd)
    if(dm .le. 0) then
!         should be a warning
      call elmsgw(0,33,nmm)
!         warning level 6
      w = 262144
    end if
  else
    r = 1
  end if
!     warning level 6
  wr = w
!     keyway proximity factor
  kpstf = r
!
end function kpstf
!
!     ==================================================================
!>    @brief function 3 - crack sensitivity factor
!
!>    @param[in] fr concordance radius (fillet) (m)
!>    @param[in] su section ultimate strength (Pa)
!>    @param[out] wr warning level 0
!>    @return crack sensitivity factor
!
real(lrk) function cksnf(fr,su,wr)
  use rd_kinds, only: lrk
  implicit none
!
  integer :: wr
  real(lrk) :: fr, su
!
  real(lrk) :: us, r1, rq, pa2lb, m2in, r, icfvlf
!     pa2lb = unit conversion Pa to lbf/in^2
!     m2in = unit conversion m to in
  parameter (pa2lb = 0.000145038_lrk,m2in = 39.37007874_lrk)
!
  intrinsic :: sqrt
!
!     units transformation (S.I. to English)
!     Pa to lbf/in^2
  us = su*pa2lb
!     m to in
  r1 = fr*m2in
!     evaluate function, wr -> warning level 0
  rq = icfvlf(1,us,wr)
!     crack sensitivity factor calculation
  r = 1/(1+rq/sqrt(r1))
!
  cksnf = r
!
end function cksnf
!
!     ==================================================================
!>    @brief function 4 - stress concentration factor - step changing
!
!>    @param[in] du section outer diameter (m)
!>    @param[in] dn section next step diameter (m)
!>    @param[in] fr concordance radius (fillet, m)
!>    @param[in] qq crack sensitivity factor
!>    @param[out] kos stress concentration factors
!>    \verbatim
!>    kos(1) torsion stress concentration factor
!>    kos(2) bending stress concentration factor
!>    kos(3) tension stress concentration factor
!>    \endverbatim
!>    @param[out] tr step changing height
!>    @param[out] wr warning level 4
!
subroutine scstf(du,dn,fr,qq,kos,tr,wr)
  use rd_kinds, only: lrk
  implicit none
!
  integer :: wr
!     stress concentration factors
  real(lrk) :: du, dn, fr, qq, kos, tr
!
  integer :: gg, w, iiff
  real(lrk) :: etp, lm, gd, rr, tt, kf, riff, stcoef
  dimension kos(4),lm(2)
  character(len=3) :: c3
  character(len=5) :: nmm
!     etp = extrapolation allowed pu
!     lm = minimum / maximum limits
  parameter (c3 = 'h/r',nmm = 'scstf',etp = 0.15_lrk,lm = (/0.1_lrk*(1-etp),20*(1+etp)/))
!
  intrinsic :: abs
!
!     warning level 4
  w = 0
!     step changing height
  tr = abs(du-dn)/2
!     definition of the greater diameter
  gd = riff(du .gt. dn,du,dn)
  rr = tr/fr
  tt = 2*tr/gd
!     calculation limitation warning
  if (rr .lt. lm(1) .or. rr .gt. lm(2)) then
!       value out of expected range
    call elmsgow(0,33,nmm,c3)
!       warning level 4
    w = 4096
  end if
!     function coefficients
!     torsion stress concentration factor
  kf = stcoef(rr,tt,1)
  kos(1) = 1+qq*(kf-1)
!     bending stress concentration factor
  gg = iiff (rr .le. 2,2,3)
  kf = stcoef(rr,tt,gg)
  kos(2) = 1+qq*(kf-1)
!     tension stress concentration factor
  gg = iiff (rr .le. 2,4,5)
  kf = stcoef(rr,tt,gg)
  kos(3) = 1+qq*(kf-1)
  kos(4) = 0
!     warning level 4
  wr = w
!
  return
!
end subroutine scstf
!
!     ==================================================================
!>    @brief function 5 - stress concentration factor - u-notch
!
!>    @param[in] rr he/re, he notch height (m), re fillet radius (m)
!>    @param[in] tt 2*he/do, he notch height (m), do section outer diame
!>    @param[in] qq crack sensitivity factor
!>    @param[out] kos stress concentration factors
!>    \verbatim
!>    kos(1) = torsion stress concentration factor
!>    kos(2) = bending stress concentration factor
!>    kos(3) = tension (axial) stress concentration factor
!>    kos(4) = 0
!>    \endverbatim
!>    @param[out] wr warning level 5
!
subroutine grstf(rr,tt,qq,kos,wr)
  use rd_kinds, only: lrk
  implicit none
!
  integer :: wr
  real(lrk) :: rr, tt, qq, kos
!
  integer :: gg, w, iiff
  real(lrk) :: etp, lm, kf, stcoef
  character(len=5) :: nmm
  dimension kos(4),lm(2)
!     etp = extrapolation allowed pu
!     lm = minimum / maximum limits
  parameter (nmm = 'grstf',etp = 0.15_lrk,lm = (/0.25_lrk*(1-etp),50*(1+etp)/))
!
!     warning level 5
  w = 0
!     calculation limitation warning
  if (rr .lt. lm(1) .or. rr .gt. lm(2)) then
!       should be a warning
!       'value out of expected range' !33
    call elmsgw(0,33,nmm)
!       warning level 5
    w = 32768
  end if
!     torsion stress concentration factor
  gg = iiff(rr .le. 2,6,7)
  kf = stcoef(rr,tt,gg)
  kos(1) = 1+qq*(kf-1)
!     bending stress concentration factor
  gg = iiff(rr .le. 2,8,9)
  kf = stcoef(rr,tt,gg)
  kos(2) = 1+qq*(kf-1)
!     tension (axial) stress concentration factor
  gg = iiff (rr .le. 2,10,11)
  kf = stcoef(rr,tt,gg)
  kos(3) = 1+qq*(kf-1)
  kos(4) = 0
!     warning level 5
  wr = w
!
  return
!
end subroutine grstf
!
!     ==================================================================
!>    @brief function 6.1 - stress concentration factor - keyway point A
!
!>    @param[out] kos stress concentration factors
!>    \verbatim
!>    kos(1) = torsion stress concentration factor
!>    kos(2) = bending stress concentration factor
!>    kos(3) = tension (axial) stress concentration factor
!>    kos(4) = 0
!>    \endverbatim
!
subroutine kwastf(kos)
  use rd_kinds, only: lrk
  implicit none
!
  real(lrk) :: kos
  dimension kos(4)
!
!     torsion stress concentration factor
  kos(1) = 1.7_lrk
!     bending stress concentration factor
  kos(2) = 1.6_lrk
!     tension (axial) stress concentration factor
  kos(3) = 1.5_lrk
  kos(4) = 0
!
  return
!
end subroutine kwastf
!
!     ==================================================================
!>    @brief function 6.2 - stress concentration factor - keyway point B
!
!>    @param[in] du section outer diameter (m)
!>    @param[in] rc keyway concordance radius (m)
!>    @param[in] qq crack sensitivity factor
!>    @param[out] kos stress concentration factors
!>    \verbatim
!>    kos(1) = torsion stress concentration factor
!>    kos(2) = bending stress concentration factor
!>    kos(3) = tension (axial) stress concentration factor
!>    kos(4) = 0
!>    \endverbatim
!>    @param[out] wr warning level 7
!
subroutine kwbstf(du,rc,qq,kos,wr)
  use rd_kinds, only: lrk
  implicit none
!
  integer :: wr
  real(lrk) :: du, rc, qq, kos
!
  character(len=3) :: c3
  character(len=6) :: nmm
  integer :: w
  real(lrk) :: etp, lm, rr, cc, kf, riff
  dimension kos(4),lm(2)
!     etp = extrapolation allowed pu
!     lm = minimum / maximum limits
  parameter (c3 = 'r/d',nmm = 'kwbstf',etp = 0.15_lrk,lm = (/0.005_lrk,0.04_lrk/))
!     statement functions
!
!     warning level 7
  w = 0
  rr = rc/du
  cc = 1/rr/10
!     calculation limitation warning
  if (rr .lt. lm(1)*(1-etp) .or. rr .gt. lm(2)*(1+etp)) then
!       value out of expected range
    call elmsgow(0,33,nmm,c3)
!       warning level 7
    w = 2097152
  end if
!     torsion stress concentration factor
  kf = riff(rr .lt. lm(1),kr1(rr),kf1(cc))
  kos(1) = 1+qq*(kf-1)
!     bending stress concentration factor
  kf = riff(rr .lt. lm(1),kr2(rr),kf2(cc))
  kos(2) = 1+qq*(kf-1)
!     tension (axial) stress concentration factor
  kos(3) = 1.5_lrk
  kos(4) = 0
!     warning level 7
  wr = w
!
  return
!
contains

  !> former statement function kr1
  pure real(lrk) function kr1(rr)
    real(lrk), intent(in) :: rr
    kr1 = 0.7758_lrk*rr**(-0.307_lrk)
  end function kr1

  !> former statement function kf1
  pure real(lrk) function kf1(cc)
    real(lrk), intent(in) :: cc
    kf1 = 1.953_lrk+0.1434_lrk*cc-0.0021_lrk*cc**2
  end function kf1

  !> former statement function kr2
  pure real(lrk) function kr2(rr)
    real(lrk), intent(in) :: rr
    kr2 = 0.3874_lrk*rr**(-0.437_lrk)
  end function kr2

  !> former statement function kf2
  pure real(lrk) function kf2(cc)
    real(lrk), intent(in) :: cc
    kf2 = 1.426_lrk+0.1643_lrk*cc-0.0019_lrk*cc**2
  end function kf2

end subroutine kwbstf
!
!     ==================================================================
!>    @brief function 7 - Von-Mises equivalent stress
!
!>    @param[in] st torsion shearing stress (Pa)
!>    @param[in] sf bending normal stress (Pa)
!>    @param[in] sa axial normal stress (Pa)
!>    @param[in] kos stress concentration factors
!>    \verbatim
!>    kos(1) = torsion stress concentration factor
!>    kos(2) = bending stress concentration factor
!>    kos(3) = tension (axial) stress concentration factor
!>    \endverbatim
!>    @return Von-Mises equivalent stress
!
real(lrk) function vmesf(st,sf,sa,kos)
  use rd_kinds, only: lrk
  implicit none
!
  real(lrk) :: st, sf, sa, kos
  dimension kos(4)
!
  real(lrk) :: va, vb, r
  intrinsic :: sqrt
!
  va = kos(3)*sa+kos(2)*sf
  vb = kos(1)*st
  r = sqrt(va**2+3*vb**2)
  vmesf = r
!
  return
!
end function vmesf
!
!     ==================================================================
!>    @brief fatigue temperature factor (kd)
!
!>    @param[in] tk section temperature (K)
!>    @return temperature factor
!
real(lrk) function ktf(tk)
  use rd_kinds, only: lrk
  implicit none
!
  real(lrk) :: tk
!
  real(lrk) :: tl, r
!     tl = limiting temperature (K)
  parameter (tl = 344.4_lrk)
!
  if (tk .gt. tl) then
    r = tl/tk
  else
    r = 1
  end if
  ktf = r
!
  return
!
end function ktf
!
!     ==================================================================
!>    @brief function 7 - fatigue limit correction factors
!
!>    @param[in] su section ultimate strength (Pa)
!>    @param[in] du section outer diameter (m)
!>    @param[in] rr reliability (%)
!>    @param[in] tk section temperature (K)
!>    @param[in] is section surface finishing (according to table)
!>    \verbatim
!>    1 mirror polished
!>    2 rectified
!>    3 machined / cold rolled
!>    4 machined / hot rolled
!>    5 forged
!>    \endverbatim
!>    @param[out] ka surface factor
!>    @param[out] kb sizing factor
!>    @param[out] kc reliability factor
!>    @param[out] kd temperature factor
!>    @param[out] wr warnig level 3
!>    @return fatigue limit correction factor
!
real(lrk) function ftlmcf(su,du,rr,tk,is,ka,kb,kc,kd,wr)
  use rd_kinds, only: lrk
  implicit none
!
  integer :: is, wr
  real(lrk) :: su, du, rr, tk, ka, kb, kc, kd
!
  character(len=6) :: nmm
  integer :: iw0, iw1, iw2
  real(lrk) :: ks, kk, mm, sm, r, icfvlf, ktf
  dimension ks(2)
  parameter(nmm = 'ftlmcf',ks = (/1.0_lrk,0.89_lrk/),kk = 1e3_lrk,mm = 1e6_lrk)
!
!     convert to MPa
  sm = su/mm
  if (is .eq. 1) then
    ka = ks(1)
  else if (is .eq. 2) then
    ka = ks(2)
  else if (is .eq. 3) then
    ka = icfvlf(2,sm,iw0)
  else if (is .eq. 4) then
    ka = icfvlf(3,sm,iw0)
  elseif (is .eq. 5) then
    ka = icfvlf(4,sm,iw0)
  else
!       invalid option
    call elmsge(1,6,nmm)
  end if
!     sizing factor, meter to mm
  sm = du*kk
  kb = icfvlf(5,sm,iw1)
!     reliability factor
  kc = icfvlf(6,rr,iw2)
!
!     temperature factor
  kd = ktf(tk)
!
!     fatigue limit correction factor
  r = ka*kb*kc*kd;
!
  ftlmcf = r
!     warnig level 3
  wr = iw0*8+iw1*64+iw2*512
!
  return
!
end function ftlmcf
!
!     ==================================================================
!>    @brief function 8 - fatigue limit calculation
!
!>    @param[in] su section ultimate strength (Pa)
!>    @param[in] nf expected life cycles
!>    @param[in] ks fatigue limit correction factor
!>    @return fatigue limit
!
real(lrk) function ftlmf(su,nf,ks)
  use rd_kinds, only: lrk
  implicit none

  real(lrk) :: su, nf, ks
!
  real(lrk) :: sf, sl, sn, nc, xc, mc, sr, se, sm, zz, aa, bb, r
!     sf = strength factor
!     sl = limit strength
!     sn = minimum strength
!     nc = minimum number of cycles
!     xc = maximum number of cycles
!     mc = correction for minimum number of cycles
  parameter(sf = 0.5_lrk,sl = 1.4E9_lrk,sn = sl*sf,nc = 1e3_lrk,xc = 1e6_lrk,mc = 0.9_lrk)
!
!     fatigue limit without correction
  if (su .le. sl) then
    sr = sf*su
  else
    sr = sn
  end if
!     fatigue limit corrected
  se = sr*ks
!     fatigue limit for 1000 cycles
  sm = mc*su
  zz = log(1/nc)
  if (nf .lt. nc) then
    bb = 1/zz*log(su/sm)
    aa = su/1**bb
  else if (nf .ge. xc) then
    aa = se
    bb = 0
  else
    bb = 1/zz*log(sm/se)
    aa = sm/nc**bb
  end if
!
  r = aa*nf**bb
  ftlmf = r
!
  return
!
end function ftlmf
!
!     ==================================================================
!>    @brief function 9 - safety factor for fatigue
!
!>    @param[in] sy section yield strength (Pa)
!>    @param[in] su section ultimate strength (Pa)
!>    @param[in] sn fatigue resistence (Pa)
!>    @param[in] sa alternating stress (Pa)
!>    @param[in] sm average stress (Pa)
!>    @param[out] sf safety factors for fatigue
!>    \verbatim
!>    sf(1) Goodman
!>    sf(2) ASME curve
!>    \endverbatim
!
subroutine ftsff(sy,su,sn,sa,sm,sf)
  use rd_kinds, only: lrk
  implicit none
!
  real(lrk) :: sy, su, sn, sa, sm, sf
  dimension sf(2)
!
  character(len=5) :: nmm
  real(lrk) :: aa, bb, cc, dn
  parameter(nmm = 'ftsff')
!
  intrinsic :: sqrt
!
  if (sn .le. 0 .or. su .le. 0 .or. sy .le. 0) then
!       invalid option
    call elmsge(1,6,nmm)
  end if
  aa = sa/sn
  bb = sm/su
  cc = sm/sy
!     Goodman
  dn = aa+bb
  sf(1) = 1/dn
!     ASME curve
  dn = aa**2+cc**2
  sf(2) = 1/sqrt(dn)
!
  return
!
end subroutine ftsff
!
!     ==================================================================
!>    @brief get fillet radius for a section index
!
!>    @param[in] si section index
!>    @param[in] ntfil number of section fillets
!>    @param[in] mxtfil dimension fillet data vectors
!>    @param[in] tfilid section ids vector
!>    @param[in] tfilrd section fillets
!>    @return fr fillet radius or zero
!
real(lrk) function gfirdf(si,ntfil,mxtfil,tfilid,tfilrd)
  use rd_kinds, only: lrk
  implicit none
!
  integer :: si, ntfil, mxtfil, tfilid
  real(lrk) :: tfilrd
  dimension tfilid(mxtfil),tfilrd(mxtfil)
!
  integer :: jj, fi
  real(lrk) :: fr
!
!     fillet radius
  fr = 0
!     loop on fillets
  do jj = 1,ntfil
!       fillet index
    fi = tfilid(jj)
!       check index
    if (fi .eq. si) then
!         get fillet radius
      fr = tfilrd(jj)
!         out of loop
      exit
    end if
!       fillets loop
  end do
!     fillet radius
  gfirdf = fr
!
  return
!
end function gfirdf
!
!     ==================================================================
!>    @brief check if given section index is in notch indices vector
!
!>    @param[in] si section index
!>    @param[in] nnt number of notches
!>    @param[in] mxscls dimension of section values vectors
!>    @param[in] nsi notch section indices
!>    @return index if true zero otherwise
!
integer function isntcf(si,nnt,mxscls,nsi)
  implicit none
!
  integer :: si, nnt, mxscls, nsi
  dimension nsi(mxscls)
!
  integer :: ii, r, ni

  r = 0
!     loop on notch
  do ii = 1,nnt
    ni = nsi(ii)
!       check if section index is notch
    if (si .eq. ni) then
      r = ii
!         out of loop
      exit
    end if
  end do
  isntcf = r
!
  return
!
end function isntcf
!
!     ==================================================================
!>    @brief get notch dimensions or zero.
!
!>    @param[in] si section index
!>    @param[in] cs number of sections
!>    @param[in] mxs dimension of shaft data vector
!>    @param[in] dd shaft outer diameter vector (m)
!>    @param[in] nnt number of notch
!>    @param[in] mxscls dimension of section values vectors
!>    @param[in] nsi notch section indices
!>    @param[in] ntfil number of section fillets
!>    @param[in] mxtfil dimension fillet data vectors
!>    @param[in] tfilid section ids vector
!>    @param[in] tfilrd section fillets
!>    @param[out] nr notch fillet radius
!>    @param[out] hr notch height (he) over fillet radius (re) = (he/re)
!>    @param[out] hd two times notch height (he) over
!>     outer diameter (d0) = (2*he/d0)
!>    @param[out] nh notch height
!
subroutine gntdm(si,&
&cs,mxs,dd,&
&nnt,mxscls,nsi,&
&ntfil,mxtfil,tfilid,tfilrd,&
&nr,hr,hd,nh)
  use rd_kinds, only: lrk
  implicit none
!
  integer :: si, cs, mxs, nnt, mxscls, nsi, ntfil, mxtfil, tfilid
  real(lrk) :: dd, tfilrd, gfirdf, nr, hr, hd, nh
  dimension dd(mxs),&
  &nsi(mxscls),&
  &tfilid(mxtfil),tfilrd(mxtfil)
!
  character(len=5) :: nmm
  parameter (nmm = 'gntdm')
  integer :: kn, isntcf
  real(lrk) :: da, db, dm, dn
!
  intrinsic :: max
!
!     notch fillet radius
  nr = 0
!     notch height (he) over fillet radius (re) = (he/re)
  hr = 0
!     two times notch height (he) over outer diameter (d0) = (2*he/d0)
  hd = 0
!     check if given section index is in notch indices vector
  kn = isntcf(si,nnt,mxscls,nsi)
!     loop on notch index vector
  if (kn .gt. 0) then
!       get notch fillet radius
    nr = gfirdf(si,ntfil,mxtfil,tfilid,tfilrd)
    if (nr .le. 0) then
!         invalid data stop
      call elmsge(1,8,nmm)
    end if
!       diameter before index
    if (si .gt. 1) then
      db = dd(si-1)
    else
      db = 0
    end if
!       diameter after index
    if (si .lt. cs) then
      da = dd(si+1)
    else
      da = 0
    end if
!       maximum notch before/after outer diameter
    dm = max(db,da)
!       notch inner diameter on index
    dn = dd(si)
!       notch height
    nh = (dm-dn)/2
!       check if notch height is positive
    if (nh .le. 0) then
!         invalid data, stop
      call elmsge(1,8,nmm)
    end if
!       notch height (he) over concordance radius (re) = (he/re)
    hr = nh/nr
!       two times notch height (he) over outer diameter (d0) = (2*he/d0)
    hd = 2*nh/dm
  end if
!
  return
!
end subroutine gntdm
!     ==================================================================
!>    @brief check if given section index is in key indices vector
!
!>    @param[in] si section index
!>    @param[in] nks number of keys
!>    @param[in] mxscls dimension of section values vectors
!>    @param[in] nki sections with keyway index vector
!>    @return index if passed section index is on key list zero otherwis
!
integer function haskwf(si,nks,mxscls,nki)
  implicit none

  integer :: si, nks, mxscls, nki
  dimension nki(mxscls)
!
  integer :: r, ii, ki
!
  r = 0
  do ii = 1,nks
!       key index
    ki = nki(ii)
!       check for key on current section
    if (ki .eq. si) then
      r = ii
!         out of loop
      exit
    end if
  end do
  haskwf = r
!
  return
!
end function haskwf
!
!     ==================================================================
!>    @brief get key dimensions for a section index or empty
!
!>    @param[in] si section index
!>    @param[in] ki key index, zero to force search
!>    @param[in] nks number of keys
!>    @param[in] mxscls dimension of section values vectors
!>    @param[in] nki sections with keyway index vector
!>    @param[in] kvl key values matrix
!>    @param[out] kvo key dimensions
!>    \verbatim
!>    1) keyway milling bottom fillet radius (m)
!>    2) keyway straight same height (m)
!>    3) keyway straight same height and width length (m)
!>    4) distance from keyway straight length start, up to left section
!>    5) keyway straight same width (m)
!>    6) sled runner keyway milling radius (m)
!>    \endverbatim
!
subroutine gkydm(si,ki,nks,mxscls,nki,kvl,kvo)
  use rd_kinds, only: lrk
  implicit  none
!
  integer :: si, ki, nks, mxscls, nki
  real(lrk) :: kvl, kvo
  dimension nki(mxscls),kvl(mxscls,6),kvo(6)
!
  integer :: ii, haskwf
!
!     initialize output
  do ii = 1,6
    kvo(ii) = 0
  end do
!     check for passed key index
  if (ki .le. 0) then
!       check if given section index is in key indices vector
    ki = haskwf(si,nks,mxscls,nki)
  end if
!     check for index
  if (ki .gt. 0) then
!       key on current section
!       get keyway dimensions
    do ii = 1,6
      kvo(ii) = kvl(ki,ii)
    end do
  end if
!
  return
!
end subroutine gkydm
!
!     ==================================================================
!>    @brief check previous and next section
!
!>    @param[in] si current section index
!>    @param[in] cs number of sections
!>    @param[in] mxs dimension of shaft data vector
!>    @param[in] ps section positions vector (m)
!>    @param[in] dd section outer diameter vector (m)
!>    @param[out] ni next section index
!>    @param[out] pi previous section index
!>    @param[out] no next section outer diameter
!>    @param[out] po previous section outer diameter
!>    @param[out] sl section length
!
subroutine chksec(si,cs,mxs,ps,dd,ni,pi,no,po,sl)
  use rd_kinds, only: lrk
  implicit none
!
  integer :: si, cs, mxs, ni, pi
  real(lrk) :: ps, dd, no, po, sl
  dimension ps(mxs),dd(mxs)
!
!     initialize output data
  sl = 0
!     check next section index
  if (si .lt. cs) then
!       has (n)ext section index
    ni = si+1
!       next outer diameter
    no = dd(ni);
  else
    ni = 0
    no = 0
  end if
!     check previous section index
  if (si .gt. 1) then
!       has (p)revious section index
    pi = si-1
!       previous outer diameter
    po = dd(pi)
!       section length
    sl = ps(si)-ps(pi)
  else
    pi = 0
    po = 0
    sl = ps(1)
  end if
!
  return
!
end subroutine chksec
!
!     ==================================================================
!>    @brief check key on section length
!
!>    @param[in] sl section length
!>    @param[in] kv keyway dimensions:
!>    \verbatim
!>    1) distance from keyway straight length start, up to left section
!>    2) keyway straight same height (m)
!>    3) keyway straight same height and width length (m)
!>    4) keyway milling bottom fillet radius (m)
!>    5) keyway straight same width (m)
!>    6) sled runner keyway milling radius (m)
!>    \endverbatim
!>    @return true if keyway less or equals than section length
!
logical function chkkwf(sl,kv)
  use rd_kinds, only: lrk
  implicit none
!
  real(lrk) :: sl, kv
  dimension kv(6)
!
  real(lrk) :: kl
!
!     check for sled runner key
  if (kv(6) .eq. 0) then
!       round key
!       keyway length
    kl = kv(3)+kv(5)
  else
!      sled runner key
    kl = kv(3)
  end if
!     return check if keyway fit on section
  chkkwf = sl .gt. kl
!
  return
!
end function chkkwf
!
!     ==================================================================
!>    @brief keyway step change and proximity factors
!
!>    @param[in] pi previous section index
!>    @param[in] si current section index
!>    @param[in] prec precision on diameters comparison (m)
!>    @param[in] po previous section outer diameter
!>    @param[in] od current section outer diameter
!>    @param[in] no next section outer diameter
!>    @param[in] su section ultimate strength (Pa)
!>    @param[in] sl section length
!>    @param[in] kv keyway dimensions:
!>    \verbatim
!>    1) distance from keyway straight length start, up to left section
!>    2) keyway straight same height (m)
!>    3) keyway straight same height and width length (m)
!>    4) keyway milling bottom fillet radius (m)
!>    5) keyway straight same width (m)
!>    6) sled runner keyway milling radius (m)
!>    \endverbatim
!>    @param[in] ntfil number of section fillets
!>    @param[in] mxtfil dimension fillet data vectors
!>    @param[in] tfilid section ids vector
!>    @param[in] tfilrd section fillets
!
!>    @param[out] scl step changing at left
!>    @param[out] scr step changing at right
!>    @param[out] sk smaller distance between keyway and step change (m)
!>    @param[out] qq crack sensitivity factor
!>    @param[out] fr fillet radius to next section
!>    @param[out] tr step changing height next section
!>    @param[out] kos stress concentration factor with proximity:
!>    \verbatim
!>    1) torsion stress concentration factor
!>    2) bending stress concentration factor
!>    3) tension stress concentration factor
!>    4) stress concentration due to keyway proximity
!>    \endverbatim
!>    @param[out] wr warning level 4
!
subroutine kwscpf(pi,si,prec,po,od,no,su,sl,kv,&
&ntfil,mxtfil,tfilid,tfilrd,&
&scl,scr,sk,qq,fr,tr,kos,wr)
  use rd_kinds, only: lrk
  implicit none
!
  integer :: pi, si, ntfil, mxtfil, tfilid, wr
  real(lrk) :: prec, po, od, no, su, sl, kv, tfilrd, sk, qq, fr, tr, kos
  logical :: scl, scr
  dimension tfilid(mxtfil),tfilrd(mxtfil),&
  &kv(6),kos(4)
!
  integer :: ii, w
  real(lrk) :: kch, kob, kpstf, cksnf, gfirdf
  character(len=6) :: nmm
  dimension  kob(4)
  parameter (nmm = 'kwscpf')
!
!     initiliaze
  sk = 0
  qq = 0
  fr = 0
  tr = 0
!     warning level 4
  wr = 0
  do ii = 1,4
    kos(ii) = 0
  end do
!
!     step changing at right
!     next outer diameter is greater than current
  scr = no .gt. od+prec
!
!     step changing at left
!     previous outer diameter is greater than current
  scl = po .gt. od+prec
!
!     check  step changing at right
  if(scr) then
!       step changing at right
!
!       get fillet radius for a section index
    fr = gfirdf(si,ntfil,mxtfil,tfilid,tfilrd)
    if (fr .eq. 0) then
!         invalid data stop
      call elmsge(1,8,nmm)
    end if
!
!       sk smaller distance between keyway and step change (m)
!       check for sled runner key
    if (kv(6) .eq. 0) then
!         round key
      sk = sl-kv(1)-kv(3)-kv(5)/2
    else
!         sled runner key
      sk = sl-kv(1)-kv(3)
    end if
!
!       stress concentration due to keyway proximity
    kch = kpstf(od,sk,fr,1,w)
!       w warning level 6
    wr = wr+w
!
!       crack sensitivity factor
    qq = cksnf(fr,su,w)
!       w warning level 0
    wr = wr+w
!
!        stress concentration factor - step changing, right
    call scstf(od,no,fr,qq,kob,tr,w)
!       w warning level 4
    wr = wr+w
!       apply proximity factor
    do ii = 1,3
      kos(ii) = kob(ii)*kch
    end do
    kos(4) = kch
!
  end if
!
!     check  step changing at left and not at right
  if (scl .and. .not. scr) then
!       step changing at left
!
!       get fillet radius for a section index
    fr = gfirdf(pi,ntfil,mxtfil,tfilid,tfilrd)
!       fr fillet radius or zero
    if (fr .eq. 0) then
!         invalid data stop
      call elmsge(1,8,nmm)
    end if
!
!       sk smaller distance between keyway and step change (m)
!       check for sled runner key
    if (kv(6) .eq. 0) then
!         round key
      sk = kv(1)-kv(5)/2
    else
!         sled runner key
      sk = kv(1)
    end if
!
!       stress concentration due to keyway proximity
    kch = kpstf(od,sk,fr,1,w)
!       w warning level 6
    wr = wr+w
!
!       crack sensitivity factor
    qq = cksnf(fr,su,w)
!       w warning level 0
    wr = wr+w
!
!       stress concentration factor - step changing, left
    call scstf(od,po,fr,qq,kob,tr,w)
!       kob stress concentration factors
!        kob(1) torsion stress concentration factor
!        kob(2) bending stress concentration factor
!        kob(3) tension stress concentration factor
!       tr step changing height
!       w warning level 4
    wr = wr+w
!
!       apply proximity factor
    do ii = 1,3
      kos(ii) = kob(ii)*kch
    end do
    kos(4) = kch
!       kos(1) torsion stress concentration factor
!       kos(2) bending stress concentration factor
!       kos(3) tension stress concentration factor
!       kos(4) stress concentration due to keyway proximity
!
  end if
!
  return
!
end subroutine kwscpf
!
!     ==================================================================
!>    @brief step change stress concentration factors
!
!>    @param[in] scl step changing at left
!>    @param[in] scr step changing at right
!>    @param[in] pi previous section index
!>    @param[in] si current section index
!>    @param[in] ni next section index
!>    @param[in] po previous section outer diameter
!>    @param[in] od current section outer diameter
!>    @param[in] no next section outer diameter
!>    @param[in] su section ultimate strength (Pa)
!
!>    @param[in] ntfil number of section fillets
!>    @param[in] mxtfil dimension fillet data vectors
!>    @param[in] tfilid section ids vector
!>    @param[in] tfilrd section fillets
!
!>    @param[out] otx step change in the other side of keyway
!>     0 = no, 1 = right, 2 = left, 3 = both
!>    @param[out] fr (x) fillet radius for next section vector
!>    @param[out] tr (x) step changing height next section vector
!>    @param[out] qc (x) crack sensitivity factor vector
!>    @param[out] kos (4,x) torsion stress concentration factors matrix
!>    \verbatim
!>    at right x = 1, at left x = 2.
!>    1,x) torsion stress concentration factor
!>    2,x) bending stress concentration factor
!>    3,x) tension stress concentration factor
!>    4,x) stress concentration due to keyway proximity
!>    \endverbatim
!>    @param[out] wr warning level 3
!
subroutine kwnstch(scl,scr,pi,si,ni,po,od,no,su,&
&ntfil,mxtfil,tfilid,tfilrd,&
&otx,fr,tr,qc,kos,wr)
  use rd_kinds, only: lrk
  implicit none
!
  logical :: scl, scr
  integer :: pi, si, ni, ntfil, mxtfil, tfilid, wr
  real(lrk) :: po, od, no, su, tfilrd, otx, fr, tr, qc, kos
  dimension tfilid(mxtfil),tfilrd(mxtfil),&
  &fr(2),tr(2),qc(2),kos(4,2)
!
  integer :: ii, jj, w
  real(lrk) :: kob, sc, cksnf, gfirdf
  character(len=7) :: nmm
  dimension  kob(4)
  parameter (nmm = 'kwnstch')
!
!     initiliaze
!     warning level 3
  wr = 0
!     step changes: 0 = no, 1 = right, 2 = left, 3 = both
  otx = 0._lrk
!     left and right
  do jj = 1,2
!       fillet radius for next section
    fr(jj) = 0
!       step changing height next section
    tr(jj) = 0
!       crack sensitivity factor
    qc(jj) = 0
    do ii = 1,4
      kos(ii,jj) = 0
    end do
  end do
!
!     check for next at left (scl)
  if (scl .and. ni .gt. 0) then
    otx = 2._lrk
!       get fillet radius for a section index
    fr(2) = gfirdf(si,ntfil,mxtfil,tfilid,tfilrd)
!       fr fillet radius or zero
    if (fr(2) .eq. 0) then
!         invalid data stop
      call elmsge(1,8,nmm)
    end if
!       crack sensitivity factor
    qc(2) = cksnf(fr(2),su,w)
!       w warning level 0
    wr = wr+w
!
!       stress concentration factor - step changing right -> kob,sc
    call scstf(od,no,fr(2),qc(2),kob,sc,w)
!       w warning level 4
    wr = wr+w
!
    tr(1) = sc
    do ii = 1,4
      kos(ii,2) = kob(ii)
    end do
!       kos stress concentration factors
!        kos(1) torsion stress concentration factor
!        kos(2) bending stress concentration factor
!        kos(3) tension stress concentration factor
!       tr step changing height next section
!
  end if
!
!     check for next at right (scr)
  if (scr  .and. pi .gt. 0) then
    otx = otx+1
!       get fillet radius for a section index
    fr(1) = gfirdf(pi,ntfil,mxtfil,tfilid,tfilrd)
    if (fr(1) .eq. 0) then
!         invalid data stop
      call elmsge(1,8,nmm)
    end if
!       crack sensitivity factor
    qc(1) = cksnf(fr(1),su,w)
!       w warning level 0
    wr = wr+w
!       stress concentration factor - step changing left -> kob,sc
    call scstf(od,po,fr(1),qc(1),kob,sc,w)
!       w warning level 4
    wr = wr+w
    tr(1) = sc
    do ii = 1,4
      kos(ii,1) = kob(ii)
    end do
!
  end if
!
  return
!
end subroutine kwnstch
!
!     ==================================================================
!>    @brief get fatigue factors on desired radius.
!>     correct bending and torsion stress to this radius.
!
!>    @param[in] dd section outer diameter
!>    @param[in] kh notch/keyway height
!>    @param[in] su section ultimate strength (Pa)
!>    @param[in] st section mean torque stress
!>    @param[in] bz section bending stress
!>    @param[in] rr  sections fatigue reliability
!>    @param[in] tk sections fatigue temperature (K)
!>    @param[in] sf section surface finishing
!>    @param[in] nf sections fatigue cycles expected life
!>    @param[in] as sections aditional safety margin
!>    @param[out] sg torque stress
!>    @param[out] bs bending stress
!>    @param[out] ka surface factor
!>    @param[out] kb sizing factor
!>    @param[out] kc reliability factor
!>    @param[out] kd temperature factor
!>    @param[out] sn fatigue limit calculation
!>    @param[out] wr warning level 3
!
subroutine stfcp(dd,kh,su,st,bz,rr,tk,sf,nf,as,&
&sg,bs,ka,kb,kc,kd,sn,wr)
  use rd_kinds, only: lrk
  implicit none
!
  integer :: sf, wr
  real(lrk) :: dd, kh, su, st, bz, rr, tk, nf, as, sg, bs, ka, kb, kc, kd, sn
!
  real(lrk) :: od, ro, nr, cf, kst, kf, ftlmcf, ftlmf
!
!     under keyway outer diameter
  od = dd-2*kh
!     current outer radius
  ro = od/2
!     new radius subtracting key height
  nr = ro-kh
!     correction factor, 1 if kh = 0
  cf = nr/ro
!     torsion stress (average)
  sg = cf*abs(st)
!     bending stress
  bs = cf*bz
!
!     fatigue limit correction factors -> ka,kb,kc,kd
  kst = ftlmcf(su,od,rr,tk,sf,ka,kb,kc,kd,wr)
!     kst fatigue limit correction factors
!     ka surface factor
!     kb sizing factor
!     kc reliability factor
!     kd temperature factor
!     wr warning level 3
!
!     fatigue limit correction factor
  kf = kst/as
!     fatigue limit calculation
  sn = ftlmf(su,nf,kf)
!     sn fatigue limit
!
  return
!
end subroutine stfcp
!
!     ==================================================================
!>    @brief get Von-Mises and safety for fatigue
!
!>    @param[in] al alternating torque, pu mean.
!>    @param[in] mx maxumum torque, pu mean.
!>    @param[in] sn fatigue limit
!>    @param[in] sg torsion stress (average)
!>    @param[in] bs bending stress
!>    @param[in] sy section yield strength
!>    @param[in] su section ultimate strength
!>    @param[in] kos stress concentration factor
!>    \verbatim
!>    1) torsion stress concentration factor
!>    2) bending stress concentration factor
!>    3) tension (axial) stress concentration factor
!>    \endverbatim
!>    @param[out] sa alternating torque
!>    @param[out] sx maximum torque
!>    @param[out] vm1 Von-Mises equivalent stress alternating
!>    @param[out] vm2 Von-Mises equivalent stress mean
!>    @param[out] vm3 Von-Mises equivalent stress maximum
!>    @param[out] sf1 safety factors for fatigue vector: Goodman
!>    @param[out] sf2 safety factors for fatigue vector: ASME
!
subroutine hcoef(al,mx,sn,sg,bs,sy,su,kos,&
&sa,sx,vm1,vm2,vm3,sf1,sf2)
  use rd_kinds, only: lrk
  implicit none
!
  real(lrk) :: al, mx, sn, sg, bs, sy, su, kos, sa, sx, vm1, vm2, vm3, sf1, sf2
  dimension kos(4)
!
  real(lrk) :: vmesf, sf
  dimension sf(2)
!
!     alternating torque
  sa = al*sg
!     maximum torque
  sx = mx*sg
!      Von-Mises equivalent stress
!      alternating
  vm1 = vmesf(sa,bs,0._lrk,kos)
!      mean
  vm2 = vmesf(sg,0._lrk,0._lrk,kos)
!      maximum
  vm3 = vmesf(sx,bs,0._lrk,kos)
!     safety factors for fatigue vector -> sf
  call ftsff(sy,su,sn,vm1,vm2,sf)
!     sf(1) = Goodman
  sf1 = sf(1)
!     sf(2) = ASME
  sf2 = sf(2)
!
  return
!
end subroutine hcoef
!
!     ==================================================================
!>    @brief prepare or get values to or from output data matrix line.
!
!>    @param[in] mxscls dimension numbero of columns output matrix
!>    @param[in] ix comlumn index
!>    @param[in,out] si 1) section index.
!>    @param[in,out] kt 2) kind:
!>    \verbatim
!>    1) notch
!>    2) keyway "A"
!>    3) kw "B"
!>    4) step change and kw
!>    5) step
!>    \endverbatim
!>    @param[in,out] sd 3) sides: 0) no meaning, 1)right, 2)left.
!>    @param[in,out] sy 4) yield strength.
!>    @param[in,out] su 5) ultimate stress.
!>    @param[in,out] st 6) torque stress.
!>    @param[in,out] bz 7) bending stress.
!>    @param[in,out] od 8) outer diameter.
!>    @param[in,out] sf 9) surface finishing.
!>    @param[in,out] sn 10) fatigue limit.
!>    @param[in,out] sa 11) alternating torque stress.
!>    @param[in,out] sg 12) mean torque stress.
!>    @param[in,out] sx 13) maximum torque stress.
!>    @param[in,out] bs 14) bending stress.
!>    @param[in,out] fr 15) fillet radius.
!>    @param[in,out] sh 16) step height.
!>    @param[in,out] qs 17) crack sensitivity.
!>    @param[in,out] ks 18) <br>
!>     10 stress concentration factor torsion.<br>
!>     2) stress concentration factor bend.<br>
!>     3) stress concentration factor axial.<br>
!>     4) stress concentration factor keyway proximity.
!>    @param[in,out] ka 22) fatigue correction factor surface.
!>    @param[in,out] kb 23) fatigue correction factor sizing.
!>    @param[in,out] kc 24) fatigue correction factor reliability.
!>    @param[in,out] kd 25) fatigue correction factor temperature.
!>    @param[in,out] vm1 26) Von-Mises equivalent stress, alternating.
!>    @param[in,out] vm2 27) Von-Mises equivalent stress, meaning.
!>    @param[in,out] vm3 28) Von-Mises equivalent stress, maximum.
!>    @param[in,out] sf1 29) fatigue safety factors "Goodman".
!>    @param[in,out] sf2 30) fatigue safety factor "ASME".
!>    @param[in,out] wi 31) warning octal code. see flxwarn
!>    @param[in,out] vo input/output data line
!
subroutine tovec(mxscls,ix,si,kt,sd,sy,su,st,bz,od,&
&sf,sn,sa,sg,sx,bs,fr,sh,qs,&
&ks,&
&ka,kb,kc,kd,vm1,vm2,vm3,sf1,sf2,wi,vo)
  use rd_kinds, only: lrk
  implicit none
!
  integer :: mxscls, ix, wi
  real(lrk) :: si, kt, sd, sy, su, st, bz, od, sf, sn, sa, sg, sx, bs, fr, sh, qs, ks, ka, kb, kc, kd, vm1, vm2, vm3, &
    & sf1, sf2, vo
  dimension ks(4),vo(31,mxscls)
!
  integer :: ii
!
  intrinsic :: real
!
  vo(1,ix) = si
  vo(2,ix) = kt
  vo(3,ix) = sd
  vo(4,ix) = sy
  vo(5,ix) = su
  vo(6,ix) = st
  vo(7,ix) = bz
  vo(8,ix) = od
  vo(9,ix) = sf
  vo(10,ix) = sn
  vo(11,ix) = sa
  vo(12,ix) = sg
  vo(13,ix) = sx
  vo(14,ix) = bs
  vo(15,ix) = fr
  vo(16,ix) = sh
  vo(17,ix) = qs
  do ii = 1,4
    vo(17+ii,ix) = ks(ii)
  end do
  vo(22,ix) = ka
  vo(23,ix) = kb
  vo(24,ix) = kc
  vo(25,ix) = kd
  vo(26,ix) = vm1
  vo(27,ix) = vm2
  vo(28,ix) = vm3
  vo(29,ix) = sf1
  vo(30,ix) = sf2
  vo(31,ix) = real(wi, lrk)
!
  return
!
end subroutine tovec
!
!     ==================================================================
!>    @brief check if given input if given number is less or equals
!>     passed input limit.
!>    if given input is greater than passed input limit the output
!>     message is set with "out of bounds" message text.
!
!>    @param[in] kk input value to check
!>    @param[in] mxc limit value
!>    @param[out] msg out of bound message text.
!>    @return true if given input is less or equals passed limit.
logical function ckmxcf(kk,mxc,msg)
  use rd_textfun, only: errmsgf
  implicit none
!
  integer :: kk, mxc
  character(len=*) :: msg
!     get error message

  ckmxcf = kk .le. mxc
  if (.not. ckmxcf) then
!       out of bounds
    msg = errmsgf(50, 4)
  end if
!
  return
!
end function ckmxcf
!
!     ==================================================================
!>    @brief calculates flexural torsion stress, fatigue figures and
!>     safety margin on sections.
!>    step change, keyway and notch are evaluated.<br>
!>    warning octal->decimal code. do not sum same level warning.
!>    see flxwarn on tsaidas.f
!
!>    @param[in] im torsion model index
!>    @param[in] pdm final [AA] matrix dimension
!>    @param[in] pp number of effective bearing supports
!>    @param[in] prv variable bearing time response
!>    @param[in] mxc dimension of columns output matrix vo,
!>     related to paramter mxscls
!>    @param[out] nc number of columns of output data matrix
!>    @param[out] as sections aditional safety margin
!>    @param[out] tfa torsion stress factor
!>    @param[out] bfa bend stress factor
!>    @param[out] vo output data matrix
!>    \verbatim
!>    1) si section index.
!>    2) kt: kind=1)notch,2)keyway "A",3)kw "B",4)step change+kw,5)step
!>    3) sd: sides: 0) no meaning, 1)right, 2)left.
!>    4) sy: yield strength.
!>    5) su: ultimate stress.
!>    6) st: torque stress.
!>    7) bz: bending stress.
!>    8) od: outer diameter.
!>    9) sf: surface finishing.
!>    10) sn: fatigue limit.
!>    11) sa: alternating torque stress.
!>    12) sg: mean torque stress.
!>    13) sx: maximum torque stress.
!>    14) bs: bending stress.
!>    15) fr: fillet radius.
!>    16) sh: step height.
!>    17) qs: crack sensitivity.
!>    18) ks(1): stress concentration factor torsion.
!>    19) ks(2): stress concentration factor bend.
!>    20) ks(3): stress concentration factor axial.
!>    21) ks(4): stress concentration factor keyway proximity.
!>    22) ka: fatigue correction factor surface.
!>    23) kb: fatigue correction factor sizing.
!>    24) kc: fatigue correction factor reliability.
!>    25) kd: fatigue correction factor temperature.
!>    26) vm1: Von-Mises equivalent stress, alternating.
!>    27) vm2: Von-Mises equivalent stress, meaning.
!>    28) vm3: Von-Mises equivalent stress, maximum.
!>    29) sf1: fatigue safety factors "Goodman".
!>    30) sf2: fatigue safety factor "ASME".
!>    31) wi: warning octal code.
!>    \endverbatim
!>    @see flxwarn
!>    @param[out] errmsg return an error message
!>    @param[out] ok return flag. unsuccessful if <0.
!
subroutine tflxt(im,pdm,pp,prv,mxc,nc,as,&
&tfa,bfa,vo,errmsg,ok)
  use rd_textfun, only: fomsgf
  use com_eix, only: l, d, di, ps, e, nu, rho_e, g_e, r1, cs
  use com_eix2, only: ys
  use com_fillets, only: ntfil, tfilid, tfilrd
  use com_hang, only: hangle, accg, isf, ha
  use com_sea, only: rs, es, gs, dely, s, ie, ump
  use com_sec, only: n, y, nt, nn
  use com_sed, only: ds, di_d, di_s
  use com_tflxtor, only: siv, nsi, nki, sfv, ftv, scy, tvl, kvl, nsc, nks, nnt
  use rd_kinds, only: lrk, wp
  implicit none
!
  logical :: prv
  real(lrk) :: as, tfa, bfa, vo
  character(len=99) :: errmsg
!     mxc is related to mxscls
  integer :: im, pdm, pp, nc, mxc, ok
  dimension vo(31,mxc)
!
!     slope and gravity
!     angle unit r -> radians, default degree
!     bending stress median filter width
!
  integer :: mxs
  parameter (mxs = 99)
!
!     shaft data
!     yield strength
!
!     all sections (div)
  integer :: mts
  parameter (mts = 999)
!
!
!     sections additional data
!
!     section diameters
!
!     section fillets
  integer :: mxtfil
  parameter (mxtfil = 20)
!     number of section fillets
!     section ids
!     section fillets
!
!     mxscls dimension of section values
!     siv vector of section indieces to check
!     nsi notch sections index vector
!     nki sections with keyway index vector
!     sfv surface finishing code values
!     scy section ultimate strength vector
!     ftv fatigue values vector
!      1) FTCNP fatigue reliability
!      2) FTNBCY fatigue number of cycles
!      3) FTSTPK fatigue temperature (K)
!      4) FTASM aditional safety margin
!     tvl torque values vector
!      1) ALTQP alternating torque PU
!      2) MXTQP maximum torque PU
!      3) TQSTF torsion stress factor
!      4) BDSTF bend stress factor
!     kvl key geometry values vector
!      1) distance from keyway straight length start, up to left section
!      2) keyway straight same height (m)
!      3) keyway straight same height and width length (m)
!      4) keyway milling bottom fillet radius (m)
!      5) keyway straight same width (m)
!      6) sled runner keyway milling radius (m)
!     nsc number of sections to check
!     nks number of keys
!     nnt number of notches
  integer :: mxscls
  parameter(mxscls = 20)
!
  character(len=1) :: blank
  character(len=6) :: nmm
  character(len=11) :: c11
  logical :: isr, scl, scr, kwo, chkkwf, ckmxcf
  real(lrk) :: prec, rkp, rtk, bmi, ysf, sif, tav, trq, tau, bsd, rr, nf, tk, al, mx, otx, sir, pd, sy, st, bz, od, &
    & su, sfr, fr, hr, hd, sh, qs, koa, kob, sg, bs, ka, kb, kc, kd, sn, sk, sa, sx, vm1, vm2, vm3, sf1, sf2, no, po, &
    & sl, kvo, kwp, frv, shv, qsv, kom, cksnf, riff, kpstf, sumv_r
  integer :: ib, ii, jj, kk, ll, dm, si, sf, ni, pi, oki, mxm, mtg, mrc, ispdpf, isntcf, haskwf, w, wi
  real(wp) :: gsf, dl, dlv
!
!     wi warning octal->decimal code. do not sum same level warning.
!     see flxwarn on tsaidas.f
!     prec = precision on diameters comparison
  parameter (prec = 5e-4_lrk,mxm = 9,mtg = 500,mrc = 1,blank = ' ',nmm = 'tflxtr',c11 = 'yield.check')
!
  dimension rtk(mxm,2),rkp(mxm,2,2),ib(mxm),&
  &dl(mtg),bmi(mts,2),ysf(mts),sif(mts,2),bsd(mts),&
  &dlv(mtg),&
  &tav(mtg,mrc),trq(mtg,mrc),tau(mtg,mrc),&
  &koa(4),kob(4),kvo(6),frv(2),shv(2),qsv(2),kom(4,2)
!
  intrinsic :: abs, nint, real
!
  ok = -1
!
!     lateral matrices assembly
  call matrizes(pp,pdm,errmsg,oki)
  if (oki .lt. 0) return
!
!     sum all divisions
  ii = nint(sumv_r(n,mts,nt))
!     array dimension
  jj = 4*(ii+1)
!     linhael.f
!     calculate generalized displacements -> isr,gsf,ib,rkp,rtk,dl
  call lgndsp(prv,mxm,mtg,ii,jj,&
  &isr,gsf,ib,rkp,rtk,dl,errmsg,oki)
  if (oki .ne. 0) return
!     isr slope angle given in radians flag
!     gsf gravity+slope weight factor
!     ib bearing index vector
!     rkp bearing and support stiffness for a given speed
!     rtk equivalent bearing and support stiffnes
!     dl generalized displacements (x,z)
!
!     linhael.f
!     bending moment and shear forces -> bmi,isf,sif
  call bendst(nt,mts,mtg,isf,y,es,ie,dl,bmi,ysf,sif,errmsg,oki)
  if (oki .ne. 0) return
!     bmi bending moment on section y coordinates, x and z
!     ysf section coordinates for shear force constant on element
!     sif shear force on ys section coordinates, x and z
!
!     bending stress -> bsd
  call bstress(nt,mts,ie,ds,bmi,bsd)
!     bsd bending stress on sections
!
!     calculates angular generalized displacement vector -> dm,dlv
  call tsttor(im,mtg,dm,dlv,errmsg,oki)
  if (oki .lt. 0) return
!
!     torsion matrices already stated on main
!     calculates angular displacements, torque and stress -> tav,trq,tau
  call ttrqdiv(im,1,dlv,tav,trq,tau,dm,mtg,mrc)
!     tav divisions relative angular displacements
!     trq divisions torque
!     tau divisions stress
!
!     check section yield strenght
  do ii = 1,nsc
    if (ys(ii) .le. 0) then
!         invalid data
      errmsg = fomsgf(99, nmm,8,c11,0)
      return
    end if
  end do
!
!     sections fatigue reliability
  rr = ftv(1)
!     sections fatigue cycles expected life
  nf = ftv(2)
!     sections fatigue temperature (K)
  tk = ftv(3)
!     sections additional safety margin
  as = ftv(4)
!     pu alternating torque
  al = tvl(1)
!     pu maximum torque
  mx = tvl(2)
!     torsion-bend factors. if zero remove that stress.
!     torsion factor
  tfa = tvl(3)
!     bend factor
  bfa = tvl(4)
!
!     output matrix line index
  kk = 0
!
!     loop on sections to check
  do ii = 1,nsc
!
!       octal warning sum
    wi = 0
!       current section index
    si = siv(ii)
!       for real output matrix
    sir = real(si, lrk)
!       check index range
    if (si .lt. 1 .or. si .gt. cs) then
!         invalid index
      errmsg = fomsgf(99, nmm,27,blank,0)
      return
    end if
!       section position
    pd = ps(si)
!       division position index
    jj = ispdpf(nt,mts,y,pd)
    if (jj .le. 0) then
      !        invalid data
      errmsg = fomsgf(99, nmm,8,blank,0)
      return
    end if
!       section yield strenght
    sy = ys(si)
!       section mean torque stress
    st = tfa*abs(tau(jj,1))
!       section bending stress
    bz = bfa*bsd(jj)
!       section outer diameter
    od = d(si)
!       section ultimate strength (Pa)
    su = scy(ii)
!       section surface finishing
    sf = sfv(ii)
!       for real output matrix
    sfr = real(sf, lrk)
!       step changing at left flag
    scl = .false.
!       step changing at right flag
    scr = .false.
!       step changes: 0 = no, 1 = right, 2 = left, 3 = both
    otx = 0._lrk
!       notch section index
    jj = isntcf(si,nnt,mxscls,nsi)
!
!       check if section is a notch
    if (jj .gt. 0) then
!
!         section is notch
!
!         get notch dimensions or zero -> fr,hr,hd,sh
      call gntdm(si,nt,mxs,d,nnt,mxscls,nsi,&
      &ntfil,mxtfil,tfilid,tfilrd,fr,hr,hd,sh)
!         fr notch fillet radius
!         hr notch height he over fillet radius re=he/re
!         hd two times notch height he over outer diameter d0=2*he/d0
!         sh notch height
!
!         crack sensitivity factor
      qs = cksnf(fr,su,w)
!         w crack sensitivity factor warning level 0
      wi = wi+w
!
!         stress concentration factors - u-notch -> koa
      call grstf(hr,hd,qs,koa,w)
!         koa(1) torsion stress concentration factor
!         koa(2) bending stress concentration factor
!         koa(3) tension (axial) stress concentration factor
!         koa(4) 0
!         w u-notch stress concentration factor warning level 5
      wi = wi+w
!
!         factors -> sg,bs,ka,kb,kc,kd,sn
      call stfcp(od,0._lrk,su,st,bz,rr,tk,sf,nf,as,sg,bs,ka,kb,kc,kd,sn,w)
!         sg mean torque stress
!         bs bending stress
!         ka fatigue surface factor
!         kb fatigue sizing factor
!         kc fatigue reliability factor
!         kd temperature factor
!         sn fatigue limit
!         w fatigue limit correction factors warning level 2
      wi = wi+w
!
!         Von-Mises and safety for fatigue -> sa,sx,vm1,vm2,vm3,sf1,sf2
      call hcoef(al,mx,sn,sg,bs,sy,su,koa,&
      &sa,sx,vm1,vm2,vm3,sf1,sf2)
!         sa alternating torque
!         sx maximum torque
!         vm1 Von-Mises equivalent stress alternating
!         vm2 Von-Mises equivalent stress mean
!         vm3 Von-Mises equivalent stress maximum
!         sf1 safety factors for fatigue vector: Goodman
!         sf2 safety factors for fatigue vector: ASME
!
!         prepares output matrix notch
      kk = kk+1
!         check vo bound
      if (.not. ckmxcf(kk,mxc,errmsg)) return
!         save data on output matrix -> vo
      call tovec(mxscls,kk,sir,1._lrk,otx,sy,su,st,bz,od,sfr,sn,sa,sg,sx,bs,fr,sh,qs,koa,ka,kb,kc,kd,vm1,vm2,vm3,sf1, &
        & sf2,wi,vo)
!
!         check if section is a notch
    else
!
!         section is not notch
!
!         check next and previous sections -> ni,pi,no,po,sl
      call chksec(si,cs,mxs,ps,d,ni,pi,no,po,sl)
!         ni next section index
!         pi previous section index
!         no next section outer diameter
!         po previous section outer diameter
!         sl section length
!
!         get key index
      jj = haskwf(si,nks,mxscls,nki)
!
!         check if sections has key
      if (jj .gt. 0) then
!
!           section has key
!
!           get key dimensions -> kvo
        call gkydm(si,jj,nks,mxscls,nki,kvl,kvo)
!           kvo key dimensions
!            1) keyway milling bottom fillet radius (m)
!            2) keyway straight same height (m)
!            3) keyway straight same height and width length (m)
!            4) distance from keyway straight length start, up to left s
!            5) keyway straight same width (m)
!            6) sled runner keyway milling radius (m)
!
!           check keyway length
        kwo = chkkwf(sl,kvo)
        if (.not. kwo) then
!             invalid data
          errmsg = fomsgf(99, nmm,8,blank,0)
          return
        end if
!           factors
        call stfcp(od,0._lrk,su,st,bz,rr,tk,sf,nf,as,sg,bs,ka,kb,kc,kd,sn,w)
!           w fatigue limit correction factors warning level 2
        wi = wi+w
!
!           keyway step change and proximity factors -> scl,scr,sk,qs,fr
        call kwscpf(pi,si,prec,po,od,no,su,sl,kvo,&
        &ntfil,mxtfil,tfilid,tfilrd,&
        &scl,scr,sk,qs,fr,sh,koa,w)
!            scl step changing at left
!            scr step changing at right
!            sk smaller distance between keyway and step change (m)
!            qs crack sensitivity factor
!            fr fillet radius to next section
!            sh step changing height next section
!            koa stress concentration factor with proximity:
!            w keyway proximity warning level 4
        wi = wi+w
!
!           check keyway step change
        if (scl .or. scr) then
!             side code
          otx = riff(scr,1._lrk,2._lrk)
!             Von-Mises and safety for fatigue
          call hcoef(al,mx,sn,sg,bs,sy,su,koa,&
          &sa,sx,vm1,vm2,vm3,sf1,sf2)
!             prepares output matrix keyway step change
          kk = kk+1
          if (.not. ckmxcf(kk,mxc,errmsg)) return
          call tovec(mxscls,kk,sir,4._lrk,otx,sy,su,st,bz,od,sfr,sn,sa,sg,sx,bs,fr,sh,qs,koa,ka,kb,kc,kd,vm1,vm2,vm3, &
            & sf1,sf2,wi,vo)
!              reset warning
          wi = 0
!              unset already handled keyway step change
          if (scr) then
            scr = .false.
          else
            scl = .false.
          end if
!
!             keyway stepchange if
        end if
!
!           keyway fillet radius (m)
        fr = kvo(4)
        if (fr .le. 0) then
!             invalid data
          errmsg = fomsgf(99, nmm,8,blank,0)
          return
        end if
!           keyway height
        sh = kvo(2)
!           stress concentration due to keyway proximity
        kwp = kpstf(od,sh,fr,2,w)
!           w keyway proximity warning level 6
        wi = wi+w
!           stress concentration factor - keyway point A
        call kwastf(kob)
!           apply proximity factor
        koa(1) = kob(1)*kwp
        koa(2) = kob(2)*kwp
        koa(3) = kob(3)
        koa(4) = kwp
!           Von-Mises and safety for fatigue
        call hcoef(al,mx,sn,sg,bs,sy,su,koa,&
        &sa,sx,vm1,vm2,vm3,sf1,sf2)
!           prepares output matrix keyway point A
        kk = kk+1
        if (.not. ckmxcf(kk,mxc,errmsg)) return
        call tovec(mxscls,kk,sir,2._lrk,0._lrk,sy,su,st,bz,od,sfr,sn,sa,sg,sx,bs,0._lrk,0._lrk,0._lrk,koa,ka,kb,kc,kd, &
          & vm1,vm2,vm3,sf1,sf2,wi,vo)
!           reset warning
        wi = 0
!           key crack sensitivity factor
        qs = cksnf(fr,su,w)
!           w crack sensitivity factor warning level 0
        wi = wi+w
!           stress concentration factor - keyway point B
        call kwbstf(od,fr,qs,kob,w)
!           w keyway point B stress concentration factor warning level 7
        wi = wi+w
!           factors
        call stfcp(od,sh,su,st,bz,rr,tk,sf,nf,as,&
        &sg,bs,ka,kb,kc,kd,sn,w)
!           w warning fatigue limit correction factors level 2
        wi = wi+w
!           equivalent VM stress, fatigue safety factors
        call hcoef(al,mx,sn,sg,bs,sy,su,kob,&
        &sa,sx,vm1,vm2,vm3,sf1,sf2)
!           prepares output matrix keyway point B
        kk = kk+1
        if (.not. ckmxcf(kk,mxc,errmsg)) return
        call tovec(mxscls,kk,sir,3._lrk,0._lrk,sy,su,st,bz,od,sfr,sn,sa,sg,sx,bs,fr,sh,qs,kob,ka,kb,kc,kd,vm1,vm2,vm3, &
          & sf1,sf2,wi,vo)
!           reset warning
        wi = 0
!
!           keyway other side step change -> otx,frv,shv,qsv,kom
        call kwnstch(scl,scr,pi,si,ni,po,od,no,su,&
        &ntfil,mxtfil,tfilid,tfilrd,&
        &otx,frv,shv,qsv,kom,w)
!           otx step change in the other side of keyway
!           0 = no, 1 = right, 2 = left, 3 = both
!           frv fillet radius for next section vector
!           shv step changing height next section vector
!           qsv crack sensitivity factor vector
!           kom torsion stress concentration factors left and right
!           w step change stress concentration factors warning level 3
        wi = wi+w
!
!           output index right or left
        jj = nint(otx)
!
!           check step change code otx
        if (jj .eq. 1 .or. jj .eq. 2) then
!             factors
          call stfcp(od,0._lrk,su,st,bz,rr,tk,sf,nf,as,sg,bs,ka,kb,kc,kd,sn,w)
!             w fatigue limit correction factors warning level 2
          wi = wi+w
          do ll = 1,4
            koa(ll) = kom(ll,jj)
          end do
          fr = frv(jj)
          sh = shv(jj)
          qs = qsv(jj)
!             equivalent VM stress, fatigue safety factors
          call hcoef(al,mx,sn,sg,bs,sy,su,koa,&
          &sa,sx,vm1,vm2,vm3,sf1,sf2)
!             prepares output matrix keyway point B
          kk = kk+1
          if (.not. ckmxcf(kk,mxc,errmsg)) return
          call tovec(mxscls,kk,sir,5._lrk,otx,sy,su,st,bz,od,sfr,sn,sa,sg,sx,bs,fr,sh,qs,koa,ka,kb,kc,kd,vm1,vm2,vm3, &
            & sf1,sf2,wi,vo)
        else if (jj .ne. 0) then
!             invalid data
          errmsg = fomsgf(99, nmm,8,blank,0)
          return
!
!             step change code jj if
        end if
!
!           check if sections has key
      else
!
!           no key
!
!           po previous section outer diameter (left)
!           od current section outer diameter
!           no next section outer diameter (right)
        scl = po .gt. od+prec
        scr = no .gt. od+prec
!           step change -> otx,frv,shv,qsv,kom
        call kwnstch(scl,scr,pi,si,ni,po,od,no,su,&
        &ntfil,mxtfil,tfilid,tfilrd,&
        &otx,frv,shv,qsv,kom,w)
!           w step change stress concentration factors warning level 3
        wi = wi+w
!           output index right or left
        jj = nint(otx)
!           check step change right (1,3)
        if (jj .eq. 1 .or. jj .eq. 3) then
!             factors
          call stfcp(od,0._lrk,su,st,bz,rr,tk,sf,nf,as,sg,bs,ka,kb,kc,kd,sn,w)
!             w fatigue limit correction factors warning level 2
          wi = wi+w
!             right = column 1
          do ll = 1,4
            koa(ll) = kom(ll,1)
          end do
          fr = frv(1)
          sh = shv(1)
          qs = qsv(1)
!             equivalent VM stress, fatigue safety factors
          call hcoef(al,mx,sn,sg,bs,sy,su,koa,&
          &sa,sx,vm1,vm2,vm3,sf1,sf2)
!             prepares output matrix step change right
          kk = kk+1
          if (.not. ckmxcf(kk,mxc,errmsg)) return
          call tovec(mxscls,kk,sir,5._lrk,1._lrk,sy,su,st,bz,od,sfr,sn,sa,sg,sx,bs,fr,sh,qs,koa,ka,kb,kc,kd,vm1,vm2, &
            & vm3,sf1,sf2,wi,vo)
!             if step change right
        end if
!
!           check step change left (2,3)
        if (jj .eq. 2 .or. jj .eq. 3) then
!             factors
          call stfcp(od,0._lrk,su,st,bz,rr,tk,sf,nf,as,sg,bs,ka,kb,kc,kd,sn,w)
!             w fatigue limit correction factors warning level 2
          wi = wi+w
!             left = column 2
          do ll = 1,4
            koa(ll) = kom(ll,2)
          end do
          fr = frv(2)
          sh = shv(2)
          qs = qsv(2)
!             equivalent VM stress, fatigue safety factors
          call hcoef(al,mx,sn,sg,bs,sy,su,koa,&
          &sa,sx,vm1,vm2,vm3,sf1,sf2)
!             prepares output matrix step change left
          kk = kk+1
          if (.not. ckmxcf(kk,mxc,errmsg)) return
          call tovec(mxscls,kk,sir,5._lrk,2._lrk,sy,su,st,bz,od,sfr,sn,sa,sg,sx,bs,fr,sh,qs,koa,ka,kb,kc,kd,vm1,vm2, &
            & vm3,sf1,sf2,wi,vo)
!
!             if step change left
        end if
!
!           check if sections has key
      end if
!
!         notch if
    end if

!       sections loop
  end do
!
!     number of computed columns to output
  nc = kk
  ok = 0
!
  return
!
end subroutine tflxt
!
!     ==================================================================
!>    @brief calculate and output flexural torsion stress, fatigue
!>     figures and safety margin on sections.
!
!>    @param[in] im torsion model index
!>    @param[in] pdm final [AA] matrix dimension
!>    @param[in] pp number of effective bearing supports
!>    @param[in] prv variable bearing time response
!>    @param[in] std standard input output
!>    @param[in] plt generate plot output
!>    @param[out] errmsg return an error message
!>    @param[out] ok return flag. unsuccessful if <0.
subroutine tflextor(im,pdm,pp,prv,std,plt,errmsg,ok)
  use rd_kinds, only: lrk
  implicit none
!
  logical :: prv, std, plt
  character(len=99) :: errmsg
  integer :: im, pdm, pp, ok
!
  real(lrk) :: as, tfa, bfa, vo
  integer :: nc, mxc, oki
  parameter(mxc = 25)
  dimension vo(31,mxc)
!
  ok = -1
  call progress_begin('FLEXURAL_TORSIONAL',2,&
  &'coupled flexural-torsional stress/fatigue processing')
  call progress_stage('FLEXURAL_TORSIONAL',&
  &'calculate coupled section response')
!
!     flexural torsion stress and fatigue on sections -> nc,as,tfa,bfa,v
  call tflxt(im,pdm,pp,prv,mxc,nc,as,&
  &tfa,bfa,vo,errmsg,oki)
  if (oki .lt. 0) return
  call progress_update('FLEXURAL_TORSIONAL',1,2,real(nc, lrk),'OUTPUT_COLUMNS')
!     nc number of columns of output data matrix
!     as sections aditional safety margin
!     tfa torsion stress factor
!     bfa bend stress factor
!     vo output data matrix
!
!     output -> ok
  call progress_stage('FLEXURAL_TORSIONAL','write coupled output')
  call ts_flxtor(as,tfa,bfa,nc,mxc,vo,std,plt,errmsg,ok)
  if(ok.eq.0) then
    call progress_update('FLEXURAL_TORSIONAL',2,2,real(nc, lrk),'OUTPUT_COLUMNS')
    call progress_end('FLEXURAL_TORSIONAL','OK')
  endif
!
  return
!
end subroutine tflextor
!
