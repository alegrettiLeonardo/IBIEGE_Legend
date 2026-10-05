!     $Id$
!     ==================================================================
!
!>    @file tmodos.f
!>    @brief torsional modal space for eigenvalues
!>    last changes:<br>
!>    new file dce-19 - f.
!
!     ==================================================================
!>    @brief torsion modes
!
!>    @param[in] im torsion model identifier: 1 two, 2 three nodes.
!>    @param[in] std standard input output
!>    @param[in] plt HPGL output
!>    @param[out] errmsg return an error message
!>    @param[out] ok return flag. unsuccessful if <0.
!
subroutine tmodos(im,std,plt,errmsg,ok)
  use rd_textfun, only: fomsgf, fwmsgf
  use com_mdampso, only: mdoff
  use com_mdd, only: qtd_modos, porb, rorb, rang
  use com_sec, only: n, y, nt, nn
  use com_sprat1, only: csrt
  use com_tepm, only: rfi, rlam, ddm
  use com_tmdn, only: tmdnb, ntmdn
  use rd_kinds, only: lrk
  implicit none
!
!     arguments
  integer :: im, ok
  character(len=99) :: errmsg
  logical :: std, plt
!
  character(len=1) :: blank
  character(len=3) :: cbd
  character(len=6) :: nmm
  dimension cbd(2)
  parameter (blank = ' ',cbd = (/'xmd','mts'/),nmm = 'tmodos')
!
!     modes
  integer :: xmd
!     max number of modes
  parameter (xmd = 19)
!     time orbit position
!
!     total number of divisions
  integer :: mts
  parameter (mts = 999)
!
!     divisions
!
  integer :: mtt
  parameter (mtt = mts)
!
!     speed ratio
!
!     mode numbers to calculate/present
!     overrides NBRMOD
  integer :: mxtmdn
  parameter (mxtmdn = 15)
!
!     mode offset
!
!     maximal number of global matrices elements
  integer :: mtg
  parameter (mtg = 500)
!
!     torsion modal space
!
  integer :: ii, jj, kk, nm, npv, idm, nidm
  real(lrk) :: py, vmx, tm, fmd, sgl
  dimension idm(xmd),py(mts),tm(xmd,mtg),fmd(xmd)
  data sgl/1/
!
  intrinsic :: abs, sign, sqrt
!
!     initialize return
  ok = -1
!
!     check desired number of mode and dimension of problem
  if (im .eq. 1) then
!       two nodes per element
    nm = ddm-1
  else
!       three nodes
    nm = (ddm-1)/2
  end if
!
!     check torsion number of mode
  if (ntmdn .gt. 0) then
!       torsion number of mode
!       check number modes
    if (ntmdn .gt. xmd) then
      errmsg = fomsgf(99, nmm,4,cbd(1),0)
      return
    end if
!       number of mode to show
    nidm = ntmdn

!       copy user torsion selected modes (ordered)
!       mode offset
    do ii = 1,nidm
!         copy user mode number selection
      idm(ii) = tmdnb(ii)
!         check max
      if (idm(ii) .gt. nm) exit
    end do
!       let only possibles
    nidm = ii-1
  else
!       lateral number of mode
    if (qtd_modos .gt. xmd) then
      errmsg = fomsgf(99, nmm,4,cbd(1),0)
      return
    end if
    nidm = qtd_modos
    do ii = 1+mdoff,nidm
      idm(ii) = ii
      if (idm(ii) .gt. nm) exit
    end do
    nidm = ii-1
  end if
!
  if (nidm .lt. 1) then
!       no modes to show...
    errmsg = fwmsgf(99, nmm,26,blank,0)
    return
  end if
!
  call progress_begin('TORSION_MODES',nidm,&
  &'normalize and output torsional mode shapes')
!     prepare normalized eigenvectors
  do ii = 1,nidm
!       max value determination
    vmx = 0
!       mode index
    jj = idm(ii)
!       mode frequency rad/s
!       lam = w^2
    fmd(ii) = sqrt(rlam(jj))
    do kk = 1,ddm
      tm(ii,kk) = rfi(kk,jj)*csrt(kk)
!         normalization value
      if (abs(tm(ii,kk)) .gt. vmx) then
        vmx = abs(tm(ii,kk))
!           signal of maximum
        sgl = sign(1.0_lrk,tm(ii,kk))
      end if
    end do
!       apply normalization
    do kk = 1,ddm
      tm(ii,kk) = sgl*tm(ii,kk)/vmx
    end do
    call progress_update('TORSION_MODES',ii,nidm,fmd(ii),&
    &'RAD_S')
  end do
!
!     final y positions vector
  if (im .eq. 1) then
!       standard two nodes
    npv = nt
    do ii = 1,npv
      py(ii) = y(ii)
    end do
  else
!       three nodes
    npv = 2*nt-1
!       check number of y positions
    if (npv .gt. mts) then
      errmsg = fomsgf(99, nmm,4,cbd(2),0)
      return
    end if
    do ii = 1,nt
      jj = (ii-1)*2
      kk = jj+1
      py(kk) = y(ii)
      if (jj .gt. 0) then
!           intermediate node
        py(jj) = y(ii)-(y(ii)-y(ii-1))/2
      end if
    end do
  end if
!
!     output, ok return will be set
  call progress_stage('TORSION_MODES','write mode-shape output')
  call ts_modos(nidm,npv,idm,fmd,py,&
  &tm,xmd,mts,mtg,std,plt,errmsg,ok)
  if(ok.eq.0) call progress_end('TORSION_MODES','OK')
!
  return
!
end subroutine tmodos
!
