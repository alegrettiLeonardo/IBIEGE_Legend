!     $Id$
!     ==================================================================
!
!>    @file tcmpbl.f
!>    @author francisco
!>    @date 06-feb-20
!>    @brief torsion Campbell diagram calculation routines, last changes
!>     new file - feb-20.
!
!     ==================================================================
!>    @brief find crossing points betwen speed time
!>      and torsional natural frequencies.
!
!>    @param[in] tcpn rated speed rpm
!>    @param[in] git torsional natural frequencies (rad/s)
!>    @param[in] teln speed time vector, 1x, 2x, ...
!      negative value represents 2 x grid frequency (slip)
!>    @param[out] ig cross natural frequency index vector
!>    @param[out] ic cross index vector
!>    @param[out] px speed cross vector (rpm)
!>    @param[out] py frequency cross vector (Hz)
!>    @param[out] ncn number of crossing point on px ans py vector
!>    @param[in] nnf number of natural frequencies on git vector
!>    @param[in] nteln number of speed time vector
!>     elements on teln vector
!>    @param[in] mtg dimension of natural frequency vector git
!>    @param[in] mxteln dimension of speed time vector teln
!>    @param[in] mxpt dimension of cross vectors
!>    @param[out] errmsg return an error message
!>    @param[out] ok return flag. unsuccessful if <0.
!
subroutine tinter(tcpn,git,teln,ig,ic,px,py,ncn,&
&nnf,nteln,mtg,mxteln,mxpt,errmsg,ok)
  use rd_textfun, only: fomsgf
  use rd_kinds, only: lrk
  implicit none
!
  integer :: ig, ic, ncn, nnf, nteln, mtg, mxteln, mxpt, ok
  real(lrk) :: tcpn, git, teln, px, py
  dimension git(mtg),teln(mxteln),&
  &ig(mxpt),ic(mxpt),px(mxpt),py(mxpt)
  character(len=99) :: errmsg
!
  integer :: ii, jj
  real(lrk) :: fr, tnf, rps, rad2hzf
!     rpm to rps
  parameter (rps = 60)
!     error handling
  character(len=4) :: cld
  character(len=6) :: nmm
  parameter (cld ='mxpt',nmm = 'tinter')
!
!     return init
  ok = -1
!
!     cross count
  ncn = 0
!
!     speed time vector
  do ii = 1,nteln
!       natural frequencies
    do jj = 1,nnf
!         natural frequency rad -> hz
      tnf = rad2hzf(git(jj))
      if (teln(ii) .gt. 0) then
        ncn = ncn+1
        if (ncn .gt. mxpt)then
          errmsg = fomsgf(99, nmm,4,cld,0)
          return
        end if
!           speed (rpm)
        px(ncn) = tnf*rps/teln(ii)
!           frequency (Hz)
        py(ncn) = tnf
!           natural frequency index
        ig(ncn) = jj
!           cross index
        ic(ncn) = ii
      else
!           2x slip <0
        if (tcpn .gt. 0) then
          ncn = ncn+1
          fr = abs(teln(ii))
          px(ncn) = (fr-tnf)/fr*tcpn
          py(ncn) = tnf
          ig(ncn) = jj
          ic(ncn) = ii
        end if
      end if
    end do
  end do
!
  ok = 0
!
  return
!
end subroutine tinter
!
!     ==================================================================
!>    @brief torsion Campbel diagram
!
!>    @param[in] std standard input output
!>    @param[in] plt HPGL output
!>    @param[out] errmsg return an error message
!>    @param[out] ok return flag. unsuccessful if <0.
!
subroutine tcmpbl(std,plt,errmsg,ok)
  use com_cpb, only: nini, nfin, dw, npi, ncc, imt
  use com_mdampso, only: mdoff
  use com_tcpbl, only: tcpn, tsmg, nteln
  use com_tcpbl1, only: teln
  use com_tepm, only: rfi, rlam, ddm
  use rd_kinds, only: lrk
  implicit none
!
!     arguments
  integer :: ok
  character(len=99) :: errmsg
  logical :: std, plt
!
!     campbell
!     campbell
!     mxc -> max number of campbell frequencies
  integer :: mxc
  parameter (mxc = 19)
!
!     torsion Campbell
  integer :: mxteln
  parameter (mxteln = 15)
!     nominal speed (rpm), separation margin (pu),
!     excitation lines pu speed, -grid frequency
!
!     modal dampings
!     mode offset
!
!     torsion modal space
  integer :: mtg
  parameter (mtg = 500)
!
  integer :: ii, ig, ic, ivr, ncn, nnf, mxpt, iiff, oki
  real(lrk) :: git, px, py
  parameter (mxpt = mxc*mxteln)
  dimension git(mtg),ig(mxpt),ic(mxpt),px(mxpt),py(mxpt)
!
  intrinsic :: abs, sqrt
!
  ok =-1
  call progress_begin('TORSION_CAMPBELL',3,&
  &'natural frequencies, crossings and output')
  call progress_stage('TORSION_CAMPBELL',&
  &'collect torsional natural frequencies')
!
!     check number of desired natural frequency and
!     available from modal space
!
  ivr = iiff (ddm .lt. ncc,ddm,ncc)
!
  nnf = 0
  do ii = 1+mdoff,ivr
    nnf = nnf+1
!       natural frequency (rad/s)
    git(nnf) = sqrt(abs(rlam(ii)))
  end do
  call progress_update('TORSION_CAMPBELL',1,3,real(nnf, lrk),'NATURAL_FREQS')
  call progress_stage('TORSION_CAMPBELL',&
  &'find excitation-line crossings')
!
!     find crossing points betwen speed time
!     and torsional natural frequencies -> ig,ic,px,py
  call tinter(tcpn,git,teln,ig,ic,px,py,ncn,&
  &nnf,nteln,mtg,mxteln,mxpt,errmsg,oki)
  if (oki .lt. 0) return
  call progress_update('TORSION_CAMPBELL',2,3,real(ncn, lrk),'CROSSINGS')
  call progress_stage('TORSION_CAMPBELL','write Campbell output')
!
!     output
  call ts_cmpbl(teln,ig,ic,px,py,git,nini,nfin,dw,&
  &tcpn,tsmg,&
  &ncn,nteln,nnf,mtg,mxteln,mxpt,std,plt,errmsg,oki)
  if (oki .lt. 0) return
!
  call progress_update('TORSION_CAMPBELL',3,3,real(ncn, lrk),'CROSSINGS')
  ok = 0
  call progress_end('TORSION_CAMPBELL','OK')
!
  return
!
end subroutine tcmpbl
!
