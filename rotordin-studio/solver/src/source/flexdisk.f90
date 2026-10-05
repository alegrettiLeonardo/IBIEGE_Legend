!     ==================================================================
!     Flexible rotating disk support based on Chen's rotational spring
!     reduction for the first nodal-diameter disk mode.
!     ==================================================================

subroutine flexdisk_upper(s)
  implicit none
  character(len=*) :: s
  integer :: i, k
  do i=1,len(s)
    k=ichar(s(i:i))
    if(k.ge.97 .and. k.le.122) s(i:i)=char(k-32)
  enddo
  return
end subroutine flexdisk_upper

subroutine flexdisk_parse_option(buf,id,off,errmsg,ok)
  use com_dflexi, only: dmodel, dfdofx, dfdofz, nfdisk, nshaftfd
  use com_dflexr, only: dkr, df1nd
  use rd_kinds, only: lrk
  implicit none
  integer :: mxd, id, ok, ios
  parameter(mxd=99)
  character(len=*) :: buf, errmsg
  character(len=16) :: model, key
  real(lrk) :: off, val

  ok=-1
  off=0._lrk
  dmodel(id)=0
  dkr(id)=0._lrk
  df1nd(id)=0._lrk
  dfdofx(id)=0
  dfdofz(id)=0
  if(len_trim(buf).eq.0) then
    ok=0
    return
  endif

  read(buf,*,iostat=ios) off
  if(ios.ne.0) then
    errmsg='flexdisk: invalid DISK OFFSET/options field'
    return
  endif

  model=' '
  read(buf,*,iostat=ios) off,model
  if(ios.ne.0) then
    ok=0
    return
  endif
  call flexdisk_upper(model)

  if(model.eq.'RIGID') then
    dmodel(id)=0
    ok=0
    return
  endif
  if(model.ne.'FLEXIBLE' .and. model.ne.'FLEX') then
    errmsg='flexdisk: model must be RIGID or FLEXIBLE'
    return
  endif
  dmodel(id)=1

  key=' '
  val=0._lrk
  read(buf,*,iostat=ios) off,model,key,val
  if(ios.eq.0) then
    call flexdisk_upper(key)
    if(key.eq.'KR') then
      dkr(id)=val
    else if(key.eq.'F1ND' .or. key.eq.'F1ND_HZ') then
      df1nd(id)=val
    else
      errmsg='flexdisk: use FLEXIBLE KR value or F1ND value'
      return
    endif
  else
!       Short form: OFFSET FLEXIBLE KR_VALUE
    read(buf,*,iostat=ios) off,model,val
    if(ios.ne.0) then
      errmsg='flexdisk: FLEXIBLE disk requires KR or F1ND'
      return
    endif
    dkr(id)=val
  endif
  ok=0
  return
end subroutine flexdisk_parse_option

subroutine flexdisk_recount()
  use com_dflexi, only: dmodel, dfdofx, dfdofz, nfdisk, nshaftfd
  use com_dflexr, only: dkr, df1nd
  use com_dis, only: pd, dd => d_d, hd => h_d, rho => rho_d, r2, nd
  use rd_kinds, only: lrk
  implicit none
  integer :: mxd, i
  parameter(mxd=99)
  nfdisk=0
  do i=1,nd
    if(dmodel(i).eq.1) nfdisk=nfdisk+1
  enddo
  return
end subroutine flexdisk_recount

subroutine flexdisk_finalize(errmsg,ok)
  use com_dflexi, only: dmodel, dfdofx, dfdofz, nfdisk, nshaftfd
  use com_dflexr, only: dkr, df1nd
  use com_dsc, only: md, idx, idy
  use rd_kinds, only: lrk, wp
  implicit none
  integer :: mxd, i, j
  parameter(mxd=99)
  character(len=*) :: errmsg
  integer :: ok
  real(wp) :: twopi
  parameter(twopi=6.283185307179586476925286766559_wp)
  ok=-1
  call flexdisk_recount()
  j=0
  if(nfdisk.gt.0) then
    call progress_begin('FLEXIBLE_DISK',nfdisk,&
    &'validate and reduce flexible disk models')
  endif
  do i=1,mxd
    if(dmodel(i).eq.1) then
      if(idx(i).le.0._lrk) then
        write(errmsg,'(A,I0,A)')&
        &'flexdisk: disk ',i,' has non-positive diametral inertia'
        return
      endif
      if(dkr(i).gt.0._lrk .and. df1nd(i).gt.0._lrk) then
        write(errmsg,'(A,I0,A)')&
        &'flexdisk: disk ',i,' has both KR and F1ND'
        return
      endif
      if(dkr(i).le.0._lrk .and. df1nd(i).le.0._lrk) then
        write(errmsg,'(A,I0,A)')&
        &'flexdisk: disk ',i,' requires positive KR or F1ND'
        return
      endif
      if(dkr(i).le.0._lrk) then
        dkr(i)=idx(i)*real((twopi*real(df1nd(i), wp))**2, lrk)
      else
        df1nd(i)=real(sqrt(real(dkr(i), wp)/real(idx(i), wp))/twopi, lrk)
      endif
      if(dkr(i).le.0._lrk) then
        write(errmsg,'(A,I0,A)')&
        &'flexdisk: disk ',i,' has non-positive KR'
        return
      endif
      j=j+1
      call progress_update('FLEXIBLE_DISK',j,nfdisk,df1nd(i),&
      &'F1ND_HZ')
    endif
  enddo
  ok=0
  if(nfdisk.gt.0) call progress_end('FLEXIBLE_DISK','OK')
  return
end subroutine flexdisk_finalize

subroutine flexdisk_assign_dofs(nshaft)
  use com_dflexi, only: dmodel, dfdofx, dfdofz, nfdisk, nshaftfd
  use com_dflexr, only: dkr, df1nd
  use com_dis, only: pd, dd => d_d, hd => h_d, rho => rho_d, r2, nd
  use rd_kinds, only: lrk
  implicit none
  integer :: mxd, nshaft, i, k
  parameter(mxd=99)
  nshaftfd=nshaft
  k=0
  do i=1,nd
    dfdofx(i)=0
    dfdofz(i)=0
    if(dmodel(i).eq.1) then
      k=k+1
      dfdofx(i)=nshaft+2*k-1
      dfdofz(i)=nshaft+2*k
    endif
  enddo
  nfdisk=k
  return
end subroutine flexdisk_assign_dofs

integer function flexdisk_count_f()
  use com_dflexi, only: dmodel, dfdofx, dfdofz, nfdisk, nshaftfd
  implicit none
  integer :: mxd
  parameter(mxd=99)
  flexdisk_count_f=nfdisk
  return
end function flexdisk_count_f

integer function flexdisk_ndof_f()
  implicit none
  integer :: flexdisk_count_f
  flexdisk_ndof_f=2*flexdisk_count_f()
  return
end function flexdisk_ndof_f

integer function shaft_ndof_f()
  use com_dflexi, only: dmodel, dfdofx, dfdofz, nfdisk, nshaftfd
  implicit none
  integer :: mxd
  parameter(mxd=99)
  shaft_ndof_f=nshaftfd
  return
end function shaft_ndof_f

integer function rotating_ndof_f()
  implicit none
  integer :: shaft_ndof_f, flexdisk_ndof_f
  rotating_ndof_f=shaft_ndof_f()+flexdisk_ndof_f()
  return
end function rotating_ndof_f

integer function flexdisk_dof_x_f(idisk)
  use com_dflexi, only: dmodel, dfdofx, dfdofz, nfdisk, nshaftfd
  implicit none
  integer :: mxd, idisk
  parameter(mxd=99)
  if(idisk.lt.1 .or. idisk.gt.mxd) then
    flexdisk_dof_x_f=0
  else
    flexdisk_dof_x_f=dfdofx(idisk)
  endif
  return
end function flexdisk_dof_x_f

integer function flexdisk_dof_z_f(idisk)
  use com_dflexi, only: dmodel, dfdofx, dfdofz, nfdisk, nshaftfd
  implicit none
  integer :: mxd, idisk
  parameter(mxd=99)
  if(idisk.lt.1 .or. idisk.gt.mxd) then
    flexdisk_dof_z_f=0
  else
    flexdisk_dof_z_f=dfdofz(idisk)
  endif
  return
end function flexdisk_dof_z_f

subroutine flexdisk_write_audit(std)
  use com_dflexi, only: dmodel, dfdofx, dfdofz, nfdisk, nshaftfd
  use com_dflexr, only: dkr, df1nd
  use com_dis, only: pd, dd => d_d, hd => h_d, rho => rho_d, r2, nd
  use com_doff, only: offd => off_d
  use com_dsc, only: md, idx, idy
  use com_mat, only: mm, mg, dm, smn
  use rd_kinds, only: lrk
  implicit none
  integer :: mxd, mtg, i, iu
  parameter(mxd=99,mtg=500)
  logical :: std
  if(nfdisk.le.0) return
  iu=97
  open(iu,file='flexdisk_audit.out',status='replace')
  call flexdisk_write_audit_unit(iu)
  close(iu)
  if(std) call flexdisk_write_audit_unit(6)
  return
end subroutine flexdisk_write_audit

subroutine flexdisk_write_audit_unit(iu)
  use com_dflexi, only: dmodel, dfdofx, dfdofz, nfdisk, nshaftfd
  use com_dflexr, only: dkr, df1nd
  use com_dis, only: pd, dd => d_d, hd => h_d, rho => rho_d, r2, nd
  use com_doff, only: offd => off_d
  use com_dsc, only: md, idx, idy
  use com_mat, only: mm, mg, dm, smn
  use com_mbk, only: mkb
  use rd_kinds, only: lrk
  implicit none
  integer :: mxd, mtg, i, iu
  parameter(mxd=99,mtg=500)
  real(lrk) :: merr, kerr, gerr, errv
  integer :: j
  merr=0._lrk
  kerr=0._lrk
  gerr=0._lrk
  do i=1,dm
    do j=1,dm
      merr=max(merr,abs(mm(i,j)-mm(j,i)))
      kerr=max(kerr,abs(mkb(i,j)-mkb(j,i)))
      gerr=max(gerr,abs(mg(i,j)+mg(j,i)))
    enddo
  enddo
  write(iu,'(A)') '#BEGIN FLEXIBLE_DISK_AUDIT'
  write(iu,'(A,I0)') 'NUMBER_OF_DISKS = ',nd
  write(iu,'(A,I0)') 'NUMBER_OF_FLEXIBLE_DISKS = ',nfdisk
  write(iu,'(A,I0)') 'SHAFT_DOFS = ',nshaftfd
  write(iu,'(A,I0)') 'ADDITIONAL_DISK_DOFS = ',2*nfdisk
  write(iu,'(A,I0)') 'TOTAL_GLOBAL_DOFS = ',dm
  write(iu,'(A,I0)') 'MAX_GLOBAL_DOFS = ',mtg
  write(iu,'(A,I0)') 'DOF_MARGIN = ',mtg-dm
  write(iu,'(A,ES16.8)') 'M_SYMMETRY_MAX = ',merr
  write(iu,'(A,ES16.8)') 'K_BASE_SYMMETRY_MAX = ',kerr
  write(iu,'(A,ES16.8)') 'G_SKEW_MAX = ',gerr
  do i=1,nd
    if(dmodel(i).eq.1) then
      write(iu,'(A,I0)') 'DISK = ',i
      write(iu,'(A,ES16.8)') 'POSITION_M = ',pd(i)
      write(iu,'(A)') 'MODEL = FLEXIBLE'
      write(iu,'(A,ES16.8)') 'MASS_KG = ',md(i)
      write(iu,'(A,ES16.8)') 'ID_KG_M2 = ',idx(i)
      write(iu,'(A,ES16.8)') 'IP_KG_M2 = ',idy(i)
      write(iu,'(A,ES16.8)') 'OFFSET_M = ',offd(i)
      write(iu,'(A,ES16.8)') 'KR_NM_RAD = ',dkr(i)
      write(iu,'(A,ES16.8)') 'F1ND_EQUIV_HZ = ',df1nd(i)
      write(iu,'(A,I0)') 'DOF_THETA_X = ',dfdofx(i)
      write(iu,'(A,I0)') 'DOF_THETA_Z = ',dfdofz(i)
      errv=abs(mkb(dfdofx(i),dfdofx(i))-dkr(i))
      errv=max(errv,abs(mkb(dfdofz(i),dfdofz(i))-dkr(i)))
      write(iu,'(A,ES16.8)') 'SPRING_DISK_DIAG_MAX_ERROR = ',errv
      errv=abs(mm(dfdofx(i),dfdofx(i))-idx(i))
      errv=max(errv,abs(mm(dfdofz(i),dfdofz(i))-idx(i)))
      write(iu,'(A,ES16.8)') 'DISK_ID_DIAG_MAX_ERROR = ',errv
      errv=abs(mg(dfdofx(i),dfdofz(i))+idy(i))
      errv=max(errv,abs(mg(dfdofz(i),dfdofx(i))-idy(i)))
      write(iu,'(A,ES16.8)') 'DISK_GYRO_MAX_ERROR = ',errv
    endif
  enddo
  write(iu,'(A)') '#END FLEXIBLE_DISK_AUDIT'
  return
end subroutine flexdisk_write_audit_unit
