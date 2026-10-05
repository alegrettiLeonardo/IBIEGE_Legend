!
!     Dynamic foundation subsystem for RotorDin.
!     Foundation is distinct from BEARING SUPPORT / housing.
!     User supplies reduced physical matrices Mf, Kf and damping.
!     Dynamic stiffness follows Zf(w)=Kf-w**2 Mf+j*w Cf.
!
subroutine foundation_reset()
  use com_fndctl, only: factive, fcompare, fndof, fnint, fdkind
  use com_fndifc, only: ficonn, fiidx, fixd, fizd
  use com_fndmat, only: fm, fk, fc
  use com_fndmod, only: fzeta
  use com_fndray, only: falpha, fbeta
  use com_fndstr, only: ffile, fred
  use rd_kinds, only: lrk
  implicit none
  integer :: mtg, mxi
  parameter(mtg=500,mxi=20)
  integer :: i, j
  factive=.false.
  fcompare=.false.
  fndof=0
  fnint=0
  fdkind=0
  falpha=0.0_lrk
  fbeta=0.0_lrk
  ffile=' '
  fred='USER_REDUCED'
  do i=1,mxi
    ficonn(i)=0
    fiidx(i)=0
    fixd(i)=0
    fizd(i)=0
  enddo
  do i=1,mtg
    fzeta(i)=0.0_lrk
  enddo
  call foundation_dynamic_reset()
  return
end subroutine foundation_reset
!
subroutine foundation_upper(s)
  implicit none
  character(len=*) :: s
  integer :: i, k
  do i=1,len(s)
    k=ichar(s(i:i))
    if(k.ge.97 .and. k.le.122) s(i:i)=char(k-32)
  enddo
  return
end subroutine foundation_upper
!
subroutine foundation_rest(line,val)
  implicit none
  character(len=*) :: line, val
  integer :: i, n
  val=' '
  n=len_trim(line)
  i=1
  do while(i.le.n .and. line(i:i).ne.' ' .and.&
  &line(i:i).ne.char(9))
    i=i+1
  enddo
  do while(i.le.n .and. (line(i:i).eq.' ' .or.&
  &line(i:i).eq.char(9)))
    i=i+1
  enddo
  if(i.le.n) val=line(i:n)
  return
end subroutine foundation_rest
!
subroutine foundation_resolve(input_name,name,resolved)
  implicit none
  character(len=*) :: input_name, name, resolved
  integer :: i, j, n
  resolved=' '
  n=len_trim(name)
  if(n.le.0) return
  if(name(1:1).eq.'/' .or. index(name,':').eq.2) then
    resolved=name
    return
  endif
  i=len_trim(input_name)
  j=0
  do while(i.gt.0)
    if(input_name(i:i).eq.'/' .or. input_name(i:i).eq.'\\') then
      j=i
      exit
    endif
    i=i-1
  enddo
  if(j.gt.0) then
    resolved=input_name(1:j)//name(1:n)
  else
    resolved=name
  endif
  return
end subroutine foundation_resolve
!
subroutine foundation_read_control(iu,input_name,errmsg,ok)
  use rd_kinds, only: lrk
  implicit none
  integer :: iu, ok, ios
  character(len=*) :: input_name, errmsg
  character(len=512) :: line, uline
  character(len=32) :: key, val
  character(len=255) :: rest, resolved
  logical :: found, foundation_active_f, transient_pending_f, mhandled
  call foundation_reset()
  ok=0
  found=.false.
  if(transient_pending_f()) return
10 continue
  read(iu,'(A)',iostat=ios) line
  if(ios.ne.0) return
  uline=adjustl(line)
  call foundation_upper(uline)
  if(len_trim(uline).eq.0) goto 10
  call model_config_line(line,errmsg,ok,mhandled)
  if(ok.ne.0) return
  if(mhandled) goto 10
  if(index(uline,'LATERAL_RESPONSE_MODE').eq.1) then
    call transient_parse_analysis_mode(iu,line,errmsg,ok)
    if(ok.ne.0) return
    goto 10
  endif
  if(index(uline,'TRANSIENT').eq.1) then
    call transient_stash_line(line)
    return
  endif
  if(index(uline,'END').eq.1 .and.&
  &index(uline,'END_FOUNDATION').ne.1) return
  if(index(uline,'FOUNDATION').eq.1) then
    found=.true.
    goto 20
  endif
  goto 10
20 continue
  if(.not.found) return
  call progress_begin('FOUNDATION',4,&
  &'load, validate, prepare damping and audit foundation')
  call progress_stage('FOUNDATION','read foundation model')
!
!     Read first nonblank row after the FOUNDATION heading.  Two syntaxe
!     are accepted:
!
!       V2 inline (preferred for GUI-generated input.txt):
!         FOUNDATION  REDUCTION  DAMPING  COMPARE
!         4           USER_REDUCED MATRIX 1
!
!       V1 file-based compatibility syntax:
!         FOUNDATION
!         ACTIVE YES
!         FILE case.fdn
!
25 continue
  read(iu,'(A)',iostat=ios) line
  if(ios.ne.0) then
    errmsg='foundation: unexpected EOF after FOUNDATION heading'
    ok=-1
    return
  endif
  if(len_trim(line).eq.0) goto 25
  uline=adjustl(line)
  call foundation_upper(uline)
  read(uline,*,iostat=ios) key
  if(ios.ne.0) goto 25
  call foundation_upper(key)
  if(trim(key).eq.'FOUNDATION_MODEL') then
    read(uline,*,iostat=ios) key,val
    if(ios.ne.0) then
      errmsg='foundation: invalid FOUNDATION_MODEL'
      ok=-1
      return
    endif
    call foundation_upper(val)
    if(trim(val).eq.'DYNAMIC_STIFFNESS_TABLE') then
      call foundation_dynamic_set_model(1)
      call foundation_load_dynamic_inline(iu,errmsg,ok)
    else if(trim(val).eq.'PHYSICAL_MCK') then
      call foundation_dynamic_set_model(0)
26    continue
      read(iu,'(A)',iostat=ios) line
      if(ios.ne.0) then
        errmsg='foundation: missing PHYSICAL_MCK control row'
        ok=-1
        return
      endif
      if(len_trim(line).eq.0) goto 26
      uline=adjustl(line)
      if(uline(1:1).eq.'#') goto 26
      call foundation_load_inline(iu,line,errmsg,ok)
    else
      errmsg='foundation: unknown FOUNDATION_MODEL'
      ok=-1
      return
    endif
    if(ok.lt.0) return
    call progress_update('FOUNDATION',1,4,1.0_lrk,'LOAD')
    call progress_stage('FOUNDATION','validate M/C/K or Z(f) data')
    call foundation_validate(errmsg,ok)
    if(ok.lt.0) return
    call progress_update('FOUNDATION',2,4,2.0_lrk,'VALIDATE')
    call progress_stage('FOUNDATION','prepare damping model')
    call foundation_prepare_damping(errmsg,ok)
    if(ok.lt.0) return
    call progress_update('FOUNDATION',3,4,3.0_lrk,'DAMPING')
    call progress_stage('FOUNDATION','run modal/dynamic audit')
    call foundation_modal_audit(errmsg,ok)
    if(ok.lt.0) return
    call progress_update('FOUNDATION',4,4,4.0_lrk,'AUDIT')
    call progress_end('FOUNDATION','OK')
    return
  endif
  if(trim(key).ne.'ACTIVE' .and. trim(key).ne.'FILE' .and.&
  &trim(key).ne.'COMPARE') then
    call foundation_dynamic_set_model(0)
    call foundation_load_inline(iu,line,errmsg,ok)
    if(ok.lt.0) return
    call progress_update('FOUNDATION',1,4,1.0_lrk,'LOAD')
    call progress_stage('FOUNDATION','validate M/C/K data')
    call foundation_validate(errmsg,ok)
    if(ok.lt.0) return
    call progress_update('FOUNDATION',2,4,2.0_lrk,'VALIDATE')
    call progress_stage('FOUNDATION','prepare damping model')
    call foundation_prepare_damping(errmsg,ok)
    if(ok.lt.0) return
    call progress_update('FOUNDATION',3,4,3.0_lrk,'DAMPING')
    call progress_stage('FOUNDATION','run modal audit')
    call foundation_modal_audit(errmsg,ok)
    if(ok.lt.0) return
    call progress_update('FOUNDATION',4,4,4.0_lrk,'AUDIT')
    call progress_end('FOUNDATION','OK')
    return
  endif
!
!     Legacy V1 control block.  Keep it for backwards compatibility, but
!     the GUI should generate the inline V2 block instead of a .fdn file
!
30 continue
  if(trim(key).eq.'ACTIVE') then
    read(uline,*,iostat=ios) key,val
    call foundation_upper(val)
    call foundation_set_active(val)
  else if(trim(key).eq.'FILE') then
    call foundation_rest(line,rest)
    call foundation_set_file(rest)
  else if(trim(key).eq.'COMPARE') then
    read(uline,*,iostat=ios) key,val
    call foundation_upper(val)
    call foundation_set_compare(val)
  endif
35 continue
  read(iu,'(A)',iostat=ios) line
  if(ios.ne.0) then
    errmsg='foundation: unexpected EOF in FOUNDATION block'
    ok=-1
    return
  endif
  if(len_trim(line).eq.0) goto 35
  uline=adjustl(line)
  call foundation_upper(uline)
  if(index(uline,'END_FOUNDATION').eq.1 .or.&
  &index(uline,'END,FOUNDATION').eq.1) goto 80
  read(uline,*,iostat=ios) key
  if(ios.ne.0) goto 35
  call foundation_upper(key)
  goto 30
80 continue
  if(foundation_active_f()) then
    call foundation_get_file(rest)
    if(len_trim(rest).le.0) then
      errmsg='foundation: ACTIVE YES but FILE is not defined'
      ok=-1
      return
    endif
    call foundation_dynamic_set_model(0)
    call foundation_resolve(input_name,rest,resolved)
    call foundation_load(resolved,errmsg,ok)
    if(ok.lt.0) return
    call progress_update('FOUNDATION',1,4,1.0_lrk,'LOAD')
    call progress_stage('FOUNDATION','validate M/C/K data')
    call foundation_validate(errmsg,ok)
    if(ok.lt.0) return
    call progress_update('FOUNDATION',2,4,2.0_lrk,'VALIDATE')
    call progress_stage('FOUNDATION','prepare damping model')
    call foundation_prepare_damping(errmsg,ok)
    if(ok.lt.0) return
    call progress_update('FOUNDATION',3,4,3.0_lrk,'DAMPING')
    call progress_stage('FOUNDATION','run modal audit')
    call foundation_modal_audit(errmsg,ok)
    if(ok.lt.0) return
    call progress_update('FOUNDATION',4,4,4.0_lrk,'AUDIT')
    call progress_end('FOUNDATION','OK')
  else
    call progress_stage('FOUNDATION','foundation disabled')
    call progress_end('FOUNDATION','OK')
  endif
  return
end subroutine foundation_read_control
!
subroutine foundation_set_damping_name(name,errmsg,ok)
  use com_fndctl, only: factive, fcompare, fndof, fnint, fdkind
  implicit none
  character(len=*) :: name, errmsg
  integer :: ok
  character(len=32) :: tmp
  tmp=adjustl(name)
  call foundation_upper(tmp)
  if(trim(tmp).eq.'NONE') then
    fdkind=0
  else if(trim(tmp).eq.'MATRIX') then
    fdkind=1
  else if(trim(tmp).eq.'RAYLEIGH') then
    fdkind=2
  else if(trim(tmp).eq.'MODAL') then
    fdkind=3
  else
    errmsg='foundation: invalid damping model'
    ok=-1
    return
  endif
  ok=0
  return
end subroutine foundation_set_damping_name
!
subroutine foundation_load_inline(iu,control,errmsg,ok)
  use com_fndctl, only: factive, fcompare, fndof, fnint, fdkind
  use com_fndifc, only: ficonn, fiidx, fixd, fizd
  use com_fndmat, only: fm, fk, fc
  use com_fndmod, only: fzeta
  use com_fndray, only: falpha, fbeta
  use com_fndstr, only: ffile, fred
  use rd_kinds, only: lrk
  implicit none
  integer :: mtg, mxi
  parameter(mtg=500,mxi=20)
  integer :: iu, ok, ios, i, j, nif, mode, nmodal
  integer :: icompare
  character(len=*) :: control, errmsg
  character(len=512) :: line, uline
  character(len=32) :: key, val1, val2, dname
  real(lrk) :: zv
  ok=-1
  ffile='INLINE_INPUT'
  factive=.true.
  fcompare=.false.
  fndof=0
  fnint=0
  fdkind=0
  falpha=0.0_lrk
  fbeta=0.0_lrk
  fred='USER_REDUCED'
!
!     Control row generated by the interface:
!       NDOF REDUCTION DAMPING COMPARE
!     Example:
!       4 USER_REDUCED MATRIX 1
!
  icompare=0
  read(control,*,iostat=ios) fndof,fred,dname,icompare
  if(ios.ne.0 .or. fndof.lt.2 .or. fndof.gt.mtg) then
    errmsg='foundation: invalid inline control row'
    return
  endif
  call foundation_upper(fred)
  call foundation_set_damping_name(dname,errmsg,ok)
  if(ok.lt.0) return
  fcompare=(icompare.ne.0)
  do i=1,fndof
    fzeta(i)=0.0_lrk
    do j=1,fndof
      fm(i,j)=0.0_lrk
      fk(i,j)=0.0_lrk
      fc(i,j)=0.0_lrk
    enddo
  enddo
10 continue
  read(iu,'(A)',iostat=ios) line
  if(ios.ne.0) then
    errmsg='foundation: missing END_FOUNDATION'
    return
  endif
  if(len_trim(line).eq.0) goto 10
  uline=adjustl(line)
  if(uline(1:1).eq.'#') goto 10
  uline=adjustl(line)
  call foundation_upper(uline)
  if(index(uline,'END_FOUNDATION').eq.1 .or.&
  &index(uline,'END,FOUNDATION').eq.1) goto 90
  read(uline,*,iostat=ios) key
  if(ios.ne.0) goto 10
  call foundation_upper(key)
  if(trim(key).eq.'FNDIFACE') then
    read(uline,*,iostat=ios) key,nif
    if(ios.ne.0 .or. nif.lt.1 .or. nif.gt.mxi) then
      errmsg='foundation: invalid FNDIFACE count'
      return
    endif
    fnint=nif
!       UI-generated header row: FNDLOC FNDCONN FNDIDX FNDXDOF FNDZDOF
20  read(iu,'(A)',iostat=ios) line
    if(ios.ne.0) then
      errmsg='foundation: missing FNDIFACE header'
      return
    endif
    if(len_trim(line).eq.0) goto 20
    uline=adjustl(line)
    if(uline(1:1).eq.'#') goto 20
    do i=1,fnint
30    read(iu,'(A)',iostat=ios) line
      if(ios.ne.0) then
        errmsg='foundation: missing FNDIFACE row'
        return
      endif
      if(len_trim(line).eq.0) goto 30
      uline=adjustl(line)
      if(uline(1:1).eq.'#') goto 30
      uline=adjustl(line)
      call foundation_upper(uline)
      read(uline,*,iostat=ios) val1,val2,fiidx(i),fixd(i),fizd(i)
      if(ios.ne.0) then
        errmsg='foundation: invalid FNDIFACE row'
        return
      endif
      call foundation_upper(val2)
      if(trim(val2).eq.'SUPPORT') then
        ficonn(i)=1
      else if(trim(val2).eq.'BEARING') then
        ficonn(i)=2
      else
        errmsg='foundation: FNDCONN must be SUPPORT or BEARING'
        return
      endif
    enddo
  else if(trim(key).eq.'FMASS') then
    do i=1,fndof
      read(iu,*,iostat=ios) (fm(i,j),j=1,fndof)
      if(ios.ne.0) then
        errmsg='foundation: invalid FMASS matrix'
        return
      endif
    enddo
  else if(trim(key).eq.'FSTIFF') then
    do i=1,fndof
      read(iu,*,iostat=ios) (fk(i,j),j=1,fndof)
      if(ios.ne.0) then
        errmsg='foundation: invalid FSTIFF matrix'
        return
      endif
    enddo
  else if(trim(key).eq.'FDAMP') then
    do i=1,fndof
      read(iu,*,iostat=ios) (fc(i,j),j=1,fndof)
      if(ios.ne.0) then
        errmsg='foundation: invalid FDAMP matrix'
        return
      endif
    enddo
  else if(trim(key).eq.'FRAYLEIGH') then
    read(uline,*,iostat=ios) key,falpha,fbeta
    if(ios.ne.0) then
      errmsg='foundation: invalid FRAYLEIGH alpha beta'
      return
    endif
  else if(trim(key).eq.'FMODDAMP') then
    read(uline,*,iostat=ios) key,nmodal
    if(ios.ne.0 .or. nmodal.lt.0 .or. nmodal.gt.fndof) then
      errmsg='foundation: invalid FMODDAMP count'
      return
    endif
    do i=1,nmodal
      read(iu,*,iostat=ios) mode,zv
      if(ios.ne.0 .or. mode.lt.1 .or. mode.gt.fndof .or.zv.lt.0.0_lrk) then
        errmsg='foundation: invalid FMODDAMP row'
        return
      endif
      fzeta(mode)=zv
    enddo
  else
    errmsg='foundation: unknown inline keyword '//trim(key)
    return
  endif
  goto 10
90 continue
  ok=0
  return
end subroutine foundation_load_inline
!
subroutine foundation_set_active(val)
  use com_fndctl, only: factive, fcompare, fndof, fnint, fdkind
  implicit none
  character(len=*) :: val
  factive=(val(1:1).eq.'Y' .or. val(1:1).eq.'1' .or.&
  &val(1:1).eq.'T')
  return
end subroutine foundation_set_active
!
subroutine foundation_set_compare(val)
  use com_fndctl, only: factive, fcompare, fndof, fnint, fdkind
  implicit none
  character(len=*) :: val
  fcompare=(val(1:1).eq.'Y' .or. val(1:1).eq.'1' .or.&
  &val(1:1).eq.'T')
  return
end subroutine foundation_set_compare
!
subroutine foundation_set_file(val)
  use com_fndstr, only: ffile, fred
  implicit none
  character(len=*) :: val
  ffile=adjustl(val)
  return
end subroutine foundation_set_file
!
subroutine foundation_get_file(val)
  use com_fndstr, only: ffile, fred
  implicit none
  character(len=*) :: val
  val=ffile
  return
end subroutine foundation_get_file
!
logical function foundation_active_f()
  use com_fndctl, only: factive, fcompare, fndof, fnint, fdkind
  implicit none
  foundation_active_f=factive
  return
end function foundation_active_f
!
logical function foundation_dynamic_f()
  use com_fnddyni, only: fdmodel, fdnpt
  implicit none
  foundation_dynamic_f=(fdmodel.eq.1)
  return
end function foundation_dynamic_f
!
integer function foundation_ndof_f()
  use com_fndctl, only: factive, fcompare, fndof, fnint, fdkind
  implicit none
  if(factive) then
    foundation_ndof_f=fndof
  else
    foundation_ndof_f=0
  endif
  return
end function foundation_ndof_f
!
integer function foundation_global_dof(dm,iloc)
  implicit none
  integer :: dm, iloc, nf, foundation_ndof_f
  nf=foundation_ndof_f()
  foundation_global_dof=dm-nf+iloc
  return
end function foundation_global_dof
!
integer function support_dof_x(dm,pp,isup)
  implicit none
  integer :: dm, pp, isup, nf, foundation_ndof_f
  nf=foundation_ndof_f()
  support_dof_x=dm-nf-2*(pp-isup)-1
  return
end function support_dof_x
!
subroutine foundation_load(name,errmsg,ok)
  use com_fndctl, only: factive, fcompare, fndof, fnint, fdkind
  use com_fndifc, only: ficonn, fiidx, fixd, fizd
  use com_fndmat, only: fm, fk, fc
  use com_fndmod, only: fzeta
  use com_fndray, only: falpha, fbeta
  use com_fndstr, only: ffile, fred
  use rd_kinds, only: lrk
  implicit none
  integer :: mtg, mxi
  parameter(mtg=500,mxi=20)
  character(len=*) :: name, errmsg
  integer :: ok, iu, ios, i, j, nif, mode
  character(len=512) :: line, uline
  character(len=32) :: key, val1, val2
  real(lrk) :: zv
  ok=-1
  iu=73
  open(iu,file=trim(name),status='old',action='read',iostat=ios)
  if(ios.ne.0) then
    errmsg='foundation: cannot open '//trim(name)
    return
  endif
  ffile=trim(name)
  fndof=0
  fnint=0
  fdkind=0
  falpha=0.0_lrk
  fbeta=0.0_lrk
10 continue
  read(iu,'(A)',iostat=ios) line
  if(ios.ne.0) goto 90
  if(len_trim(line).eq.0 .or. line(1:1).eq.'#') goto 10
  uline=adjustl(line)
  call foundation_upper(uline)
  read(uline,*,iostat=ios) key
  if(ios.ne.0) goto 10
  call foundation_upper(key)
  if(trim(key).eq.'FOUNDATION_VERSION') then
    goto 10
  else if(trim(key).eq.'NDOF') then
    read(uline,*,iostat=ios) key,fndof
    if(ios.ne.0 .or. fndof.lt.2 .or. fndof.gt.mtg) then
      errmsg='foundation: invalid NDOF'
      close(iu)
      return
    endif
    do i=1,fndof
      fzeta(i)=0.0_lrk
      do j=1,fndof
        fm(i,j)=0.0_lrk
        fk(i,j)=0.0_lrk
        fc(i,j)=0.0_lrk
      enddo
    enddo
  else if(trim(key).eq.'UNITS') then
    read(uline,*,iostat=ios) key,val1
    call foundation_upper(val1)
    if(trim(val1).ne.'SI') then
      errmsg='foundation: V1 accepts only SI units'
      close(iu)
      return
    endif
  else if(trim(key).eq.'REDUCTION') then
    read(uline,*,iostat=ios) key,fred
    call foundation_upper(fred)
  else if(trim(key).eq.'DAMPING') then
    read(uline,*,iostat=ios) key,val1
    call foundation_upper(val1)
    if(trim(val1).eq.'NONE') then
      fdkind=0
    else if(trim(val1).eq.'MATRIX') then
      fdkind=1
    else if(trim(val1).eq.'RAYLEIGH') then
      fdkind=2
    else if(trim(val1).eq.'MODAL') then
      fdkind=3
    else
      errmsg='foundation: invalid damping model'
      close(iu)
      return
    endif
  else if(trim(key).eq.'RAYLEIGH') then
    read(uline,*,iostat=ios) key,falpha,fbeta
    if(ios.ne.0) then
      errmsg='foundation: invalid RAYLEIGH alpha beta'
      close(iu)
      return
    endif
  else if(trim(key).eq.'INTERFACES') then
    read(uline,*,iostat=ios) key,nif
    if(ios.ne.0 .or. nif.lt.1 .or. nif.gt.mxi) then
      errmsg='foundation: invalid INTERFACES count'
      close(iu)
      return
    endif
    fnint=nif
    do i=1,fnint
20    read(iu,'(A)',iostat=ios) line
      if(ios.ne.0) then
        errmsg='foundation: missing INTERFACE line'
        close(iu)
        return
      endif
      if(len_trim(line).eq.0 .or. line(1:1).eq.'#') goto 20
      uline=adjustl(line)
      call foundation_upper(uline)
      read(uline,*,iostat=ios) key,val1,val2,fiidx(i),&
      &fixd(i),fizd(i)
      if(ios.ne.0 .or. trim(key).ne.'INTERFACE') then
        errmsg='foundation: invalid INTERFACE definition'
        close(iu)
        return
      endif
      if(trim(val2).eq.'SUPPORT') then
        ficonn(i)=1
      else if(trim(val2).eq.'BEARING') then
        ficonn(i)=2
      else
        errmsg='foundation: CONNECT must be SUPPORT or BEARING'
        close(iu)
        return
      endif
    enddo
  else if(trim(key).eq.'MASS_MATRIX') then
    if(fndof.le.0) then
      errmsg='foundation: NDOF must precede matrices'
      close(iu)
      return
    endif
    do i=1,fndof
      read(iu,*,iostat=ios) (fm(i,j),j=1,fndof)
      if(ios.ne.0) then
        errmsg='foundation: invalid MASS_MATRIX'
        close(iu)
        return
      endif
    enddo
  else if(trim(key).eq.'STIFFNESS_MATRIX') then
    do i=1,fndof
      read(iu,*,iostat=ios) (fk(i,j),j=1,fndof)
      if(ios.ne.0) then
        errmsg='foundation: invalid STIFFNESS_MATRIX'
        close(iu)
        return
      endif
    enddo
  else if(trim(key).eq.'DAMPING_MATRIX') then
    do i=1,fndof
      read(iu,*,iostat=ios) (fc(i,j),j=1,fndof)
      if(ios.ne.0) then
        errmsg='foundation: invalid DAMPING_MATRIX'
        close(iu)
        return
      endif
    enddo
  else if(trim(key).eq.'MODAL_DAMPING') then
40  read(iu,'(A)',iostat=ios) line
    if(ios.ne.0) then
      errmsg='foundation: missing END_MODAL_DAMPING'
      close(iu)
      return
    endif
    if(len_trim(line).eq.0 .or. line(1:1).eq.'#') goto 40
    uline=adjustl(line)
    call foundation_upper(uline)
    if(index(uline,'END_MODAL_DAMPING').eq.1) goto 10
    read(uline,*,iostat=ios) mode,zv
    if(ios.ne.0 .or. mode.lt.1 .or. mode.gt.fndof .or.zv.lt.0.0_lrk) then
      errmsg='foundation: invalid modal damping entry'
      close(iu)
      return
    endif
    fzeta(mode)=zv
    goto 40
  else if(trim(key).eq.'END') then
    goto 90
  endif
  goto 10
90 continue
  close(iu)
  if(fndof.le.0) then
    errmsg='foundation: NDOF not defined'
    return
  endif
  ok=0
  return
end subroutine foundation_load
!
subroutine foundation_cholesky(a,n,pd,minp)
  use rd_kinds, only: lrk
  implicit none
  integer :: n, i, j, k
  real(lrk) :: a(500,500), l(500,500), s, minp, scale
  logical :: pd
  pd=.true.
  minp=1.0e30_lrk
  scale=0.0_lrk
  do i=1,n
    scale=max(scale,abs(a(i,i)))
    do j=1,n
      l(i,j)=0.0_lrk
    enddo
  enddo
  if(scale.le.0.0_lrk) then
    pd=.false.
    return
  endif
  do i=1,n
    do j=1,i
      s=a(i,j)
      do k=1,j-1
        s=s-l(i,k)*l(j,k)
      enddo
      if(i.eq.j) then
        minp=min(minp,s)
        if(s.le.max(1.0e-7_lrk*scale,1.0e-20_lrk)) then
          pd=.false.
          return
        endif
        l(i,j)=sqrt(s)
      else
        l(i,j)=s/l(j,j)
      endif
    enddo
  enddo
  return
end subroutine foundation_cholesky
!
subroutine foundation_validate(errmsg,ok)
  use com_fndctl, only: factive, fcompare, fndof, fnint, fdkind
  use com_fndifc, only: ficonn, fiidx, fixd, fizd
  use com_fndmat, only: fm, fk, fc
  use com_sp1, only: ns, bn
  use com_ymc, only: nbrg, rks, pc
  use rd_kinds, only: lrk
  implicit none
  integer :: mtg, mxi
  parameter(mtg=500,mxi=20)
  character(len=*) :: errmsg
  integer :: ok, i, j, k
  logical :: pdm, pdk, foundation_dynamic_f
  real(lrk) :: d, sm, sk, sc, minp
!     bearing/support model counts are already available when the
!     optional FOUNDATION block is parsed (it follows SUPPORT data).
  ok=-1
  if(.not.factive) then
    ok=0
    return
  endif
  if(fnint.lt.1) then
    errmsg='foundation: at least one interface is required'
    return
  endif
  if(foundation_dynamic_f()) then
    if(fndof.ne.4) then
      errmsg='foundation: dynamic stiffness V1 requires NDOF=4'
      return
    endif
    call foundation_dynamic_validate(errmsg,k)
    if(k.lt.0) return
    goto 50
  endif
  sm=0.0_lrk
  sk=0.0_lrk
  sc=0.0_lrk
  do i=1,fndof
    do j=1,fndof
      sm=max(sm,abs(fm(i,j)))
      sk=max(sk,abs(fk(i,j)))
      sc=max(sc,abs(fc(i,j)))
    enddo
  enddo
  do i=1,fndof
    do j=1,fndof
      if(abs(fm(i,j)-fm(j,i)).gt.1.0e-5_lrk*max(sm,1.0_lrk)) then
        errmsg='foundation: MASS_MATRIX is not symmetric'
        return
      endif
      if(abs(fk(i,j)-fk(j,i)).gt.1.0e-5_lrk*max(sk,1.0_lrk)) then
        errmsg='foundation: STIFFNESS_MATRIX is not symmetric'
        return
      endif
      if(fdkind.eq.1 .and.abs(fc(i,j)-fc(j,i)).gt.1.0e-5_lrk*max(sc,1.0_lrk)) then
        errmsg='foundation: DAMPING_MATRIX is not symmetric'
        return
      endif
    enddo
  enddo
  call foundation_cholesky(fm,fndof,pdm,minp)
  if(.not.pdm) then
    errmsg='foundation: MASS_MATRIX is not positive definite'
    return
  endif
  call foundation_cholesky(fk,fndof,pdk,minp)
  if(.not.pdk) then
    errmsg='foundation: STIFFNESS_MATRIX is not positive definite'
    return
  endif
50 continue
  do i=1,fnint
    if(fixd(i).lt.1 .or. fixd(i).gt.fndof .or.&
    &fizd(i).lt.1 .or. fizd(i).gt.fndof .or.&
    &fixd(i).eq.fizd(i)) then
      errmsg='foundation: invalid interface DOF mapping'
      return
    endif
    if(fiidx(i).lt.1) then
      errmsg='foundation: invalid interface component index'
      return
    endif
!       Interface component indices are explicit model indices:
!       SUPPORT n refers to the nth SUPPORT row; BEARING n to nth
!       bearing.  Reject nonexistent and duplicate connections.
    if(ficonn(i).eq.1 .and. fiidx(i).gt.ns) then
      errmsg='foundation: SUPPORT interface index exceeds count'
      return
    endif
    if(ficonn(i).eq.2 .and. fiidx(i).gt.nbrg) then
      errmsg='foundation: BEARING interface index exceeds count'
      return
    endif
    do j=1,i-1
      if(ficonn(i).eq.ficonn(j) .and.&
      &fiidx(i).eq.fiidx(j)) then
        errmsg='foundation: duplicate component interface'
        return
      endif
    enddo
!       A bearing that already owns a flexible SUPPORT must connect
!       to FOUNDATION through that housing, not in parallel directly.
    if(ficonn(i).eq.2) then
      do k=1,ns
        if(bn(k).eq.fiidx(i)) then
          errmsg='foundation: BEARING has SUPPORT; connect via '&
          &//'SUPPORT to avoid double counting'
          return
        endif
      enddo
    endif
  enddo
  ok=0
  return
end subroutine foundation_validate
!
subroutine foundation_eigen(freq,phi,errmsg,ok)
  use com_fndmat, only: fm, fk, fc
  use rd_kinds, only: lrk
  implicit none
  integer :: mtg, mxi
  parameter(mtg=500,mxi=20)
  real(lrk) :: freq(mtg), phi(mtg,mtg)
  character(len=*) :: errmsg
  integer :: ok, n, i, j, info, lwork, foundation_ndof_f
  logical :: foundation_dynamic_f
  real(lrk) :: a(mtg,mtg), b(mtg,mtg), w(mtg), work(8*mtg)
  real(lrk) :: pi
  parameter(pi=3.14159265358979323846_lrk)
!     M8.4 LAPACK precision interface: a,b,w,work,freq,phi are real(lrk),
!     promoted to real64 by rd_kinds' M8.1 change; SSYGV (single precision)
!     is replaced by its double-precision counterpart DSYGV so the routine
!     called always matches the width of the data passed to it (this
!     subroutine has an implicit interface, so a width mismatch here would
!     silently reinterpret bits rather than fail to compile).  Both SSYGV
!     and DSYGV are resolved from the system LAPACK the executable already
!     links (-llapack), exactly as SSYGV was in A: no new library boundary
!     is introduced, and the solver's own vendored LAPACK/BLAS tree
!     (src/source/lapack/) is unchanged.  See tools/migration/m8_lapack_map.csv.
  external dsygv
  n=foundation_ndof_f()
  ok=-1
  if(foundation_dynamic_f()) then
    errmsg='foundation: dynamic table has no MCK eigenproblem'
    return
  endif
  if(n.le.0) then
    ok=0
    return
  endif
  do i=1,n
    do j=1,n
      a(i,j)=fk(i,j)
      b(i,j)=fm(i,j)
    enddo
  enddo
  lwork=8*mtg
  call dsygv(1,'V','U',n,a,mtg,b,mtg,w,work,lwork,info)
  if(info.ne.0) then
    write(errmsg,'(A,I8)') 'foundation: DSYGV failed INFO=',info
    return
  endif
  do i=1,n
    if(w(i).gt.0.0_lrk) then
      freq(i)=sqrt(w(i))/(2.0_lrk*pi)
    else
      freq(i)=0.0_lrk
    endif
    do j=1,n
      phi(j,i)=a(j,i)
    enddo
  enddo
  ok=0
  return
end subroutine foundation_eigen
!
subroutine foundation_prepare_damping(errmsg,ok)
  use com_fndctl, only: factive, fcompare, fndof, fnint, fdkind
  use com_fndmat, only: fm, fk, fc
  use com_fndmod, only: fzeta
  use com_fndray, only: falpha, fbeta
  use rd_kinds, only: lrk
  implicit none
  integer :: mtg, mxi
  parameter(mtg=500,mxi=20)
  character(len=*) :: errmsg
  integer :: ok, i, j, k, n, oki
  logical :: foundation_dynamic_f
  real(lrk) :: freq(mtg), phi(mtg,mtg)
  real(lrk) :: tmp(mtg), omega, pi
  parameter(pi=3.14159265358979323846_lrk)
  ok=-1
  n=fndof
  if(.not.factive) then
    ok=0
    return
  endif
  if(foundation_dynamic_f()) then
    ok=0
    return
  endif
  if(fdkind.eq.0) then
    do i=1,n
      do j=1,n
        fc(i,j)=0.0_lrk
      enddo
    enddo
  else if(fdkind.eq.2) then
    if(falpha.lt.0.0_lrk .or. fbeta.lt.0.0_lrk) then
      errmsg='foundation: Rayleigh alpha/beta must be >= 0'
      return
    endif
    do i=1,n
      do j=1,n
        fc(i,j)=falpha*fm(i,j)+fbeta*fk(i,j)
      enddo
    enddo
  else if(fdkind.eq.3) then
    call foundation_eigen(freq,phi,errmsg,oki)
    if(oki.lt.0) return
    do i=1,n
      do j=1,n
        fc(i,j)=0.0_lrk
      enddo
    enddo
    do k=1,n
      if(fzeta(k).gt.0.0_lrk .and. freq(k).gt.0.0_lrk) then
        omega=2.0_lrk*pi*freq(k)
        do i=1,n
          tmp(i)=0.0_lrk
          do j=1,n
            tmp(i)=tmp(i)+fm(i,j)*phi(j,k)
          enddo
        enddo
        do i=1,n
          do j=1,n
            fc(i,j)=fc(i,j)+2.0_lrk*fzeta(k)*omega*tmp(i)*tmp(j)
          enddo
        enddo
      endif
    enddo
  endif
  ok=0
  return
end subroutine foundation_prepare_damping
!
subroutine foundation_modal_audit(errmsg,ok)
  use com_fndctl, only: factive, fcompare, fndof, fnint, fdkind
  use com_fndifc, only: ficonn, fiidx, fixd, fizd
  use com_fndmat, only: fm, fk, fc
  use com_fndstr, only: ffile, fred
  use rd_kinds, only: lrk
  implicit none
  integer :: mtg, mxi
  parameter(mtg=500,mxi=20)
  character(len=*) :: errmsg
  integer :: ok, i, j, k, oki, n, iu, ii
  real(lrk) :: freq(mtg), phi(mtg,mtg)
  real(lrk) :: mmod, cmod, omega, zeta, pi
  parameter(pi=3.14159265358979323846_lrk)
  logical :: foundation_dynamic_f
  character(len=16) :: dname
  if(.not.factive) then
    ok=0
    return
  endif
  if(foundation_dynamic_f()) then
    call foundation_dynamic_audit(errmsg,ok)
    return
  endif
  call foundation_eigen(freq,phi,errmsg,oki)
  if(oki.lt.0) then
    ok=-1
    return
  endif
  n=fndof
  iu=74
  open(iu,file='foundation_audit.out',status='replace')
  write(iu,'(A)')&
  &'============================================================'
  write(iu,'(A)') ' ROTORDIN - FOUNDATION MODAL AUDIT'
  write(iu,'(A)')&
  &'============================================================'
  write(iu,'(A,A)') ' SOURCE FILE: ',trim(ffile)
  write(iu,'(A,I6)') ' FOUNDATION DOF: ',n
  write(iu,'(A,A)') ' REDUCTION: ',trim(fred)
  if(fdkind.eq.0) then
    dname='NONE'
  else if(fdkind.eq.1) then
    dname='MATRIX'
  else if(fdkind.eq.2) then
    dname='RAYLEIGH'
  else
    dname='MODAL'
  endif
  write(iu,'(A,A)') ' DAMPING MODEL: ',trim(dname)
  write(iu,'(A)') ' MATRIX VALIDATION:'
  write(iu,'(A)') '   M SYMMETRY / POSITIVE DEFINITE ........ PASS'
  write(iu,'(A)') '   K SYMMETRY / POSITIVE DEFINITE ........ PASS'
  write(iu,'(A)') '   C SYMMETRY ............................. PASS'
  write(iu,'(A,I6)') ' INTERFACES: ',fnint
  write(iu,'(A)')&
  &'   #   CONNECT    COMPONENT     X_DOF     Z_DOF'
  do ii=1,fnint
    if(ficonn(ii).eq.1) then
      write(iu,'(I5,3X,A7,3I11)') ii,'SUPPORT',fiidx(ii),&
      &fixd(ii),fizd(ii)
    else
      write(iu,'(I5,3X,A7,3I11)') ii,'BEARING',fiidx(ii),&
      &fixd(ii),fizd(ii)
    endif
  enddo
  write(iu,'(A)')&
  &' MODE       UNDAMPED FREQUENCY [Hz]       MODAL DAMPING [%]'
  do i=1,n
    mmod=0.0_lrk
    cmod=0.0_lrk
    do j=1,n
      do k=1,n
        mmod=mmod+phi(j,i)*fm(j,k)*phi(k,i)
        cmod=cmod+phi(j,i)*fc(j,k)*phi(k,i)
      enddo
    enddo
    omega=2.0_lrk*pi*freq(i)
    zeta=0.0_lrk
    if(omega.gt.0.0_lrk .and. mmod.gt.0.0_lrk)zeta=cmod/(2.0_lrk*omega*mmod)
    write(iu,'(I6,8X,F18.6,12X,F12.5)') i,freq(i),100.0_lrk*zeta
  enddo
  close(iu)
  write(*,'(A,I5,A,F12.4,A)')&
  &' foundation: ',n,' DOF, first mode = ',freq(1),' Hz'
  ok=0
  return
end subroutine foundation_modal_audit
!
subroutine foundation_add_mass(dm,mm,ldm)
  use com_fndmat, only: fm, fk, fc
  use rd_kinds, only: lrk
  implicit none
  logical :: foundation_dynamic_f
  integer :: mtg, mxi
  parameter(mtg=500,mxi=20)
  integer :: dm, ldm, nf, i, j, gi, gj, foundation_ndof_f
  real(lrk) :: mm(ldm,ldm)
  if(foundation_dynamic_f()) return
  nf=foundation_ndof_f()
  if(nf.le.0) return
  do i=1,nf
    gi=dm-nf+i
    do j=1,nf
      gj=dm-nf+j
      mm(gi,gj)=mm(gi,gj)+fm(i,j)
    enddo
  enddo
  return
end subroutine foundation_add_mass
!
subroutine foundation_add_kc(dm,wk,wc,ldm)
  use com_fndmat, only: fm, fk, fc
  use rd_kinds, only: lrk, wp
  implicit none
  logical :: foundation_dynamic_f
  integer :: mtg, mxi
  parameter(mtg=500,mxi=20)
  integer :: dm, ldm, nf, i, j, gi, gj, foundation_ndof_f
  complex(wp) :: wk(ldm,ldm), wc(ldm,ldm)
  if(foundation_dynamic_f()) return
  nf=foundation_ndof_f()
  if(nf.le.0) return
  do i=1,nf
    gi=dm-nf+i
    do j=1,nf
      gj=dm-nf+j
      wk(gi,gj)=wk(gi,gj)+cmplx(fk(i,j),0.0_lrk, kind=wp)
      wc(gi,gj)=wc(gi,gj)+cmplx(fc(i,j),0.0_lrk, kind=wp)
    enddo
  enddo
  return
end subroutine foundation_add_kc
!
subroutine foundation_interface(kind,idx,xd,zd,found)
  use com_fndctl, only: factive, fcompare, fndof, fnint, fdkind
  use com_fndifc, only: ficonn, fiidx, fixd, fizd
  implicit none
  integer :: mtg, mxi
  parameter(mtg=500,mxi=20)
  integer :: kind, idx, xd, zd, i
  logical :: found
  found=.false.
  xd=0
  zd=0
  if(.not.factive) return
  do i=1,fnint
    if(ficonn(i).eq.kind .and. fiidx(i).eq.idx) then
      xd=fixd(i)
      zd=fizd(i)
      found=.true.
      return
    endif
  enddo
  return
end subroutine foundation_interface
!
subroutine foundation_add_support(jj,isup,dm,pp,hx,wk,wc,ldm)
  use com_sp2, only: sup, sps
  use rd_kinds, only: lrk, wp
  implicit none
  integer :: mtg, mxi
  parameter(mtg=500,mxi=20)
  integer :: jj, isup, dm, pp, hx, ldm, xd, zd, fx, fz
  integer :: foundation_global_dof
  logical :: found
  complex(wp) :: wk(ldm,ldm), wc(ldm,ldm)
  call foundation_interface(1,jj,xd,zd,found)
  if(.not.found) return
  fx=foundation_global_dof(dm,xd)
  fz=foundation_global_dof(dm,zd)
!     stiffness housing-foundation cross terms
  wk(hx,fx)=wk(hx,fx)-sup(jj,1)*sps
  wk(hx,fz)=wk(hx,fz)-sup(jj,2)*sps
  wk(hx+1,fx)=wk(hx+1,fx)-sup(jj,3)*sps
  wk(hx+1,fz)=wk(hx+1,fz)-sup(jj,4)*sps
  wk(fx,hx)=wk(fx,hx)-sup(jj,1)*sps
  wk(fx,hx+1)=wk(fx,hx+1)-sup(jj,2)*sps
  wk(fz,hx)=wk(fz,hx)-sup(jj,3)*sps
  wk(fz,hx+1)=wk(fz,hx+1)-sup(jj,4)*sps
  wk(fx,fx)=wk(fx,fx)+sup(jj,1)*sps
  wk(fx,fz)=wk(fx,fz)+sup(jj,2)*sps
  wk(fz,fx)=wk(fz,fx)+sup(jj,3)*sps
  wk(fz,fz)=wk(fz,fz)+sup(jj,4)*sps
!     damping
  wc(hx,fx)=wc(hx,fx)-sup(jj,5)*sps
  wc(hx,fz)=wc(hx,fz)-sup(jj,6)*sps
  wc(hx+1,fx)=wc(hx+1,fx)-sup(jj,7)*sps
  wc(hx+1,fz)=wc(hx+1,fz)-sup(jj,8)*sps
  wc(fx,hx)=wc(fx,hx)-sup(jj,5)*sps
  wc(fx,hx+1)=wc(fx,hx+1)-sup(jj,6)*sps
  wc(fz,hx)=wc(fz,hx)-sup(jj,7)*sps
  wc(fz,hx+1)=wc(fz,hx+1)-sup(jj,8)*sps
  wc(fx,fx)=wc(fx,fx)+sup(jj,5)*sps
  wc(fx,fz)=wc(fx,fz)+sup(jj,6)*sps
  wc(fz,fx)=wc(fz,fx)+sup(jj,7)*sps
  wc(fz,fz)=wc(fz,fz)+sup(jj,8)*sps
  return
end subroutine foundation_add_support
!
subroutine foundation_add_bearing(ib,dm,rx,par,wk,wc,ldm)
  use rd_kinds, only: lrk, wp
  implicit none
  integer :: ib, dm, rx, ldm, xd, zd, fx, fz
  integer :: foundation_global_dof
  real(lrk) :: par(9,10)
  logical :: found
  complex(wp) :: wk(ldm,ldm), wc(ldm,ldm)
  call foundation_interface(2,ib,xd,zd,found)
  if(.not.found) return
  fx=foundation_global_dof(dm,xd)
  fz=foundation_global_dof(dm,zd)
  wk(rx,fx)=wk(rx,fx)-par(ib,1)
  wk(rx,fz)=wk(rx,fz)-par(ib,2)
  wk(rx+1,fx)=wk(rx+1,fx)-par(ib,3)
  wk(rx+1,fz)=wk(rx+1,fz)-par(ib,4)
  wk(fx,rx)=wk(fx,rx)-par(ib,1)
  wk(fx,rx+1)=wk(fx,rx+1)-par(ib,2)
  wk(fz,rx)=wk(fz,rx)-par(ib,3)
  wk(fz,rx+1)=wk(fz,rx+1)-par(ib,4)
  wk(fx,fx)=wk(fx,fx)+par(ib,1)
  wk(fx,fz)=wk(fx,fz)+par(ib,2)
  wk(fz,fx)=wk(fz,fx)+par(ib,3)
  wk(fz,fz)=wk(fz,fz)+par(ib,4)
  wc(rx,fx)=wc(rx,fx)-par(ib,5)
  wc(rx,fz)=wc(rx,fz)-par(ib,6)
  wc(rx+1,fx)=wc(rx+1,fx)-par(ib,7)
  wc(rx+1,fz)=wc(rx+1,fz)-par(ib,8)
  wc(fx,rx)=wc(fx,rx)-par(ib,5)
  wc(fx,rx+1)=wc(fx,rx+1)-par(ib,6)
  wc(fz,rx)=wc(fz,rx)-par(ib,7)
  wc(fz,rx+1)=wc(fz,rx+1)-par(ib,8)
  wc(fx,fx)=wc(fx,fx)+par(ib,5)
  wc(fx,fz)=wc(fx,fz)+par(ib,6)
  wc(fz,fx)=wc(fz,fx)+par(ib,7)
  wc(fz,fz)=wc(fz,fz)+par(ib,8)
  return
end subroutine foundation_add_bearing
!
subroutine foundation_mode_participation(pp)
  use com_epm, only: fi, fn, avl, ddm
  use com_epmq, only: modal_condition, modal_valid
  use com_mat, only: mm, mg, dm, smn
  use rd_kinds, only: lrk, wp
  implicit none
!     Shared modal-conditioning diagnostics.
!     Keep this include fixed-form compatible.
  integer :: mtq
  parameter (mtq = 1000)
!
!     Scale-invariant biorthogonal conditioning floor.
  real(wp) :: modal_condition_min
  parameter (modal_condition_min = 1e-10_wp)
  integer :: mtg, mte
  parameter(mtg=500,mte=1000)
  integer :: pp, nf, nr, nsd, im, k, i, j, nm
  integer :: foundation_ndof_f, chklamf
  complex(wp) :: vi, vj, term
  real(wp) :: er, es, ef, et, frq, pi
  parameter(pi=3.14159265358979323846_wp)
  nf=foundation_ndof_f()
  if(nf.le.0) return
  nsd=2*pp
  nr=dm-nsd-nf
  if(nr.lt.1) return
  nm=min(20,dm/2)
  open(75,file='foundation_participation.out',status='replace')
  write(75,'(A)')&
  &'============================================================'
  write(75,'(A)') ' ROTORDIN - COUPLED MODE MASS PARTICIPATION'
  write(75,'(A)')&
  &'============================================================'
  write(75,'(A)')&
  &' MODE   FREQ[Hz]     ROTOR[%]   SUPPORT[%] FOUNDATION[%]'
  do im=1,nm
    k=chklamf(avl,im,ddm,mte)
    if(k.lt.1) cycle
    if(k.le.mtq) then
      if(.not.modal_valid(k)) cycle
    endif
    er=0.0_wp
    es=0.0_wp
    ef=0.0_wp
!       Mass-metric partition.  The current assembled M matrix has no
!       inertial coupling between rotor, bearing-support and foundation
!       blocks, so these block energies provide an auditable partition.
    do i=1,nr
      vi=fi(i,k)
      do j=1,nr
        vj=fi(j,k)
        term=conjg(vi)*cmplx(mm(i,j),0.0_wp, kind=wp)*vj
        er=er+real(term, wp)
      enddo
    enddo
    do i=nr+1,nr+nsd
      vi=fi(i,k)
      do j=nr+1,nr+nsd
        vj=fi(j,k)
        term=conjg(vi)*cmplx(mm(i,j),0.0_wp, kind=wp)*vj
        es=es+real(term, wp)
      enddo
    enddo
    do i=nr+nsd+1,dm
      vi=fi(i,k)
      do j=nr+nsd+1,dm
        vj=fi(j,k)
        term=conjg(vi)*cmplx(mm(i,j),0.0_wp, kind=wp)*vj
        ef=ef+real(term, wp)
      enddo
    enddo
    er=max(er,0.0_wp)
    es=max(es,0.0_wp)
    ef=max(ef,0.0_wp)
    et=er+es+ef
    if(et.le.0.0_wp) cycle
    frq=abs(avl(k))/(2.0_wp*pi)
    write(75,'(I5,2X,F11.4,3(3X,F10.4))') im,frq,100.0_wp*er/et,100.0_wp*es/et,100.0_wp*ef/et
  enddo
  close(75)
  return
end subroutine foundation_mode_participation
!
subroutine foundation_response_begin()
  implicit none
  integer :: nf, foundation_ndof_f
  nf=foundation_ndof_f()
  if(nf.le.0) return
  open(76,file='foundation_response.csv',status='replace')
  write(76,'(A)')&
  &'rpm,interface,connect,index,x_dof,z_dof,'//&
  &'qx_re_m,qx_im_m,qz_re_m,qz_im_m,'//&
  &'fx_re_N,fx_im_N,fz_re_N,fz_im_N,'//&
  &'qx_abs_m,qz_abs_m,fx_abs_N,fz_abs_N'
  close(76)
  return
end subroutine foundation_response_begin
!
subroutine foundation_response_point(rpm,omega,rsp,ldm)
  use com_fndctl, only: factive, fcompare, fndof, fnint, fdkind
  use com_fndifc, only: ficonn, fiidx, fixd, fizd
  use com_fndmat, only: fm, fk, fc
  use com_mat, only: mm, mg, dm, smn
  use rd_kinds, only: lrk, wp
  implicit none
  integer :: mtg, mxi
  parameter(mtg=500,mxi=20)
  integer :: ldm, nf
  integer :: i, j, gi, xd, zd, kind, idx, oki
  character(len=99) :: dem
  integer :: foundation_ndof_f, foundation_global_dof
  real(lrk) :: rpm, omega
  logical :: foundation_dynamic_f
  complex(wp) :: rsp(ldm,1), q(mtg), fr(mtg), zij, zdyn(4,4)
  character(len=7) :: cname
  nf=foundation_ndof_f()
  if(nf.le.0) return
  do i=1,nf
    gi=foundation_global_dof(dm,i)
    q(i)=rsp(gi,1)
    fr(i)=cmplx(0.0_wp,0.0_wp, kind=wp)
  enddo
  if(foundation_dynamic_f()) then
    call foundation_dynamic_matrix(omega,zdyn,dem,oki)
    if(oki.lt.0) return
    do i=1,nf
      do j=1,nf
        fr(i)=fr(i)+zdyn(i,j)*q(j)
      enddo
    enddo
  else
    do i=1,nf
      do j=1,nf
        zij=cmplx(fk(i,j)-omega*omega*fm(i,j),omega*fc(i,j), kind=wp)
        fr(i)=fr(i)+zij*q(j)
      enddo
    enddo
  endif
  open(76,file='foundation_response.csv',status='old',&
  &position='append')
  do i=1,fnint
    kind=ficonn(i)
    idx=fiidx(i)
    xd=fixd(i)
    zd=fizd(i)
    if(kind.eq.1) then
      cname='SUPPORT'
    else
      cname='BEARING'
    endif
    write(76,10) rpm,i,trim(cname),idx,xd,zd,real(q(xd), wp),aimag(q(xd)),real(q(zd), wp),aimag(q(zd)),real(fr(xd), &
      & wp),aimag(fr(xd)),real(fr(zd), wp),aimag(fr(zd)),abs(q(xd)),abs(q(zd)),abs(fr(xd)),abs(fr(zd))
  enddo
  close(76)
  return
10 format(F12.4,',',I4,',',A,',',I4,',',I4,',',I4,&
  &12(',',ES16.8))
end subroutine foundation_response_point
!
subroutine foundation_warn_legacy_analysis(name)
  use com_fndctl, only: factive, fcompare, fndof, fnint, fdkind
  implicit none
  character(len=*) :: name
  if(.not.factive) return
  write(*,'(A,A,A)')&
  &' WARNING: FOUNDATION active; ',trim(name),&
  &' uses legacy rigid-foundation formulation in V1.'
  write(*,'(A)')&
  &'          Use coupled Campbell/modal/forced/transient analysis.'
  return
end subroutine foundation_warn_legacy_analysis

!
!     ==================================================================
!     Frequency-domain dynamic foundation stiffness table.
!     V1 supports the customer 4-DOF reciprocal interface matrix:
!       q=[DE-X,DE-Z,NDE-X,NDE-Z]
!     Term order in each input row:
!       11,22,33,44,12,34,13,24,14,23
!     Complex samples are constructed before interpolation, so the
!     interpolation is linear in real/imaginary parts and is safe across
!     the +180/-180 degree phase wrap.
!
subroutine foundation_dynamic_reset()
  use com_fnddynf, only: fdfreq
  use com_fnddyni, only: fdmodel, fdnpt
  use com_fnddynz, only: fdz
  use rd_kinds, only: lrk, wp
  implicit none
  integer :: mxfpt
  parameter(mxfpt=2048)
  integer :: i, k
  fdmodel=0
  fdnpt=0
  do i=1,mxfpt
    fdfreq(i)=0.0_lrk
    do k=1,10
      fdz(k,i)=cmplx(0.0_wp,0.0_wp, kind=wp)
    enddo
  enddo
  return
end subroutine foundation_dynamic_reset
!
subroutine foundation_dynamic_set_model(model)
  use com_fnddyni, only: fdmodel, fdnpt
  implicit none
  integer :: model
  fdmodel=model
  if(model.eq.0) fdnpt=0
  return
end subroutine foundation_dynamic_set_model
!
subroutine foundation_load_dynamic_inline(iu,errmsg,ok)
  use com_fndctl, only: factive, fcompare, fndof, fnint, fdkind
  use com_fndifc, only: ficonn, fiidx, fixd, fizd
  use com_fndstr, only: ffile, fred
  implicit none
  integer :: mtg, mxi
  parameter(mtg=500,mxi=20)
  integer :: iu, ok, ios, i, nif
  character(len=*) :: errmsg
  character(len=512) :: line, uline
  character(len=32) :: key, val2
  ok=-1
  factive=.true.
  fcompare=.false.
  fndof=0
  fnint=0
  fdkind=0
  ffile='INLINE_DYNAMIC_STIFFNESS'
  fred='DYN_STIFF_TABLE'
10 continue
  read(iu,'(A)',iostat=ios) line
  if(ios.ne.0) then
    errmsg='foundation: missing END_FOUNDATION'
    return
  endif
  if(len_trim(line).eq.0) goto 10
  uline=adjustl(line)
  if(uline(1:1).eq.'#') goto 10
  call foundation_upper(uline)
  if(index(uline,'END_FOUNDATION').eq.1 .or.&
  &index(uline,'END,FOUNDATION').eq.1) goto 90
  read(uline,*,iostat=ios) key
  if(ios.ne.0) goto 10
  call foundation_upper(key)
  if(trim(key).eq.'NDOF') then
    read(uline,*,iostat=ios) key,fndof
    if(ios.ne.0 .or. fndof.ne.4) then
      errmsg='foundation: dynamic stiffness requires NDOF 4'
      return
    endif
  else if(trim(key).eq.'COMPARE') then
    read(uline,*,iostat=ios) key,i
    if(ios.eq.0) fcompare=(i.ne.0)
  else if(trim(key).eq.'FNDIFACE') then
    read(uline,*,iostat=ios) key,nif
    if(ios.ne.0 .or. nif.lt.1 .or. nif.gt.mxi) then
      errmsg='foundation: invalid FNDIFACE count'
      return
    endif
    fnint=nif
!       Consume UI header row.
20  read(iu,'(A)',iostat=ios) line
    if(ios.ne.0) then
      errmsg='foundation: missing FNDIFACE header'
      return
    endif
    if(len_trim(line).eq.0) goto 20
    uline=adjustl(line)
    if(uline(1:1).eq.'#') goto 20
    do i=1,fnint
30    read(iu,'(A)',iostat=ios) line
      if(ios.ne.0) then
        errmsg='foundation: missing FNDIFACE row'
        return
      endif
      if(len_trim(line).eq.0) goto 30
      uline=adjustl(line)
      if(uline(1:1).eq.'#') goto 30
      call foundation_upper(uline)
      read(uline,*,iostat=ios) key,val2,fiidx(i),fixd(i),&
      &fizd(i)
      if(ios.ne.0) then
        errmsg='foundation: invalid FNDIFACE row'
        return
      endif
      call foundation_upper(val2)
      if(trim(val2).eq.'SUPPORT') then
        ficonn(i)=1
      else if(trim(val2).eq.'BEARING') then
        ficonn(i)=2
      else
        errmsg='foundation: FNDCONN must be SUPPORT or BEARING'
        return
      endif
    enddo
  else if(trim(key).eq.'FDYNSTIFF') then
    if(fndof.ne.4) then
      errmsg='foundation: NDOF 4 must precede FDYNSTIFF'
      return
    endif
    call foundation_read_dyn_table(iu,errmsg,ok)
    if(ok.lt.0) return
  else
    errmsg='foundation: unknown dynamic keyword '//trim(key)
    return
  endif
  goto 10
90 continue
  ok=0
  return
end subroutine foundation_load_dynamic_inline
!
subroutine foundation_read_dyn_table(iu,errmsg,ok)
  use com_fnddynf, only: fdfreq
  use com_fnddyni, only: fdmodel, fdnpt
  use com_fnddynz, only: fdz
  use rd_kinds, only: lrk, wp
  implicit none
  integer :: mxfpt
  parameter(mxfpt=2048)
  integer :: iu, ok, ios, npt, i, k, hfreq, hfmt
  character(len=*) :: errmsg
  character(len=512) :: line, uline
  character(len=32) :: key, val
  real(lrk) :: amp(10), pha(10), fq, scale, pi
  parameter(pi=3.14159265358979323846_lrk)
  npt=0
  scale=-1.0_lrk
  hfreq=0
  hfmt=0
  ok=-1
10 continue
  read(iu,'(A)',iostat=ios) line
  if(ios.ne.0) then
    errmsg='foundation: unexpected EOF in FDYNSTIFF'
    return
  endif
  if(len_trim(line).eq.0) goto 10
  uline=adjustl(line)
  if(uline(1:1).eq.'#') goto 10
  call foundation_upper(uline)
  read(uline,*,iostat=ios) key
  if(ios.ne.0) goto 10
  call foundation_upper(key)
  if(trim(key).eq.'FREQUENCY_UNIT') then
    call foundation_rest(line,val)
    call foundation_upper(val)
    if(trim(val).ne.'HZ') then
      errmsg='foundation: FDYNSTIFF frequency unit must be HZ'
      return
    endif
    hfreq=1
  else if(trim(key).eq.'STIFFNESS_UNIT') then
    call foundation_rest(line,val)
    call foundation_upper(val)
    if(trim(val).eq.'N/MM') then
      scale=1000.0_lrk
    else if(trim(val).eq.'N/M') then
      scale=1.0_lrk
    else
      errmsg='foundation: FDYNSTIFF unit must be N/MM or N/M'
      return
    endif
  else if(trim(key).eq.'FORMAT') then
    call foundation_rest(line,val)
    call foundation_upper(val)
    if(trim(val).ne.'AMPLITUDE_PHASE') then
      errmsg='foundation: FDYNSTIFF FORMAT must be AMPLITUDE_PHASE'
      return
    endif
    hfmt=1
  else if(trim(key).eq.'NPOINTS') then
    read(uline,*,iostat=ios) key,npt
    if(ios.ne.0 .or. npt.lt.2 .or. npt.gt.mxfpt) then
      errmsg='foundation: invalid FDYNSTIFF NPOINTS'
      return
    endif
    if(scale.le.0.0_lrk .or. hfreq.eq.0 .or. hfmt.eq.0) then
      errmsg='foundation: units/format must precede NPOINTS'
      return
    endif
    do i=1,npt
20    read(iu,'(A)',iostat=ios) line
      if(ios.ne.0) then
        errmsg='foundation: missing FDYNSTIFF data row'
        return
      endif
      if(len_trim(line).eq.0) goto 20
      uline=adjustl(line)
      if(uline(1:1).eq.'#') goto 20
      read(uline,*,iostat=ios) fq,&
      &(amp(k),pha(k),k=1,10)
      if(ios.ne.0 .or. fq.le.0.0_lrk) then
        errmsg='foundation: invalid FDYNSTIFF data row'
        return
      endif
      if(i.gt.1) then
        if(fq.le.fdfreq(i-1)) then
          errmsg='foundation: FDYNSTIFF frequency not increasing'
          return
        endif
      endif
      fdfreq(i)=fq
      do k=1,10
        fdz(k,i)=cmplx(scale*amp(k)*cos(pha(k)*pi/180.0_lrk),scale*amp(k)*sin(pha(k)*pi/180.0_lrk), kind=wp)
      enddo
    enddo
    fdnpt=npt
!       Require an explicit END_FDYNSTIFF after the rows.
30  read(iu,'(A)',iostat=ios) line
    if(ios.ne.0) then
      errmsg='foundation: missing END_FDYNSTIFF'
      return
    endif
    if(len_trim(line).eq.0) goto 30
    uline=adjustl(line)
    if(uline(1:1).eq.'#') goto 30
    call foundation_upper(uline)
    if(index(uline,'END_FDYNSTIFF').ne.1) then
      errmsg='foundation: expected END_FDYNSTIFF'
      return
    endif
    ok=0
    return
  else if(trim(key).eq.'END_FDYNSTIFF') then
    errmsg='foundation: FDYNSTIFF missing NPOINTS/data'
    return
  else
    errmsg='foundation: unknown FDYNSTIFF keyword '//trim(key)
    return
  endif
  goto 10
end subroutine foundation_read_dyn_table
!
subroutine foundation_dynamic_validate(errmsg,ok)
  use com_fnddynf, only: fdfreq
  use com_fnddyni, only: fdmodel, fdnpt
  use rd_kinds, only: lrk
  implicit none
  integer :: mxfpt
  parameter(mxfpt=2048)
  integer :: ok, i
  character(len=*) :: errmsg
  ok=-1
  if(fdmodel.ne.1) then
    ok=0
    return
  endif
  if(fdnpt.lt.2) then
    errmsg='foundation: dynamic stiffness table is empty'
    return
  endif
  do i=2,fdnpt
    if(fdfreq(i).le.fdfreq(i-1)) then
      errmsg='foundation: dynamic frequencies not increasing'
      return
    endif
  enddo
  ok=0
  return
end subroutine foundation_dynamic_validate
!
subroutine foundation_dynamic_matrix(omega,z,errmsg,ok)
  use com_fnddynf, only: fdfreq
  use com_fnddyni, only: fdmodel, fdnpt
  use com_fnddynz, only: fdz
  use rd_kinds, only: lrk, wp
  implicit none
  integer :: mxfpt
  parameter(mxfpt=2048)
  real(lrk) :: omega, freq, t, pi
  integer :: ok, i, k
  complex(wp) :: zt(10), z(4,4)
  character(len=*) :: errmsg
  parameter(pi=3.14159265358979323846_lrk)
  ok=-1
  do i=1,4
    do k=1,4
      z(i,k)=cmplx(0.0_wp,0.0_wp, kind=wp)
    enddo
  enddo
  if(fdmodel.ne.1) then
    ok=0
    return
  endif
  freq=omega/(2.0_lrk*pi)
  if(freq.lt.fdfreq(1)-1.0e-5_lrk .or.freq.gt.fdfreq(fdnpt)+1.0e-5_lrk) then
    write(errmsg,'(A,F10.4,A,F10.4,A,F10.4,A)')&
    &'foundation: vibration frequency ',freq,&
    &' Hz outside FDYNSTIFF [',fdfreq(1),',',&
    &fdfreq(fdnpt),'] Hz'
    return
  endif
  if(freq.le.fdfreq(1)) then
    do k=1,10
      zt(k)=fdz(k,1)
    enddo
  else if(freq.ge.fdfreq(fdnpt)) then
    do k=1,10
      zt(k)=fdz(k,fdnpt)
    enddo
  else
    do i=1,fdnpt-1
      if(freq.ge.fdfreq(i) .and. freq.le.fdfreq(i+1)) then
        t=(freq-fdfreq(i))/(fdfreq(i+1)-fdfreq(i))
        do k=1,10
!             Complex linear interpolation == separate Re/Im linear
!             interpolation. Never interpolate wrapped phase angles.
          zt(k)=(1.0_lrk-t)*fdz(k,i)+t*fdz(k,i+1)
        enddo
        goto 20
      endif
    enddo
  endif
20 continue
!     Customer reciprocal 4-DOF mapping.
  z(1,1)=zt(1)
  z(2,2)=zt(2)
  z(3,3)=zt(3)
  z(4,4)=zt(4)
  z(1,2)=zt(5)
  z(2,1)=zt(5)
  z(3,4)=zt(6)
  z(4,3)=zt(6)
  z(1,3)=zt(7)
  z(3,1)=zt(7)
  z(2,4)=zt(8)
  z(4,2)=zt(8)
  z(1,4)=zt(9)
  z(4,1)=zt(9)
  z(2,3)=zt(10)
  z(3,2)=zt(10)
  ok=0
  return
end subroutine foundation_dynamic_matrix
!
subroutine foundation_add_dynamic_hh(omega,dm,hh,ldm,&
&errmsg,ok)
  use rd_kinds, only: lrk, wp
  implicit none
  integer :: dm, ldm, ok, i, j, gi, gj, ki, nf, foundation_ndof_f
  real(lrk) :: omega
  complex(wp) :: hh(ldm,ldm), z(4,4), jr
  character(len=*) :: errmsg
  logical :: foundation_dynamic_f
  ok=0
  if(.not.foundation_dynamic_f()) return
  nf=foundation_ndof_f()
  if(nf.ne.4) then
    errmsg='foundation: dynamic HH assembly requires 4 DOF'
    ok=-1
    return
  endif
  call foundation_dynamic_matrix(omega,z,errmsg,ok)
  if(ok.lt.0) return
  jr=cmplx(0.0_wp,omega, kind=wp)
  do i=1,4
    gi=dm-nf+i
    do j=1,4
      gj=dm-nf+j
      hh(gi,gj)=hh(gi,gj)+z(i,j)
    enddo
!       mntmth uses a doubled first-order algebraic system. Dynamic-
!       table foundation DOFs have no physical Mf, so their auxiliary
!       velocity rows would otherwise be identically zero. Enforce the
!       neutral kinematic constraint v=j*w*q with unit algebraic rows;
!       these rows do not feed back into the physical top equation.
    ki=gi+dm
    hh(ki,gi)=jr
    hh(ki,ki)=cmplx(-1.0_wp,0.0_wp, kind=wp)
  enddo
  return
end subroutine foundation_add_dynamic_hh
!
subroutine foundation_dynamic_audit(errmsg,ok)
  use com_fndctl, only: factive, fcompare, fndof, fnint, fdkind
  use com_fnddynf, only: fdfreq
  use com_fnddyni, only: fdmodel, fdnpt
  use com_fndifc, only: ficonn, fiidx, fixd, fizd
  use rd_kinds, only: lrk
  implicit none
  integer :: mxfpt, mxi
  parameter(mxfpt=2048,mxi=20)
  integer :: i, iu, ok
  character(len=*) :: errmsg
  character(len=7) :: cname
  ok=-1
  if(fdmodel.ne.1) then
    ok=0
    return
  endif
  iu=74
  open(iu,file='foundation_audit.out',status='replace')
  write(iu,'(A)')&
  &'============================================================'
  write(iu,'(A)') ' ROTORDIN - FOUNDATION DYNAMIC STIFFNESS AUDIT'
  write(iu,'(A)')&
  &'============================================================'
  write(iu,'(A)') ' FOUNDATION_MODEL: DYNAMIC_STIFFNESS_TABLE'
  write(iu,'(A,I6)') ' FOUNDATION DOF: ',fndof
  write(iu,'(A,I6)') ' TABLE POINTS: ',fdnpt
  write(iu,'(A,F12.4)') ' FREQUENCY MIN [Hz]: ',fdfreq(1)
  write(iu,'(A,F12.4)') ' FREQUENCY MAX [Hz]: ',fdfreq(fdnpt)
  write(iu,'(A)') ' INTERNAL STIFFNESS UNIT: N/M'
  write(iu,'(A)') ' INPUT FORMAT: AMPLITUDE_PHASE'
  write(iu,'(A)') ' INTERPOLATION: LINEAR REAL_IMAG'
  write(iu,'(A)') ' RECIPROCAL 4X4 MATRIX: YES (10 TERMS)'
  write(iu,'(A)') ' SUPPORTED ANALYSIS: LATERAL FREQUENCY RESPONSE'
  write(iu,'(A,I6)') ' INTERFACES: ',fnint
  do i=1,fnint
    if(ficonn(i).eq.1) then
      cname='SUPPORT'
    else
      cname='BEARING'
    endif
    write(iu,'(I5,3X,A7,3I11)') i,cname,fiidx(i),&
    &fixd(i),fizd(i)
  enddo
  close(iu)
  ok=0
  return
end subroutine foundation_dynamic_audit
