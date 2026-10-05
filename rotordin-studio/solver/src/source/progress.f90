!
!     Unified processing/progress telemetry for RotorDin.
!     Human-readable in interactive mode and machine-readable in -std.
!     Up to 24 concurrent/nested analysis contexts are tracked by name.
!
subroutine progress_init(std)
  use com_progress_cfg, only: pgstd, pgon, pglast, pgstart, pgname
  use rd_kinds, only: lrk
  implicit none
  logical :: std
  integer :: i
  pgstd=std
  pgon=.true.
  do i=1,24
    pglast(i)=-1
    pgstart(i)=0.0_lrk
    pgname(i)=' '
  enddo
  return
end subroutine progress_init
!
subroutine progress_slot(name,create,slot)
  use com_progress_cfg, only: pgstd, pgon, pglast, pgstart, pgname
  use rd_kinds, only: lrk
  implicit none
  character(len=*) :: name
  logical :: create
  integer :: slot, i, free
  slot=0
  free=0
  do i=1,24
    if(len_trim(pgname(i)).eq.0 .and. free.eq.0) free=i
    if(trim(pgname(i)).eq.trim(name)) then
      slot=i
      return
    endif
  enddo
  if(create .and. free.gt.0) then
    slot=free
    pgname(slot)=adjustl(name)
    pglast(slot)=-1
    pgstart(slot)=0.0_lrk
  endif
  return
end subroutine progress_slot
!
subroutine progress_begin(name,total,detail)
  use com_progress_cfg, only: pgstd, pgon, pglast, pgstart, pgname
  use rd_kinds, only: lrk
  implicit none
  character(len=*) :: name, detail
  integer :: total, slot
  if(.not.pgon) return
  call progress_slot(name,.true.,slot)
  if(slot.gt.0) then
    pglast(slot)=-1
    call cpu_time(pgstart(slot))
  endif
  if(pgstd) then
    write(0,'(A,A,A,I0,A,A)') '#PROGRESS ANALYSIS=',trim(name),&
    &' EVENT=BEGIN TOTAL=',total,' DETAIL=',trim(detail)
  else
    write(6,'(A,A,A,I0,A,A)') '[',trim(name),&
    &'] BEGIN | total=',total,' | ',trim(detail)
  endif
  if(pgstd) then
    flush(0)
  else
    flush(6)
  endif
  return
end subroutine progress_begin
!
subroutine progress_stage(name,detail)
  use com_progress_cfg, only: pgstd, pgon, pglast, pgstart, pgname
  use rd_kinds, only: lrk
  implicit none
  character(len=*) :: name, detail
  if(.not.pgon) return
  if(pgstd) then
    write(0,'(A,A,A,A)') '#PROGRESS ANALYSIS=',trim(name),&
    &' EVENT=STAGE DETAIL=',trim(detail)
  else
    write(6,'(A,A,A,A)') '[',trim(name),'] ',trim(detail)
  endif
  if(pgstd) then
    flush(0)
  else
    flush(6)
  endif
  return
end subroutine progress_stage
!
subroutine progress_update(name,current,total,value,label)
  use com_progress_cfg, only: pgstd, pgon, pglast, pgstart, pgname
  use rd_kinds, only: lrk
  implicit none
  character(len=*) :: name, label
  integer :: current, total, pct, slot
  real(lrk) :: value, percent
  if(.not.pgon) return
  if(total.le.0) return
  call progress_slot(name,.true.,slot)
  pct=int(100.0_lrk*real(current, lrk)/real(total, lrk))
  if(current.ge.total) pct=100
  if(slot.gt.0) then
    if(current.gt.1 .and. current.lt.total .and.&
    &pct.le.pglast(slot)) return
    if(current.gt.1 .and. current.lt.total .and.&
    &pct.lt.pglast(slot)+1) return
    pglast(slot)=pct
  endif
  percent=100.0_lrk*real(current, lrk)/real(total, lrk)
  if(percent.gt.100.0_lrk) percent=100.0_lrk
  if(pgstd) then
    write(0,'(A,A,A,F6.1,A,I0,A,I0,A,A,A,ES14.6)')&
    &'#PROGRESS ANALYSIS=',trim(name),' EVENT=UPDATE PERCENT=',&
    &percent,' CURRENT=',current,' TOTAL=',total,' ',trim(label),&
    &'=',value
  else
    write(6,'(A,A,A,F6.1,A,I0,A,I0,A,A,A,ES14.6)')&
    &'[',trim(name),'] ',percent,'% | ',current,'/',total,&
    &' | ',trim(label),'=',value
  endif
  if(pgstd) then
    flush(0)
  else
    flush(6)
  endif
  return
end subroutine progress_update
!

subroutine progress_update2(name,current,total,value1,label1,&
&value2,label2)
  use com_progress_cfg, only: pgstd, pgon, pglast, pgstart, pgname
  use rd_kinds, only: lrk
  implicit none
  character(len=*) :: name, label1, label2
  integer :: current, total, pct, slot
  real(lrk) :: value1, value2, percent
  if(.not.pgon) return
  if(total.le.0) return
  call progress_slot(name,.true.,slot)
  pct=int(100.0_lrk*real(current, lrk)/real(total, lrk))
  if(current.ge.total) pct=100
  if(slot.gt.0) then
    if(current.gt.1 .and. current.lt.total .and.&
    &pct.le.pglast(slot)) return
    if(current.gt.1 .and. current.lt.total .and.&
    &pct.lt.pglast(slot)+1) return
    pglast(slot)=pct
  endif
  percent=100.0_lrk*real(current, lrk)/real(total, lrk)
  if(percent.gt.100.0_lrk) percent=100.0_lrk
  if(pgstd) then
    write(0,'(A,A,A,F6.1,A,I0,A,I0,A,A,A,ES14.6,A,A,A,ES14.6)')&
    &'#PROGRESS ANALYSIS=',trim(name),' EVENT=UPDATE PERCENT=',&
    &percent,' CURRENT=',current,' TOTAL=',total,' ',trim(label1),&
    &'=',value1,' ',trim(label2),'=',value2
  else
    write(6,'(A,A,A,F6.1,A,I0,A,I0,A,A,A,ES14.6,A,A,A,ES14.6)')&
    &'[',trim(name),'] ',percent,'% | ',current,'/',total,&
    &' | ',trim(label1),'=',value1,' | ',trim(label2),'=',value2
  endif
  if(pgstd) then
    flush(0)
  else
    flush(6)
  endif
  return
end subroutine progress_update2
!
subroutine progress_end(name,status)
  use com_progress_cfg, only: pgstd, pgon, pglast, pgstart, pgname
  use rd_kinds, only: lrk
  implicit none
  character(len=*) :: name, status
  integer :: slot
  real(lrk) :: pgend, elapsed
  if(.not.pgon) return
  call progress_slot(name,.false.,slot)
  call cpu_time(pgend)
  elapsed=0.0_lrk
  if(slot.gt.0) elapsed=max(0.0_lrk,pgend-pgstart(slot))
  if(pgstd) then
!       Keep machine-readable telemetry deterministic.  Wall/CPU timing
!       intentionally omitted in -std mode so redirected pipe/file runs
!       byte-identical structured result streams even if stderr is merge
    write(0,'(A,A,A,A)') '#PROGRESS ANALYSIS=',trim(name),&
    &' EVENT=END STATUS=',trim(status)
  else
    write(6,'(A,A,A,A,A,F10.4,A)') '[',trim(name),'] ',&
    &trim(status),' | elapsed=',elapsed,' s'
  endif
  if(pgstd) then
    flush(0)
  else
    flush(6)
  endif
  if(slot.gt.0) then
    pglast(slot)=-1
    pgstart(slot)=0.0_lrk
    pgname(slot)=' '
  endif
  return
end subroutine progress_end
!
