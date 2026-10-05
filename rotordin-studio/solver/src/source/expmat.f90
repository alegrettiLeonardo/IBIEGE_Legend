!     $Id$
!     ==================================================================
!
!>    @file expmat.f
!>    @brief import / expor matrices in Harwell-Boeing and
!>    matrix market formats, last changes:<br>
!>    new module - francisco - 17/11/2014<br>
!>    changed error messages variable name was emsg - francisco - jul/15
!>    export error messaged block moved to messages.f - francisco - mar-
!>    created character constants, updated old Fortran, added intrinsic
!>    added lateral get export speed function expspdf - francisco sep-21
!>    added kind export option - francisco sep-21<br>
!>    added export matrix dwritepl function - francisco nov-21.
!
!     =================================================================
!>    @brief DNNZ get non zero values of a double precision dense matrix
!>    Also values and coordinate vectors. See messages.f
!
!>    @param[in] nrow number of rows
!>    @param[in] ncol number of columns
!>    @param[in] dns input nrow x ncol(dense) matrix.
!>    @param[in] ndns first dimension of dns.
!>    @param[out] ja column indexes for non zero values
!>    @param[out] ia row indexes for non zero values
!>    @param[out] a non zero values
!>    @param[in] job = 0 only counter
!>    @returns number of non zero values
!
integer function dnnz(nrow,ncol,dns,ndns,ja,ia,a,job)
  use rd_kinds, only: wp
  implicit none
!
  integer :: nrow, ncol, ndns, job
  integer :: ja(*), ia(*)
  real(wp) :: a(*)
  real(wp) :: dns(ndns,*)
!
  real(wp) :: d0
  integer :: i, j, k
  parameter (d0 = 0._wp)
!
  intrinsic :: abs
!
  k = 0
  do i=1,nrow
    do j=1,ncol
      if(abs(dns(i,j)) .gt. d0) then
        k = k+1
        if (job .ne. 0) then
          a(k) = dns(i,j)
          ia(k) = i
          ja(k) = j
        end if
      end if
    end do
  end do
  dnnz = k
!
  return
!
end function dnnz
!
!     =================================================================
!>    @brief DNSCSR converts Dense to Compressed Row Sparse format;
!>    Converts a densely stored matrix into a row orientied
!>    compactly sparse matrix.(reverse of csrdns);<br>
!>    Note: this routine does not check whether an element
!>    is small; It considers that a(i,j) is zero if it is exactly
!>    equal to zero: see test below.
!
!>    @param[in] nrow number of rows
!>    @param[in] ncol number of colmns
!>    @param[in] nzmax maximum number of nonzero elements allowed.
!>    This should be set to be the lengths of the arrays a and ja.
!>    @param[in] dns input nrow x ncol (dense) matrix.
!>    @param[in] ndns first dimension of dns.
!>    @param[out] a value pointer array for output matrix
!>    @param[out] ja column pointer array for output matrix
!>    @param[out] ia row pointer array for output matrix
!>    @param[out] ierr integer error indicator:
!>    <ul><li>ierr .eq. 0 means normal return</li>
!>    <li>ierr .eq. i means that the the code stopped while
!>    processing row number i,because there was no space left in
!>    a,and ja(as defined by parameter nzmax).</li></ul>
!
subroutine dnscsr(nrow,ncol,nzmax,dns,ndns,a,ja,ia,ierr)
  use rd_kinds, only: lrk, wp
  implicit none
!
  integer :: nrow, ncol, nzmax, ndns, ierr
  real(wp) :: dns(ndns,*), a(*)
  integer :: ia(*), ja(*)
!
  integer :: i, j, next
!
  next = 1
  ia(1) = 1
  do i = 1,nrow
    do j=1,ncol
      if(dns(i,j) .eq. 0.0_lrk) cycle
      if(next .gt. nzmax) then
        ierr = i
        return
      end if
      ja(next) = j
      a(next) = dns(i,j)
      next = next+1
    end do
    ia(i+1) = next
  end do
!
  return
!
end subroutine dnscsr
!
!     =================================================================
!>    @brief CSRCSC converts Compressed Sparse Row to Compressed Sparse
!>    Column, transposition operation Not in place.
!
!>    @param[in] n dimension of A.
!>    @param[in] job integer to indicate whether or not to fill the valu
!>    of the matrix ao or only the pattern(ia,and ja). Enter 1 for yes.
!>    @param[in] ipos starting position in ao,jao of the transposed matr
!>    the iao array takes this into account(thus iao(1) is set to ipos.)
!>    Note: this may be useful if one needs to append the data structure
!>    of the transpose to that of A. In this case use
!>    call csrcsc(n,1,n+2,a,ja,ia,a,ja,ia(n+2)) for any other normal
!>    usage,enter ipos=1.
!>    @param[in] a double precision array of length nnz(nnz=number of
!>    nonzero elements in input matrix) containing the nonzero elements.
!>    @param[in] ja integer array of length nnz containing the column
!>    positions of the corresponding elements in a.
!>    @param[in] ia integer of size n+1. ia(k) contains the position in
!>    a,ja of the beginning of the k-th row.
!>    @param[out] ao double precision array of size nzz containing the
!>    "a" part of the transpose
!>    @param[out] jao integer array of size nnz containing the column
!>    indices.
!>    @param[out] iao integer array of size n+1 containing the "ia"
!>    index array of the transpose.
!
subroutine csrcsc(n,job,ipos,a,ja,ia,ao,jao,iao)
  use rd_kinds, only: wp
  implicit none
!
  integer :: n, job, ipos
  integer :: ia(n+1), iao(n+1), ja(*), jao(*)
  real(wp) :: a(*), ao(*)
!
  integer :: i, j, k, next
!
!     compute lengths of rows of transp(A)
  do i = 1,n+1
    iao(i) = 0
  end do
  do i = 1,n
    do k = ia(i),ia(i+1)-1
      j = ja(k)+1
      iao(j) = iao(j)+1
    end do
  end do
!     compute pointers from lengths
  iao(1) = ipos
  do i = 1,n
    iao(i+1) = iao(i)+iao(i+1)
  end do
!     now do the actual copying
  do i = 1,n
    do k = ia(i),ia(i+1)-1
      j = ja(k)
      next = iao(j)
      if(job .eq. 1) ao(next) = a(k)
      jao(next) = i
      iao(j) = next+1
    end do
  end do
!     reshift iao and leave
  do i = n,1,-1
    iao(i+1) = iao(i)
  end do
  iao(1) = ipos
!
  return
!
end subroutine csrcsc
!
!     =================================================================
!>    @brief PRTMT writes a matrix in Harwell-Boeing format into a file;
!>    Writes a matrix in Harwell-Boeing format into a file,
!>    assumes that the matrix is stored in COMPRESSED SPARSE COLUMN
!>    FORMAT; some limited functionality for right hand sides;<br>
!>    The matrix a,ja,ia will be written in output unit iounit in the
!>    Harwell-Boeing format; None of the inputs is modofied<br>
!>    Notes:<br>
!>    1)This code attempts to pack as many elements as possible per
!>    80-character line;<br>
!>    2)This code attempts to avoid as much as possible to put blanks
!>    in the formats that are written in the 4-line header (This is done
!>    for purely esthetical reasons since blanks are ignored in format
!>    descriptors);<br>
!>    3)Sparse formats for righr hand sides and guesses not suported.
!>    @author Youcef Saad - Date: Sept.,1989 - updated Oct. 31,1989 to
!>    cope with new format.
!
!>    @param[in] nrow number of rows in matrix
!>    @param[in] ncol number of columns in matrix
!>    @param[in] a double precision array containing the values of the
!>    matrix stored columnwise.
!>    @param[in] ja integer array of the same length as a containing the
!>    row indices of the corresponding matrix elements of array a.
!>    @param[in] ia integer array of containing the pointers to the
!>    beginning of the columns in arrays a,ja.
!>    @param[in] rhs  double precision array  containing the right hand
!>    side(s) and optionally the associated initial guesses and/or exact
!>    solutions in this order. See also guesol for details. the vector
!>    rhs will be used only if job .gt. 2(see below). Only full storage
!>    for the right hand sides is supported.
!>    @param[in] guesol a 2-character string indicating whether an
!>    initial guess (1-st character) and / or the exact solution(2-nd)
!>    character) is provided with the right hand side.<br>
!>    If the first character of guesol is 'G' it means that an intial
!>    guess is provided for each right hand sides.<br>
!>    These are assumed to be appended to the right hand sides in the
!>    array rhs.<br>
!>    if the second character of guesol is 'X' it means that an exact
!>    solution is provided for each right hand side.<br>
!>    These are assumed to be appended to the right hand sides and the
!>    initial guesses(if any) in the array rhs.
!>    @param[in] title character*71 title of matrix
!>    @param[in] key character*8 key of matrix
!>    @param[in] type charatcer*3 type of matrix.
!>    @param[in] ifmt integer specifying the format chosen for the real
!>    values to be output(i.e.,for a,and for rhs-guess-sol if applicable
!>    The meaning of ifmt is as follows.
!>    <ul><li> if(ifmt .lt. 100) then the E descriptor is used, format
!>    Ed.m,in which the length(m) of the mantissa is precisely the
!>    integer ifmt(and d = ifmt+6)</li>
!>    <li>if(ifmt .gt. 100) then prtmt will use the F- descriptor
!>    (format Fd.m) in which the length of the mantissa(m) is the intege
!>     mod(ifmt,100) and the length of the integer part is k=ifmt/100
!>    (and d = k+m+2). Thus ifmt= 4 means E10.4 +.xxxxD+ee while
!>    ifmt=104 means F7.4 +x.xxxx and ifmt=205 means F9.5 +xx.xxxxx</li>
!>     Note: formats for ja,and ia are internally computed.
!>    @param[in] job integer to indicate whether matrix values and a
!>    right hand side is available to be written:
!>    <ul><li>job = 1 write srtucture only,i.e.,the arrays ja and ia.</l
!>    <li>job = 2 write matrix including values,i.e.,a,ja,ia</li>
!>    <li>job = 3 write matrix and one right hand side: a,ja,ia,rhs.</li
!>    <li>job = nrhs+2 write matrix and nrhs successive right hand sides
!>    Note that there cannot be any right hand side if the matrix has no
!>    values. Also the initial guess and exact solutions when provided
!>    are for each right hand side. For example if nrhs=2 and guesol='GX
!>    there are 6 vectors to write.
!>    @param[in] iounit logical unit number where to write the matrix in
!>    @param[out] ierr integer used for error messages.
!>    ierr = 1 general i/o error
!
subroutine prtmt(nrow,ncol,a,ja,ia,rhs,guesol,title,key,type,&
&ifmt,job,iounit, ierr)
  use rd_kinds, only: lrk, wp
  implicit none
!
  character(len=72) :: title
  character(len=8) :: key
  character(len=3) :: type
  character(len=16) :: ptrfmt, indfmt
  character(len=20) :: valfmt
  character(len=2) :: guesol
  character(len=3) :: rhstyp
  integer :: totcrd, ptrcrd, indcrd, valcrd, rhscrd, nrow, ncol, nnz, nrhs, ilen, nperli, ifmt, job, iounit, ierr
  integer :: ja(*), ia(*)
  real(wp) :: a(*), rhs(*)
!
  character(len=1) :: c1
  integer :: i, iend, ihead, ifmtl, next, cols
  dimension c1(3)
  parameter (c1 = (/'X','G','F'/),cols = 80)
!
  intrinsic :: log10, min, int, real
!
  ifmtl = ifmt
!     compute pointer format
  nnz = ia(ncol+1)-1
  ilen = int(log10(0.1_lrk+real(nnz+1, wp)))+1
  nperli = cols/ilen
  ptrcrd = ncol/nperli+1
!
  if(ilen .gt. 9) then
    write(ptrfmt,101,err=9) nperli,ilen
  else
    write(ptrfmt,100,err=9) nperli,ilen
  end if
!     compute ROW index format
  ilen = int(log10(0.1_lrk+real(nrow, wp)))+1
  nperli = min(cols/ilen,nnz)
  indcrd = (nnz-1)/nperli+1
  write(indfmt,100,err=9) nperli,ilen
!     compute values and rhs format(using the same for both)
  valcrd = 0
  rhscrd = 0
!     quit this part if no values provided.
  if(job .le. 1) goto 20
!
  if(ifmtl .ge. 100) then
    ihead = ifmtl/100
    ifmtl = ifmtl-100*ihead
    ilen = ihead+ifmtl+2
    nperli = cols/ilen
    if(ilen .le. 9) then
      write(valfmt,102,err=9) nperli,ilen,ifmtl
    elseif(ifmtl .le. 9) then
      write(valfmt,103,err=9) nperli,ilen,ifmtl
    else
      write(valfmt,104,err=9) nperli,ilen,ifmtl
    end if
  else
    ilen = ifmtl+6
    nperli = cols/ilen
!       try to minimize the blanks in the format strings.
    if(nperli .le. 9) then
      if(ilen .le. 9) then
        write(valfmt,105,err=9) nperli,ilen,ifmtl
      elseif(ifmtl .le. 9) then
        write(valfmt,106,err=9) nperli,ilen,ifmtl
      else
        write(valfmt,107,err=9) nperli,ilen,ifmtl
      end if
    else
      if(ilen .le. 9) then
        write(valfmt,108,err=9) nperli,ilen,ifmtl
      elseif(ifmtl .le. 9) then
        write(valfmt,109,err=9) nperli,ilen,ifmtl
      else
        write(valfmt,110) nperli,ilen,ifmtl
      end if
    end if
  end if
  valcrd =(nnz-1)/nperli+1
  nrhs   = job-2
  if(nrhs .ge. 1) then
    i = (nrhs*nrow-1)/nperli+1
    rhscrd = i
    if(guesol(1:1) .eq. c1(2)) rhscrd = rhscrd+i
    if(guesol(2:2) .eq. c1(1)) rhscrd = rhscrd+i
    rhstyp = c1(3)//guesol
  end if
20 continue
!
  totcrd = ptrcrd+indcrd+valcrd+rhscrd
!     write 4-line or five line header
  write(iounit,10,err=9) title,key,totcrd,ptrcrd,indcrd,valcrd,&
  &rhscrd,type,nrow,ncol,nnz,nrhs,ptrfmt,indfmt,valfmt,valfmt
!
  if(nrhs .ge. 1) write(iounit,11,err=9) rhstyp,nrhs
!
  write(iounit,ptrfmt,err=9)(ia(i),i = 1,ncol+1)
  write(iounit,indfmt,err=9)(ja(i),i = 1,nnz)
  if(job .le. 1) return
  write(iounit,valfmt,err=9)(a(i),i = 1,nnz)
  if(job .le. 2) return
  ilen = nrow*nrhs
  next = 1
  iend = ilen
  write(iounit,valfmt,err=9)(rhs(i),i = next,iend)
!     write initial guesses if available
  if(guesol(1:1) .eq. c1(2)) then
    next = next+ilen
    iend = iend+ ilen
    write(iounit,valfmt,err=9)(rhs(i),i = next,iend)
  end if
!     write exact solutions if available
  if(guesol(2:2) .eq. c1(1))then
    next = next+ilen
    iend = iend+ilen
    write(iounit,valfmt,err=9)(rhs(i),i = next,iend)
  end if
!
  return
!     general io error see default error messages block data
9 ierr = 1
!
  return
!
10 format(a72,a8 / 5i14 / a3,11x,4i14 / 2a16,2a20)
11 format(A3,11x,i4)
100 format('(',i2,'I',i1,')')
101 format('(',i2,'I',i2,')')
102 format('(',i2,'F',i1,'.',i1,')')
103 format('(',i2,'F',i2,'.',i1,')')
104 format('(',i2,'F',i2,'.',i2,')')
105 format('(',i1,'E',i1,'.',i1,')')
106 format('(',i1,'E',i2,'.',i1,')')
107 format('(',i1,'E',i2,'.',i2,')')
108 format('(',i2,'E',i1,'.',i1,')')
109 format('(',i2,'E',i2,'.',i1,')')
110 format('(',i2,'E',i2,'.',i2,')')
!
end subroutine prtmt
!
!>    @brief DWRITEHB write a sparse double precision matrix to a file;
!>    Uses a Harwell/Boeing format.
!>    @param[in] title character*72 title of matrix test.
!>    @param[in] key character*8 key of matrix
!>    @param[in] iounit logical unit number where to write the matrix.
!>    @param[in] nrow number of rows in matrix
!>    @param[in] ncol number of columns in matrix
!>    @param[in] dns input nrow x ncol (dense) matrix.
!>    @param[in] ndns first dimension of dns.
!>    @param[out] ierr integer used for error messages see default
!>    error messages block data.
!
subroutine dwritehb(title,key,iounit,nrow,ncol,dns,ndns,ierr)
  use rd_kinds, only: wp
  implicit none
!
  integer :: iounit, nrow, ncol, ndns, ierr
  real(wp) :: dns(ndns,*)
  character(len=8) :: key
  character(len=72) :: title
!
!     dimensions
  integer :: mtg
  parameter (mtg = 500)
!     modal space globals
  integer :: vte
  parameter (vte = (2*mtg)**2+1)
!
  real(wp) :: a(vte), ao(vte)
  integer :: ia(vte), ja(vte), iao(vte), jao(vte)
!
  integer :: irows, dnnz
!     dummy arguments, job=0, just count
  real(wp) :: aa(1), rhs(1)
!
  character(len=1) :: blank
  character(len=2) :: guesol
  character(len=3) :: type, c3
  parameter (blank = ' ',c3 = 'RUA')
!
!     number of non zero elements on dns, job=0
  irows = dnnz(nrow,ncol,dns,ndns,ja,ia,aa,0)
!     should have at least one non zero element
  if (irows .eq. 0) then
    ierr = 12
    return
  end if
!     convert dense dns to sparse compressed row a, ja and ia vectors
  call dnscsr(nrow,ncol,irows,dns,ndns,a,ja,ia,ierr)
  if (ierr .ne. 0) then
!       ierr hold the line on dnscsr, convert to general case
    ierr = 11
    return
  end if
!     convert sparse compressed row to column
  call csrcsc(ndns,1,1,a,ja,ia,ao,jao,iao)
!
  type = c3
  guesol = blank
!     write matrix to file
  call prtmt(nrow,ncol,ao,jao,iao,rhs,guesol,title,key,type,&
  &8,2,iounit,ierr)
!
  return
!
end subroutine dwritehb
!
!     =================================================================
!>    @brief MM_COMMENT_WRITE writes a comment to a Matrix Market file;
!>    Comments may be written AFTER the header line and BEFORE the size
!>    line;<br>
!>    This routine will prepend a "%  " comment marker to the string,<br
!>    Licensing:This code is distributed under the GNU LGPL license,<br>
!>    Modified: 03 January 2007.
!>    @author John Burkardt
!
!>    @param[in] iounit output unit identifier number.
!>    @param[in] comment comment to be written.
!>    @param[out] ierr error code, 0 means OK see default error
!>    messages block data
!
subroutine mm_comment_write(iounit,comment, ierr)
!
  implicit none
!
  integer :: iounit, ierr
  character(len=*) :: comment
!
  integer :: i
  character(len=1) :: cp1
  character(len=3) :: cp3
  parameter (cp1 = '%',cp3 = '%  ')
  character(len=80) :: lc
!     intrinsic functions
  intrinsic :: len_trim
!
  ierr = 0
  i = len_trim(comment)
  if(i .eq. 0) then
    write(iounit,5,err=100) cp1
  else
!       copy due g77 char issue.
    lc = comment
    write(iounit,15,err=100) cp3,lc(1:i)
  end if
!
  return
!
5 format(a)
15 format(2a)
!
100 ierr = 2
!
  return
!
end subroutine mm_comment_write
!
!     =================================================================
!>    @brief MM_FILE_WRITE writes data to a Matrix Market file;
!>    The data may be either sparse coordinate format,or dense array
!>    format; The unit iounit must be open,<br>
!>    Original FORTRAN77 version by Karin A Remington,NIST ACMD
!>    Licensing:This code is distributed under the GNU LGPL license,<br>
!>    Modified: 03 January 2007.
!>    @author John Burkardt
!
!>    @param[in] iounit output unit identifier number.
!>    @param[in] id Matrix Market identifier.This value must be
!>     "%%MatrixMarket".
!>    @param[in] type Matrix Market type. This value must be "matrix".
!>    @param[in] rep Matrix Market "representation" indicator.
!>    Possible values include:
!>    <ul><li>coordinate (for sparse data)</li>
!>    <li>array (for dense data)</li>
!>    <li>elemental (to be added)</li></ul>
!>    @param[in] field Matrix Market "field". Possible values include:
!>    <ul><li>real</li>
!>    <li>double</li>
!>    <li>complex</li>
!>    <li>integer</li>
!>    <li>pattern(for "rep" = "coordinate" only)</li></ul>
!>    @param[in] symm Matrix Market "symmetry". Possible values include:
!>    <ul><li>symmetric</li>
!>    <li>hermitian</li>
!>    <li>skew-symmetric</li>
!>    <li>general</li></ul>
!>    @param[in] comment comment line
!>    @param[in] nrow number of rows in the matrix.
!>    @param[in] ncol number of columns in the matrix.
!>    @param[in] nnz number of nonzero entries required to store the
!>    matrix, if "rep" = "coordinate".
!>    @param[in] indx row indices for coordinate format. Not used if
!>    "rep" = "array".
!>    @param[out] jndx column indices for coordinate format. Not used
!>    if "rep" = "array".
!>    @param[in] ival values, if "field" is "integer".
!>    @param[in] rval values ,if "filed" is "real".
!>    @param[in] dval values,if "field" is "double".
!>    @param[in] cval values,if "field" is "complex".
!>    @param[out] ierr error code, 0 means OK see block data.
!
subroutine mm_file_write(iounit,id,type,rep,field,&
&symm,comment,nrow,ncol,nnz,indx,jndx,ival,rval,dval,cval,ierr)
  use rd_kinds, only: lrk, wp
!
  implicit none
!
  integer :: ierr
  complex(lrk) :: cval(*)
  real(wp) :: dval(*)
  integer :: indx(*), ival(*), jndx(*)
  integer :: ncol, nnz, nnz2, nrow, iounit
  real(lrk) :: rval(*)
  character(len=*) :: field, rep, symm, comment
  character(len=6) :: type
  character(len=10) :: c10
  character(len=14) :: id
  parameter (c10 = 'coordinate')
  logical :: s_eqi, mm_header_check, h
!
!     Test the header values.
  h = mm_header_check(id,type,rep,field,symm,ierr)
!     check for errors
  if (.not. h) return
!     Write the header line.
  call mm_header_write(iounit,id,type,rep,field,symm,ierr)
!     check for errors
  if (ierr .gt. 0) return
!     Write a comment line.
  call mm_comment_write(iounit,comment,ierr)
!     check for errors
  if (ierr .gt. 0) return
!     Write the size line.
  call mm_size_write(iounit,rep,nrow,ncol,nnz,ierr)
!     check for errors
  if (ierr .gt. 0) return
!     Determine NNZ where necessary.
  call mm_nnz_set(rep,symm,nrow,ncol,nnz2)
!     Write the data.
  if(s_eqi(rep,c10)) then
    call mm_values_write(iounit,rep,field,nnz,indx,jndx,&
    &ival,rval,dval,cval,ierr)
  else
    call mm_values_write(iounit,rep,field,nnz2,indx,&
    &jndx,ival,rval,dval,cval,ierr)
  end if
!
  return
!
end subroutine mm_file_write
!
!     =================================================================
!>    @brief DWRITEMM write a sparse double precision matrix to a file,
!>    Uses a matrix market format.
!>    @param[in] title of matrix.
!>    @param[in] iounit logical unit number where to write the matrix.
!>    @param[in] nrow number of rows in matrix
!>    @param[in] ncol number of columns in matrix
!>    @param[in] dns input nrow x ncol (dense) matrix.
!>    @param[in] ndns first dimension of dns.
!>    @param[out] ierr integer used for error messages
!
subroutine dwritemm(title,iounit,nrow,ncol,dns,ndns,ierr)
  use rd_kinds, only: lrk, wp
  implicit none
!
  integer :: iounit, nrow, ncol, ndns, ierr
  real(wp) :: dns(ndns,*)
  character(len=72) :: title
!
!     dimensions
  integer :: mtg
  parameter (mtg = 500)
!     global modal spacE
  integer :: vte
  parameter (vte = (2*mtg)**2+1)
!
  integer :: dnnz, irows
  real(wp) :: a(vte)
  integer :: ia(vte), ja(vte)
!     dummy arguments
  integer :: ival(1)
  real(lrk) :: rval(1)
  complex(lrk) :: cval(1)
  character(len=14) :: id
  character(len=6) :: typm
  character(len=10) :: rep
  character(len=7) :: field
  character(len=19) :: symm
!
  parameter (&
  &id = '%%MatrixMarket'&
  &,typm = 'matrix'&
  &,rep = 'coordinate'&
  &,field = 'double'&
  &,symm = 'general')
!
!     number of non zero elements n dns, coordinates ja, ia and values a
!     please note ja column ans ia row coordinates
  irows = dnnz(nrow,ncol,dns,ndns,ja,ia,a,1)
!
  call mm_file_write(iounit,id,typm,rep,field,symm,title,nrow,&
  &ncol,irows,ia,ja,ival,rval,a,cval,ierr)
!
  return
!
end subroutine dwritemm
!
!     =================================================================
!>    @brief CH_CAP capitalizes a single character;
!>    Converts a row-stored sparse matrix into a densely stored one,<br>
!>    Licensing:This code is distributed under the GNU LGPL license,<br>
!>    Modified: 03 January 2007.
!>    @author John Burkardt
!
!>    @param[in,out] ch character to capitalize.
!
subroutine ch_cap(ch)
!
  implicit none
!
  character :: ch
  integer :: itemp
!
  intrinsic :: char, ichar
!
  itemp = ichar(ch)
!
  if(97 .le. itemp .and. itemp .le. 122) then
    ch = char(itemp-32)
  end if
!
  return
!
end subroutine ch_cap
!
!     =================================================================
!>    @brief S_EQI is a case insensitive comparison of two strings for
!.    equality; Example: S_EQI('Anjana','ANJANA') is TRUE,<br>
!>    Licensing:This code is distributed under the GNU LGPL license;<br>
!>    Modified: 03 January 2007.
!>    @author John Burkardt
!
!>    @param[in] s1 string to compare.
!>    @param[in] s2 string to compare.
!>    @return result of the comparison, TRUE if strings match.
!
logical function s_eqi(s1,s2)
!
  implicit none
!
  character :: c1, c2, blank
  integer :: i, lenc, s1_length, s2_length
  character(len=*) :: s1, s2
  parameter (blank = ' ')
!
  intrinsic :: len, min
!
  s1_length = len(s1)
  s2_length = len(s2)
  lenc = min(s1_length,s2_length)
!
  s_eqi = .false.
!
  do i = 1,lenc
    c1 = s1(i:i)
    c2 = s2(i:i)
    call ch_cap(c1)
    call ch_cap(c2)
    if(c1 .ne. c2) then
      return
    end if
  end do
!
  do i = lenc+1,s1_length
    if(s1(i:i) .ne. blank) then
      return
    end if
  end do
!
  do i = lenc+1,s2_length
    if(s2(i:i) .ne. blank) then
      return
    end if
  end do
!
  s_eqi = .true.
!
  return
!
end function s_eqi
!
!     =================================================================
!>    @brief S_NEQI compares two strings for non-equality,ignoring
!>    case;<br>
!>    Licensing:This code is distributed under the GNU LGPL license;<br>
!>    Modified: 03 January 2007.
!>    @author John Burkardt
!
!>    @param[in] s1 string to compare.
!>    @param[in] s2 string to compare.
!>    @return result of the comparison, TRUE if strings NOT match.
!
logical function s_neqi(s1,s2)
!
  implicit none

  character :: c1, c2, blank
  integer :: i, len1, len2, lenc
  character(len=*) :: s1, s2
  parameter (blank = ' ')
!
  intrinsic :: len, min
!
  len1 = len(s1)
  len2 = len(s2)
  lenc = min(len1,len2)

  s_neqi = .true.

  do i = 1,lenc
    c1 = s1(i:i)
    c2 = s2(i:i)
    call ch_cap(c1)
    call ch_cap(c2)
    if(c1 .ne. c2) then
      return
    end if
  end do
!
  do i = lenc+1,len1
    if(s1(i:i) .ne. blank) then
      return
    end if
  end do
!
  do i = lenc+1,len2
    if(s2(i:i) .ne. blank) then
      return
    end if
  end do
!
  s_neqi = .false.
!
  return
!
end function s_neqi
!
!     =================================================================
!>    @brief MM_HEADER_CHECK checks the header strings for a Matrix
!>    Market file;<br>
!>    Licensing:This code is distributed under the GNU LGPL license;<br>
!>    Modified: 03 January 2007.
!>    @author John Burkardt
!
!>    @param[in] id Matrix Market identifier.This value must be
!>     "%%MatrixMarket".
!>    @param[in] type Matrix Market type. This value must be "matrix".
!>    @param[in] rep Matrix Market "representation" indicator.
!>    Possible values include:
!>    <ul><li>coordinate (for sparse data)</li>
!>    <li>array (for dense data)</li>
!>    <li>elemental  (to be added)</li></ul>
!>    @param[in] field Matrix Market "field". Possible values include:
!>    <ul><li>real</li>
!>    <li>double</li>
!>    <li>complex</li>
!>    <li>integer</li>
!>    <li>pattern(for "rep" = "coordinate" only)</li></ul>
!>    @param[in] symm Matrix Market "symmetry". Possible values include:
!>    <ul><li>symmetric</li>
!>    <li>hermitian</li>
!>    <li>skew-symmetric</li>
!>    <li>general</li></ul>
!>    @param[out] ierr error code, 0 means OK see block data.
!>    @return TRUE if the header is OK.
!
logical function mm_header_check(id,type,rep,field,symm,ierr)
!
  implicit none
!
  integer :: ierr
  character(len=*) :: field, rep, symm
  character(len=14) :: id
  character(len=4) :: cb5
  character(len=5) :: cb2
  character(len=6) :: type, cb3
  character(len=7) :: cb4
  character(len=9) :: cb6
  character(len=10) :: cb1
  character(len=14) :: cb7
  dimension cb3(2),cb4(4),cb6(2),cb7(2)
  parameter (cb1 = 'coordinate',cb2 = 'array',&
  &cb3 = (/'matrix','double'/),&
  &cb4 = (/'integer','complex','pattern','general'/),&
  &cb5 = 'real' ,cb6 = (/'symmetric','hermitian'/),&
  &cb7 = (/'skew-symmetric','%%MatrixMarket'/))
  logical :: s_eqi, s_neqi
!
  mm_header_check = .false.
!     Test the input qualifiers.
  if(s_neqi(id,cb7(2))) then
    ierr = 3
    return
  end if
!
  if(s_neqi(type,cb3(1))) then
    ierr = 4
    return
  end if
!
  if(s_neqi(rep,cb1) .and.&
  &s_neqi(rep,cb2)) then
    ierr = 5
    return
  end if
!
  if(s_eqi(rep,cb1)) then
    if(&
    &s_neqi(field,cb4(1)) .and.&
    &s_neqi(field,cb5) .and.&
    &s_neqi(field,cb3(2)) .and.&
    &s_neqi(field,cb4(2)) .and.&
    &s_neqi(field,cb4(3))) then
      ierr = 6
      return
    end if
!
  else if(s_eqi(rep,cb2)) then
    if(s_neqi(field,cb4(1)) .and.&
    &s_neqi(field,cb5) .and.&
    &s_neqi(field,cb3(2)) .and.&
    &s_neqi(field,cb4(2))) then
      ierr = 7
      return
    end if
  end if
!
  if(s_neqi(symm,cb4(4)) .and.&
  &s_neqi(symm,cb6(1)) .and.&
  &s_neqi(symm,cb6(2)) .and.&
  &s_neqi(symm,cb7(1))) then
    ierr = 7
    return
  end if
!
  mm_header_check = .true.
!
  return
!
end function mm_header_check
!
!     =================================================================
!>    @brief MM_HEADER_WRITE prints header information to a Matrix
!>    Market file;<br>
!>    Licensing:This code is distributed under the GNU LGPL license;<br>
!>    Modified: 03 January 2007.
!>    @author John Burkardt
!
!>    @param[in] iounit output unit identifier number.
!>    @param[in] id Matrix Market identifier.This value must be
!>     "%%MatrixMarket".
!>    @param[in] type Matrix Market type. This value must be "matrix".
!>    @param[in] rep Matrix Market "representation" indicator.
!>    Possible values include:
!>    <ul><li>coordinate (for sparse data)</li>
!>    <li>array (for dense data)</li>
!>    <li>elemental  (to be added)</li></ul>
!>    @param[in] field Matrix Market "field". Possible values include:
!>    <ul><li>real</li>
!>    <li>double</li>
!>    <li>complex</li>
!>    <li>integer</li>
!>    <li>pattern(for "rep" = "coordinate" only)</li></ul>
!>    @param[in] symm Matrix Market "symmetry". Possible values include:
!>    <ul><li>symmetric</li>
!>    <li>hermitian</li>
!>    <li>skew-symmetric</li>
!>    <li>general</li></ul>
!>    @param[out] ierr error code, 0 means OK see block data.
!
subroutine mm_header_write(iounit,id,type,rep,field,symm,ierr)
  implicit none
!
  integer :: iounit, ierr
  character(len=6) :: type
  character(len=7) :: field
  character(len=10) :: rep
  character(len=14) :: id
  character(len=19) :: symm
!     local
  integer :: i, j, k, l, m
!     intrinsic
  intrinsic :: len_trim
!
  i = len_trim(id)
  j = len_trim(type)
  k = len_trim(rep)
  l = len_trim(field)
  m = len_trim(symm)
  write(iounit,5,err=100)&
  &id(1:i),type(1:j),rep(1:k),field(1:l),symm(1:m)
!
  return
!
100 ierr = 8
!
  return
!
5 format(a,1x,a,1x,a,1x,a,1x,a)
!
end subroutine mm_header_write
!
!     =================================================================
!>    @brief MM_NNZ_SET sets the value of NNZ for the ARRAY
!>    representation;<br>
!>    If the representation is not "ARRAY",then NNZ is returned as 0;<br
!>    Licensing:This code is distributed under the GNU LGPL license;<br>
!>    Modified: 03 January 2007.
!>    @author John Burkardt
!
!>    @param[in] rep Matrix Market "representation" indicator.
!>    Possible values include:
!>    <ul><li>coordinate (for sparse data)</li>
!>    <li>array (for dense data)</li>
!>    <li>elemental  (to be added)</li></ul>
!>    @param[in] symm Matrix Market "symmetry". Possible values include:
!>    <ul><li>symmetric</li>
!>    <li>hermitian</li>
!>    <li>skew-symmetric</li>
!>    <li>general</li></ul>
!>    @param[in] nrow number of rows in the matrix.
!>    @param[in] ncol number of columns in the matrix.
!>    @param[in] nnz number of nonzero entries required to store the
!>    matrix, if "rep" = "coordinate".
!
subroutine mm_nnz_set(rep,symm,nrow,ncol,nnz)
  implicit none
!
  integer :: ncol, nrow, nnz
  character(len=5) :: c5
  character(len=7) :: c7
  character(len=9) :: c9
  character(len=10) :: rep, c10
  character(len=14) :: c14
  character(len=19) :: symm
  logical :: s_eqi
  dimension c9(2)
  parameter(c5 = 'array',c7 = 'general',&
  &c9 = (/'symmetric','hermitian'/),c10 = 'coordinate',&
  &c14 = 'skew-symmetric')
!
  nnz = 0
!
  if(s_eqi(rep,c10)) then
  else if(s_eqi(rep,c5)) then
    if(s_eqi(symm,c7)) then
      nnz = nrow*ncol
    else if(s_eqi(symm,c9(1)) .or. s_eqi(symm,c9(2))) then
      nnz =(nrow*ncol-nrow)/2+nrow
    else if(s_eqi(symm,c14)) then
      nnz =(nrow*ncol-nrow)/2
    end if
  end if
!
  return
!
end subroutine mm_nnz_set
!
!     =================================================================
!>    @brief MM_SIZE_WRITE writes size information to a Matrix Market
!>    file;<br>
!>    Licensing:This code is distributed under the GNU LGPL license;<br>
!>    Modified: 03 January 2007.
!>    @author John Burkardt
!
!>    @param[in] iounit output unit identifier number.
!>    @param[in] rep Matrix Market "representation" indicator.
!>    Possible values include:
!>    <ul><li>coordinate (for sparse data)</li>
!>    <li>array (for dense data)</li>
!>    <li>elemental  (to be added)</li></ul>
!>    @param[in] nrow number of rows in the matrix.
!>    @param[in] ncol number of columns in the matrix.
!>    @param[in] nnz number of nonzero entries required to store the
!>    matrix, if "rep" = "coordinate".
!>    @param[out] ierr error code, 0 means OK see block data.
!
subroutine mm_size_write(iounit,rep,nrow,ncol,nnz,ierr)
  implicit none
!
  integer :: ncol, nnz, nrow, iounit, ierr
  character(len=5) :: c5
  character(len=10) :: rep, c10
  logical :: s_eqi
  parameter(c5 = 'array',c10 = 'coordinate')
!
  if(s_eqi(rep,c10)) then
    write(iounit,*,err=100) nrow,ncol,nnz
  else if(s_eqi(rep,c5)) then
    write(iounit,*,err=100) nrow,ncol
  end if
!
  return
!
100 ierr = 9
!
  return
!
end subroutine mm_size_write
!
!     =================================================================
!>    @brief MM_VALUES_WRITE writes matrix values to a Matrix Market
!>    file;<br>
!>    Licensing:This code is distributed under the GNU LGPL license;<br>
!>    Modified: 03 January 2007.
!>    @author John Burkardt
!
!>    @param[in] iounit output unit identifier number.
!>    @param[in] rep Matrix Market "representation" indicator.
!>    Possible values include:
!>    <ul><li>coordinate (for sparse data)</li>
!>    <li>array (for dense data)</li>
!>    <li>elemental  (to be added)</li></ul>
!>    @param[in] field Matrix Market "field". Possible values include:
!>    <ul><li>real</li>
!>    <li>double</li>
!>    <li>complex</li>
!>    <li>integer</li>
!>    <li>pattern(for "rep" = "coordinate" only)</li></ul>
!>    @param[in] nnz number of nonzero entries required to store the
!>    matrix, if "rep" = "coordinate".
!>    @param[in] indx row indices for coordinate format. Not used if
!>    "rep" = "array".
!>    @param[in] jndx column indices for coordinate format. Not used
!>    if "rep" = "array".
!>    @param[in] ival values, if "field" is "integer".
!>    @param[in] rval values ,if "filed" is "real".
!>    @param[out] dval values,if "field" is "double".
!>    @param[in] cval values,if "field" is "complex".
!>    @param[out] ierr error code, 0 means OK see block data.
!
subroutine mm_values_write(iounit,rep,field,nnz,indx,&
&jndx,ival,rval,dval,cval,ierr)
  use rd_kinds, only: lrk, wp
  implicit none
!
  integer :: nnz, ierr
!
  complex(lrk) :: cval(nnz)
  real(wp) :: dval(nnz)
  integer :: i, iounit
  integer :: indx(nnz), ival(nnz), jndx(nnz)
  real(lrk) :: rval(nnz)
  character(len=4) :: c4
  character(len=5) :: c5
  character(len=6) :: c6
  character(len=7) :: field, c7
  character(len=10) :: rep, c10
  dimension c7(3)
  parameter(c4 = 'real',c5 = 'array',c6 ='double',&
  &c7 = (/'integer','complex','pattern'/),c10 = 'coordinate')
  logical :: s_eqi
!
  if(s_eqi(rep,c10)) then
    if(s_eqi(field,c7(1))) then
      do i = 1,nnz
        write(iounit,*,err=100) indx(i),jndx(i),ival(i)
      end do
    else if(s_eqi(field,c4)) then
      do i = 1,nnz
        write(iounit,*,err=100) indx(i),jndx(i),rval(i)
      end do
    else if(s_eqi(field,c6)) then
      do i = 1,nnz
        write(iounit,*,err=100) indx(i),jndx(i),dval(i)
      end do
    else if(s_eqi(field,c7(2))) then
      do i = 1,nnz
        write(iounit,*,err=100) indx(i),jndx(i),cval(i)
      end do
    else if(s_eqi(field,c7(3))) then
      do i = 1,nnz
        write(iounit,*,err=100) indx(i),jndx(i)
      end do
    end if
  else if(s_eqi(rep,c5)) then
    if(s_eqi(field,c7(1))) then
      do i = 1,nnz
        write(iounit,*,err=100) ival(i)
      end do
    else if(s_eqi(field,c4)) then
      do i = 1,nnz
        write(iounit,*,err=100) rval(i)
      end do
    else if(s_eqi(field,c6)) then
      do i = 1,nnz
        write(iounit,*,err=100) dval(i)
      end do
    else if(s_eqi(field,c7(2))) then
      do i = 1,nnz
        write(iounit,*,err=100) cval(i)
      end do
    end if
  end if
!
  return
!
100 ierr = 10
  return
!
end subroutine mm_values_write
!
!     =================================================================
!>    @brief get speed to export modal parameters matrices (rpm).
!>     First option is field rated speed next,
!>     the mode calculation speed, at least, mean Campbell.
!>    @return speed to export modal parameters matrices (rpm).
!
real(lrk) function expspdf()
  use com_cpb, only: nini, nfin, dw, npi, ncc, imt
  use com_cpbn, only: spdn
  use com_mfa, only: ma, rm, au
  use rd_kinds, only: lrk
  implicit none
!
  real(lrk) :: exs
!
!     nominal speed (rpm), added francisco oct-20
!
!     min amplification factor fator, modal rpm, angle unit block
!     angle unit r -> radians, default degree
!
  exs = 0
!     check rated speed
  if (spdn .le. 0) then
!       check modes speed
    if (rm .le. 0) then
!         mean Campbell
      exs = nini+(nfin-nini)/2
    else
!         mode speed
      exs = rm
    end if
  else
!       rated speed
    exs = spdn
  end if
!
  expspdf = exs
!
  return
!
end function expspdf
!
!     =================================================================
!>    @brief write plain square matrix.
!
!>    @param[in] title of matrix.
!>    @param[in] iu logical unit number where to write the matrix.
!>    @param[in] ddm number of rows and columns in given input matrix
!>    @param[in] a matrix to export
!>    @param[in] mte dimension of matrix to export.
!>    @param[out] ierr integer used for error messages
!
subroutine dwritepl(title,iu,ddm,a,mte,ierr)
  use rd_kinds, only: wp
  implicit none
!
  character(len=*) :: title
  integer :: iu, ddm, mte, ierr
  real(wp) :: a
  dimension a(mte,mte)
!
  character(len=1) :: c1
  character(len=8) :: cf1
  dimension c1(2)
  parameter (c1 = (/'(',')'/),cf1 = '(e20.12)')
  character(len=14) :: cf2
  integer :: i, j
!
!     title
  write(iu,25,err=10) title
!     format
  write(cf2,15,err=10) c1(1),ddm,cf1,c1(2)
!
  do i = 1,ddm
    write(iu,cf2,err=10) (a(i,j),j = 1,ddm)
  end do
!
  return
!
!     matrix error message code
!     'dwritepl:general i/o error' !13
10 ierr = 13
!
  return
!
15 format(a,i4,2a)
25 format(a)
!
end subroutine dwritepl
!
