!> @file rd_sys.f90
!> @brief Operating-system services without compiler extensions.
module rd_sys
  use, intrinsic :: iso_c_binding, only: c_char, c_null_char, c_ptr, c_size_t, c_associated
  implicit none
  private
  public :: rd_getcwd

  interface
    function c_getcwd(buf, size) bind(c, name='getcwd') result(p)
      import :: c_char, c_ptr, c_size_t
      character(kind=c_char), intent(out) :: buf(*)
      integer(c_size_t), value :: size
      type(c_ptr) :: p
    end function c_getcwd
  end interface

contains

  !> current working directory (replaces the GNU extension GETCWD)
  subroutine rd_getcwd(path)
    character(len=*), intent(out) :: path
    character(kind=c_char) :: buf(4097)
    integer :: i
    path = ' '
    if (.not. c_associated(c_getcwd(buf, int(size(buf), c_size_t)))) return
    do i = 1, min(len(path), size(buf))
      if (buf(i) == c_null_char) exit
      path(i:i) = buf(i)
    end do
  end subroutine rd_getcwd

end module rd_sys
