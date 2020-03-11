module rhyme_xyz
   use rhyme_logger

   implicit none

   type, private :: indices_t
      integer :: idx = 1
   end type indices_t

   type(indices_t), parameter :: xxxid = indices_t()

   type xyz_t
   contains
      procedure :: rhyme_xyz_write_formatted
      generic :: write (formatted) => rhyme_xyz_write_formatted
   end type xyz_t

   interface
      module subroutine rhyme_xyz_init(xxx, logger)
         type(xyz_t), intent(inout) :: xxx
         type(logger_t), intent(inout) :: logger
      end subroutine rhyme_xyz_init

      pure module function rhyme_xyz_equality(xxx1, xxx2) result(eq)
         type(xyz_t), intent(in) :: xxx1, xxx2
         logical :: eq
      end function rhyme_xyz_equality
   end interface

   interface operator(==)
      module procedure rhyme_xyz_equality
   end interface operator(==)

contains
   subroutine rhyme_xyz_write_formatted( &
      this, unit, iotype, v_list, iostat, iomsg)
      implicit none

      class(xyz_t), intent(in) :: this
      integer, intent(in) :: unit
      character(len=*), intent(in) :: iotype
      integer, intent(in) :: v_list(:)
      integer, intent(out) :: iostat
      character(len=*), intent(inout) :: iomsg

      write (unit, fmt='(A,A,A,A,A,I0,A)', iostat=iostat, iomsg=iomsg) &
         '<xyz_t', &
         ' iotype="', trim(iotype), '"', &
         ' v_list=', size(v_list), &
         '>'
   end subroutine rhyme_xyz_write_formatted
end module rhyme_xyz
