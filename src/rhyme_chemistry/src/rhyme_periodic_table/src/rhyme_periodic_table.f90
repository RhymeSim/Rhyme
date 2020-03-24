module rhyme_periodic_table
   use rhyme_logger

   implicit none

   type, private :: indices_t
      integer :: idx = 1
   end type indices_t

   type(indices_t), parameter :: ptid = indices_t()

   type periodic_table_t
   contains
      procedure :: rhyme_periodic_table_write_formatted
      generic :: write (formatted) => rhyme_periodic_table_write_formatted
   end type periodic_table_t

   interface
      module subroutine rhyme_periodic_table_init(pt, logger)
         type(periodic_table_t), intent(inout) :: pt
         type(logger_t), intent(inout) :: logger
      end subroutine rhyme_periodic_table_init

      pure module function rhyme_periodic_table_equality(pt1, pt2) result(eq)
         type(periodic_table_t), intent(in) :: pt1, pt2
         logical :: eq
      end function rhyme_periodic_table_equality
   end interface

   interface operator(==)
      module procedure rhyme_periodic_table_equality
   end interface operator(==)

contains
   subroutine rhyme_periodic_table_write_formatted( &
      this, unit, iotype, v_list, iostat, iomsg)
      implicit none

      class(periodic_table_t), intent(in) :: this
      integer, intent(in) :: unit
      character(len=*), intent(in) :: iotype
      integer, intent(in) :: v_list(:)
      integer, intent(out) :: iostat
      character(len=*), intent(inout) :: iomsg

      write (unit, fmt='(A,A,A,A,A,I0,A)', iostat=iostat, iomsg=iomsg) &
         '<periodic_table_t', &
         ' iotype="', trim(iotype), '"', &
         ' v_list=', size(v_list), &
         '>'
   end subroutine rhyme_periodic_table_write_formatted
end module rhyme_periodic_table
