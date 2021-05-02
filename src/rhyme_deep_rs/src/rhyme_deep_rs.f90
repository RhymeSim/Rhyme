module rhyme_deep_rs
   use rhyme_logger

   implicit none

   type, private :: indices_t
      integer :: idx = 1
   end type indices_t

   type(indices_t), parameter :: drsid = indices_t()

   type deep_rs_t
   contains
      procedure :: rhyme_deep_rs_write_formatted
      generic :: write (formatted) => rhyme_deep_rs_write_formatted
   end type deep_rs_t

   interface
      module subroutine rhyme_deep_rs_init(drs, logger)
         type(deep_rs_t), intent(inout) :: drs
         type(logger_t), intent(inout) :: logger
      end subroutine rhyme_deep_rs_init
   end interface

contains
   subroutine rhyme_deep_rs_write_formatted( &
      this, unit, iotype, v_list, iostat, iomsg)
      implicit none

      class(deep_rs_t), intent(in) :: this
      integer, intent(in) :: unit
      character(len=*), intent(in) :: iotype
      integer, intent(in) :: v_list(:)
      integer, intent(out) :: iostat
      character(len=*), intent(inout) :: iomsg

      write (unit, fmt='(A,A,A,A,A,I0,A)', iostat=iostat, iomsg=iomsg) &
         '<deep_rs_t', &
         ' iotype="', trim(iotype), '"', &
         ' v_list=', size(v_list), &
         '>'
   end subroutine rhyme_deep_rs_write_formatted
end module rhyme_deep_rs
