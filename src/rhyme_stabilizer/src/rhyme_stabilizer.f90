module rhyme_stabilizer
   use rhyme_logger

   implicit none

   type, private :: indices_t
      integer :: idx = 1
   end type indices_t

   type(indices_t), parameter :: stid = indices_t()

   type stabilizer_t
   contains
      procedure :: rhyme_stabilizer_write_formatted
      generic :: write (formatted) => rhyme_stabilizer_write_formatted
   end type stabilizer_t

   interface
      module subroutine rhyme_stabilizer_init(st, logger)
         type(stabilizer_t), intent(inout) :: st
         type(logger_t), intent(inout) :: logger
      end subroutine rhyme_stabilizer_init
   end interface

contains
   subroutine rhyme_stabilizer_write_formatted( &
      this, unit, iotype, v_list, iostat, iomsg)
      implicit none

      class(stabilizer_t), intent(in) :: this
      integer, intent(in) :: unit
      character(len=*), intent(in) :: iotype
      integer, intent(in) :: v_list(:)
      integer, intent(out) :: iostat
      character(len=*), intent(inout) :: iomsg

      write (unit, fmt='(A,A,A,A,A,I0,A)', iostat=iostat, iomsg=iomsg) &
         '<stabilizer_t', &
         ' iotype="', trim(iotype), '"', &
         ' v_list=', size(v_list), &
         '>'
   end subroutine rhyme_stabilizer_write_formatted
end module rhyme_stabilizer
