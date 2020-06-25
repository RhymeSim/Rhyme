module rhyme_tiling_drawing
   use rhyme_logger

   implicit none

   type, private :: indices_t
      integer :: idx = 1
   end type indices_t

   type(indices_t), parameter :: tdrawid = indices_t()

   type tiling_drawing_t
   contains
      procedure :: rhyme_tiling_drawing_write_formatted
      generic :: write (formatted) => rhyme_tiling_drawing_write_formatted
   end type tiling_drawing_t

   interface
      module subroutine rhyme_tiling_drawing_init(draw, logger)
         type(tiling_drawing_t), intent(inout) :: draw
         type(logger_t), intent(inout) :: logger
      end subroutine rhyme_tiling_drawing_init
   end interface

contains
   subroutine rhyme_tiling_drawing_write_formatted( &
      this, unit, iotype, v_list, iostat, iomsg)
      implicit none

      class(tiling_drawing_t), intent(in) :: this
      integer, intent(in) :: unit
      character(len=*), intent(in) :: iotype
      integer, intent(in) :: v_list(:)
      integer, intent(out) :: iostat
      character(len=*), intent(inout) :: iomsg

      write (unit, fmt='(A,A,A,A,A,I0,A)', iostat=iostat, iomsg=iomsg) &
         '<tiling_drawing_t', &
         ' iotype="', trim(iotype), '"', &
         ' v_list=', size(v_list), &
         '>'
   end subroutine rhyme_tiling_drawing_write_formatted
end module rhyme_tiling_drawing
