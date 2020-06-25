submodule(rhyme_tiling_drawing) init_smod
contains
module subroutine rhyme_tiling_drawing_init(draw, logger)
   implicit none

   type(tiling_drawing_t), intent(inout) :: draw
   type(logger_t), intent(inout) :: logger

   character(len=128) :: draw_str

   call logger%begin_section('tiling_drawing')

   write (draw_str, *) draw
   call logger%log('', 'tiling_drawing', '=', [draw_str])

   call logger%end_section
end subroutine rhyme_tiling_drawing_init
end submodule init_smod
