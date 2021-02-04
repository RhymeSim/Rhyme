submodule(rhyme_plotter) canvas_init_smod
contains
   module subroutine rhyme_plotter_canvas_init(canvas, x, y)
      implicit none

      class(plotter_canvas_t), intent(inout) :: canvas
      integer, intent(in) :: x, y

      canvas%x = x
      canvas%y = y

      canvas%lbound_x = 1 - offset_x - canvas_border
      canvas%ubound_x = x + offset_x + canvas_border
      canvas%lbound_y = 1 - offset_y - canvas_border
      canvas%ubound_y = y + offset_y + canvas_border

      ! default values
      canvas%axes%is_on = .false.
      canvas%axes%scale = plid%linear

      if (allocated(canvas%grid)) deallocate (canvas%grid)

      allocate ( &
         canvas%grid( &
         canvas%lbound_x:canvas%ubound_x, &
         canvas%lbound_y:canvas%ubound_y, &
         2 &
         ))

      call canvas%clear

      call rhyme_color_init()
   end subroutine rhyme_plotter_canvas_init
end submodule canvas_init_smod
