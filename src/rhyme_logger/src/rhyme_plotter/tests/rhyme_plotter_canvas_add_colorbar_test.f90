logical function rhyme_plotter_canvas_add_colorbar_test() result(failed)
   use rhyme_plotter

   implicit none

   integer :: res = 72

   type(plotter_canvas_t) :: canvas

   call canvas%init(res, res/3)

   call canvas%add_axis( &
      plid%bottom, 7, [epsilon(0d0), 3d0], &
      scale=plid%linear, label='X', color=tc%blue)

   call canvas%add_axis( &
      plid%left, 7, [epsilon(0d0), 3d0], &
      scale=plid%linear, label='Y', color=tc%blue)

   call canvas%add_corner(plid%bottom, plid%left)

   call canvas%add_colorbar( &
      colorschemes(csid%magma_grey), -3d0, 3d0, &
      plid%linear, plid%right)

   call canvas%plot
   call canvas%clear

   call canvas%add_axis( &
      plid%bottom, 7, [epsilon(0d0), 3d0], &
      scale=plid%linear, label='X', color=tc%blue)

   call canvas%add_axis( &
      plid%right, 7, [epsilon(0d0), 3d0], &
      scale=plid%linear, label='Y', color=tc%blue)

   call canvas%add_corner(plid%bottom, plid%right)

   call canvas%add_colorbar( &
      colorschemes(csid%magma_grey), 1d1, 1d5, &
      plid%log, plid%left)

   call canvas%plot
   call canvas%clear

! To see the output set failed to .true.
   failed = .false.
end function rhyme_plotter_canvas_add_colorbar_test
