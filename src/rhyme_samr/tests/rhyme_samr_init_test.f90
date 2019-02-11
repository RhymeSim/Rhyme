logical function rhyme_samr_init_test () result (failed)
  use rhyme_samr_factory

  implicit none

  integer :: actual_grid_size(3) = base_grid + (2 * ghost_cells)


  call samr%init

  failed = &
  samr%nlevels .ne. nlevels &
  .or. .not. samr%initialized &
  .or. any ( samr%max_nboxes .ne. max_nboxes ) &
  .or. any ( samr%base_grid .ne. base_grid ) &
  .or. any ( samr%ghost_cells .ne. ghost_cells ) &
  .or. any ( samr%levels(0)%boxes(1)%dims .ne. base_grid ) &
  .or. size ( samr%levels(0)%boxes ) .ne. max_nboxes(0) &
  .or. size ( samr%levels(0)%boxes(1)%flags ) .ne. product ( actual_grid_size ) &
  .or. size ( samr%levels(0)%boxes(1)%hydro ) .ne. product ( actual_grid_size ) &
  .or. any ( abs(samr%levels(0)%boxes(1)%left_edge - 0.d0) > epsilon(0.d0) ) &
  .or. any ( abs(samr%levels(0)%boxes(1)%right_edge - 1.d0) > epsilon(0.d0) ) &
  .or. any ( abs ( samr%levels(0)%dx - 1.d0 / base_grid ) > epsilon(0.d0) ) &
  .or. .not. ( abs (samr%levels(0)%refine_factor - 2.d0) < epsilon(0.d0) )

  if ( failed ) return


  call samr1d%init

  failed = &
  any ( abs ( samr1d%levels(1)%dx - [ 1.d0 / ( 2.d0 * base_grid_1d(1)) , 1.d0, 1.d0] ) > epsilon(0.d0) ) &
  .or. any ( abs ( samr1d%levels(2)%dx - [ 1.d0 / ( 4.d0 * base_grid_1d(1)) , 1.d0, 1.d0] ) > epsilon(0.d0) ) &
  .or. any ( abs ( samr1d%levels(3)%dx - [ 1.d0 / ( 8.d0 * base_grid_1d(1)) , 1.d0, 1.d0] ) > epsilon(0.d0) )
  if ( failed ) return


  call samr_uni%init

  failed = &
  samr_uni%max_nboxes(0) .ne. 1 &
  .or. any ( samr_uni%max_nboxes(1:) .ne. 0 ) &
  .or. size ( samr_uni%levels(0)%boxes ) .ne. 1 &
  .or. samr_uni%levels(0)%nboxes .ne. 1 &
  .or. samr_uni%levels(0)%max_nboxes .ne. 1

end function rhyme_samr_init_test
