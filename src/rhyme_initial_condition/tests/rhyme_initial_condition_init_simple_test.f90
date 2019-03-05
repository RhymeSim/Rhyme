logical function rhyme_initial_condition_init_simple_test () result ( failed )
  use rhyme_initial_condition_factory

  implicit none

  integer :: i, actual_grid_size(3) = base_grid + ( 2 * ghost_cells )
  type ( samr_t ) :: samr, samr1d, samr_uni
  type ( chemistry_t ) :: chemi
  type ( thermo_base_t ) :: thermo
  type ( ideal_gas_t ) :: ig
  type ( log_t ) :: log

  call chemi%init
  call thermo%init
  call ig%init_with( chemi, thermo, igid%monatomic )

  ! SAMR test
  call simple%init( samr, ig, log )

  failed = &
  samr%nlevels .ne. nlevels &
  .or. any ( samr%max_nboxes .ne. max_nboxes ) &
  .or. any ( samr%base_grid .ne. base_grid ) &
  .or. any ( samr%ghost_cells .ne. ghost_cells ) &
  .or. any ( samr%levels%level .ne. [ (i, i=0, 23) ] ) &
  .or. any ( samr%levels(0)%boxes(1)%dims .ne. base_grid ) &
  .or. size ( samr%levels(0)%boxes ) .ne. max_nboxes(0) &
  .or. size ( samr%levels(0)%boxes(1)%flags ) .ne. product ( actual_grid_size ) &
  .or. size ( samr%levels(0)%boxes(1)%hydro ) .ne. product ( actual_grid_size ) &
  .or. any ( samr%levels(0)%boxes(1)%left_edge .ne. 1 ) &
  .or. any ( samr%levels(0)%boxes(1)%right_edge .ne. base_grid ) &
  .or. any ( abs ( samr%levels(0)%dx - 1.d0 / base_grid ) > epsilon(0.d0) ) &
  .or. .not. ( abs (samr%levels(0)%refine_factor - 2.d0) < epsilon(0.d0) )
  if ( failed ) return


  ! 1D SAMR test
  call simple1d%init( samr1d, ig, log )

  failed = &
  any ( abs ( samr1d%levels(1)%dx - [ 1.d0 / ( 2.d0 * base_grid_1d(1)) , 1.d0, 1.d0] ) > epsilon(0.d0) ) &
  .or. any ( abs ( samr1d%levels(2)%dx - [ 1.d0 / ( 4.d0 * base_grid_1d(1)) , 1.d0, 1.d0] ) > epsilon(0.d0) ) &
  .or. any ( abs ( samr1d%levels(3)%dx - [ 1.d0 / ( 8.d0 * base_grid_1d(1)) , 1.d0, 1.d0] ) > epsilon(0.d0) )
  if ( failed ) return


  ! Uniform SAMR test
  call simple_uni%init( samr_uni, ig, log )

  failed = &
  samr_uni%max_nboxes(0) .ne. 1 &
  .or. any ( samr_uni%max_nboxes(1:) .ne. 0 ) &
  .or. size ( samr_uni%levels(0)%boxes ) .ne. 1 &
  .or. samr_uni%levels(0)%nboxes .ne. 1 &
  .or. samr_uni%levels(0)%max_nboxes .ne. 1
  if ( failed ) return
end function rhyme_initial_condition_init_simple_test
