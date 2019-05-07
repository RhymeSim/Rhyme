logical function rhyme_initial_condition_init_simple_test () result ( failed )
  use rhyme_initial_condition_factory
  use rhyme_assertion

  implicit none

  type ( assertion_t ) :: ic_tester

  type ( initial_condition_t ) :: simple_3d, simple_1d, simple_uni
  type ( samr_t ) :: samr_3d, samr_1d, samr_uni
  integer :: i, actual_grid_size(3)

  ic_tester = .describe. "initial_condition_init_simple"

  ! SAMR test
  simple_3d = ic_factory%generate( ic_factory%simple_3d, 4 )
  call rhyme_initial_condition_init( simple_3d, samr_3d, ic_factory%ig, &
    ic_factory%units, ic_factory%logger )

  call ic_tester%expect( samr_3d%nlevels .toBe. simple_3d%nlevels )
  call ic_tester%expect( samr_3d%max_nboxes .toBe. simple_3d%max_nboxes )
  call ic_tester%expect( samr_3d%base_grid .toBe. simple_3d%base_grid )
  call ic_tester%expect( samr_3d%ghost_cells .toBe. [ 2, 2, 2 ] )
  call ic_tester%expect( (samr_3d%levels%level) .toBe. [ (i, i=0, 23) ] )
  call ic_tester%expect( samr_3d%levels(0)%boxes(1)%dims .toBe. simple_3d%base_grid )
  call ic_tester%expect( size( samr_3d%levels(0)%boxes ) .toBe. simple_3d%max_nboxes(0) )

  actual_grid_size = simple_3d%base_grid + ( 2 * [ 2, 2, 2 ] )
  call ic_tester%expect( size( samr_3d%levels(0)%boxes(1)%flags ) .toBe. product( actual_grid_size ) )
  call ic_tester%expect( size( samr_3d%levels(0)%boxes(1)%hydro ) .toBe. product( actual_grid_size ) )

  call ic_tester%expect( samr_3d%levels(0)%boxes(1)%left_edge .toBe. 1 )
  call ic_tester%expect( samr_3d%levels(0)%boxes(1)%right_edge .toBe. simple_3d%base_grid )
  call ic_tester%expect( samr_3d%levels(0)%dx .toBe. simple_3d%box_lengths%v / simple_3d%base_grid )
  call ic_tester%expect( samr_3d%levels(0)%refine_factor .toBe. 2.d0 )

  ! 1D SAMR test
  simple_1d = ic_factory%generate( ic_factory%simple_1d, 4 )
  call rhyme_initial_condition_init( simple_1d, samr_1d, ic_factory%ig, &
    ic_factory%units, ic_factory%logger )

  call ic_tester%expect( samr_1d%levels(0)%dx &
    .toBe. ( simple_1d%box_lengths%v / ( 1.d0 * simple_1d%base_grid ) ) )
  call ic_tester%expect( samr_1d%levels(1)%dx &
    .toBe. ( simple_1d%box_lengths%v / ( 2.d0 * simple_1d%base_grid ) ) )
  call ic_tester%expect( samr_1d%levels(2)%dx &
    .toBe. ( simple_1d%box_lengths%v / ( 4.d0 * simple_1d%base_grid ) ) )
  call ic_tester%expect( samr_1d%levels(3)%dx &
    .toBe. ( simple_1d%box_lengths%v / ( 8.d0 * simple_1d%base_grid ) ) )

  ! Uniform SAMR test
  simple_uni = ic_factory%generate( ic_factory%simple_uni )
  call rhyme_initial_condition_init( simple_uni, samr_uni, ic_factory%ig, &
    ic_factory%units, ic_factory%logger )

  call ic_tester%expect( samr_uni%max_nboxes(0) .toBe. 1 )
  call ic_tester%expect( samr_uni%max_nboxes(1:) .toBe. 0 )
  call ic_tester%expect( size( samr_uni%levels(0)%boxes ) .toBe. 1 )
  call ic_tester%expect( samr_uni%levels(0)%nboxes .toBe. 1 )
  call ic_tester%expect( samr_uni%levels(0)%max_nboxes .toBe. 1 )

  failed = ic_tester%failed()
end function rhyme_initial_condition_init_simple_test
