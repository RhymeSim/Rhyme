logical function rhyme_initial_condition_init_simple_test () result ( failed )
  use rhyme_initial_condition_factory
  use rhyme_assertion

  implicit none

  type ( assertion_t ) :: ic_tester

  integer :: i, actual_grid_size(3) = base_grid + ( 2 * ghost_cells )
  type ( samr_t ) :: samr, samr1d, samr_uni

  ic_tester = .describe. "initial_condition init_simple"

  call rhyme_initial_condition_factory_init

  ! SAMR test
  call simple%init( samr, ig, log )

  call ic_tester%expect( samr%nlevels .toBe. nlevels )
  call ic_tester%expect( samr%max_nboxes .toBe. max_nboxes )
  call ic_tester%expect( samr%base_grid .toBe. base_grid )
  call ic_tester%expect( samr%ghost_cells .toBe. ghost_cells )
  call ic_tester%expect( (samr%levels%level) .toBe. [ (i, i=0, 23) ] )
  call ic_tester%expect( samr%levels(0)%boxes(1)%dims .toBe. base_grid )
  call ic_tester%expect( size( samr%levels(0)%boxes ) .toBe. max_nboxes(0) )
  call ic_tester%expect( size( samr%levels(0)%boxes(1)%flags ) .toBe. product( actual_grid_size ) )
  call ic_tester%expect( size( samr%levels(0)%boxes(1)%hydro ) .toBe. product( actual_grid_size ) )
  call ic_tester%expect( samr%levels(0)%boxes(1)%left_edge .toBe. 1 )
  call ic_tester%expect( samr%levels(0)%boxes(1)%right_edge .toBe. base_grid )
  call ic_tester%expect( samr%levels(0)%dx .toBe. 1.d0 / base_grid )
  call ic_tester%expect( samr%levels(0)%refine_factor .toBe. 2.d0 )

  ! 1D SAMR test
  call simple1d%init( samr1d, ig, log )

  call ic_tester%expect( samr1d%levels(1)%dx .toBe. [ 1.d0 / ( 2.d0 * base_grid_1d(1)) , 1.d0, 1.d0] )
  call ic_tester%expect( samr1d%levels(2)%dx .toBe. [ 1.d0 / ( 4.d0 * base_grid_1d(1)) , 1.d0, 1.d0] )
  call ic_tester%expect( samr1d%levels(3)%dx .toBe. [ 1.d0 / ( 8.d0 * base_grid_1d(1)) , 1.d0, 1.d0] )

  ! Uniform SAMR test
  call simple_uni%init( samr_uni, ig, log )

  call ic_tester%expect( samr_uni%max_nboxes(0) .toBe. 1 )
  call ic_tester%expect( samr_uni%max_nboxes(1:) .toBe. 0 )
  call ic_tester%expect( size( samr_uni%levels(0)%boxes ) .toBe. 1 )
  call ic_tester%expect( samr_uni%levels(0)%nboxes .toBe. 1 )
  call ic_tester%expect( samr_uni%levels(0)%max_nboxes .toBe. 1 )

  failed = ic_tester%failed()
end function rhyme_initial_condition_init_simple_test
