logical function rhyme_cfl_dt_test () result (failed)
  use rhyme_cfl
  use rhyme_ideal_gas_factory
  use rhyme_samr_factory
  use rhyme_assertion

  implicit none

  type ( assertion_t ) :: cfl_tester

  integer, parameter :: base_grid(3) = [ 4, 8, 1 ]
  integer, parameter :: nlevels = 1
  integer, parameter :: ghost_cells(3) = [ 2, 2, 0 ]
  integer, parameter :: max_nboxes( 0:samrid%max_nlevels ) = [ &
    1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0 &
  ]
  integer, parameter :: init_nboxes( 0:samrid%max_nlevels ) = [ &
    1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0 &
  ]

  type ( cfl_t ) :: cfl
  type ( ideal_gas_t ) :: ig
  type ( samr_t ) :: samr

  real ( kind=8 ) :: dt, dt_expected
  real ( kind=8 ) :: v_cs, max_v_cs
  integer :: i, j, k

  cfl_tester = .describe. "CFL"

  call rhyme_ideal_gas_factory_init

  ig%type = igid%diatomic
  call rhyme_ideal_gas_init( ig, ig_chemi, ig_thermo, ig_units, ig_logger )

  call rhyme_samr_factory_fill( nlevels, base_grid, ghost_cells, &
    max_nboxes, init_nboxes, samr, physical=.true. )

  dt = cfl%dt ( ig, samr )

  max_v_cs = calc_v_cs( samr%levels(0)%boxes(1)%hydro(1,1,1) )

  do k = 1, samr%levels(0)%boxes(1)%dims(3)
    do j = 1, samr%levels(0)%boxes(1)%dims(2)
      do i = 1, samr%levels(0)%boxes(1)%dims(1)
        v_cs = calc_v_cs( samr%levels(0)%boxes(1)%hydro(i, j, k) )
        if ( v_cs > max_v_cs ) max_v_cs = v_cs
      end do
    end do
  end do

  dt_expected = cfl%courant_number * minval ( samr%levels(0)%dx ) / max_v_cs

  call cfl_tester%expect( dt .toBe. dt_expected )

  failed = cfl_tester%failed()

contains
  real ( kind=8 ) function calc_v_cs ( U ) result ( v )
    implicit none

    type ( hydro_conserved_t ), intent ( in ) :: U

    v = sqrt( sum( U%u( hyid%rho_u:hyid%rho_w )**2 / U%u( hyid%rho )**2 ) ) &
      + ig%cs( U )
  end function calc_v_cs

end function rhyme_cfl_dt_test
