logical function rhyme_muscl_hancock_advection_test () result ( failed )
  use rhyme_muscl_hancock
  use rhyme_samr_bc_factory
  use rhyme_chombo_factory

  implicit none

  type ( muscl_hancock_t ) :: mh
  type ( cfl_t ) :: cfl
  type ( chemistry_t ) :: chemi
  type ( thermo_base_t ) :: thermo
  type ( ideal_gas_t ) :: ig
  type ( irs_t ) :: irs
  type ( slope_limiter_t ) :: sl
  type ( samr_bc_t ) :: bc
  type ( chombo_t ) :: ch
  type ( log_t ) :: log

  integer, parameter :: nlevels = 1
  integer, parameter :: base_grid_2d(3) = [ 64, 64, 1 ]
  integer, parameter :: ghost_cells_2d(3) = [ 2, 2, 0 ]
  integer, parameter :: max_nboxes ( 0:samrid%max_nlevels ) = [ &
    1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0 &
  ]
  integer, parameter :: init_nboxes ( 0:samrid%max_nlevels ) = [ &
    1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0 &
  ]
  type ( samr_t ) :: samr_2d

  integer :: i, j, step
  real ( kind=8 ) :: rhol, rhor, v , p, dx, dt
  type ( hydro_conserved_t ) :: left, right

  call rhyme_samr_factory_fill( &
    nlevels, base_grid_2d, ghost_cells_2d, max_nboxes, init_nboxes, samr_2d )

  cfl%courant_number = 0.2d0

  sl%type = slid%minmod

  ch%nickname = 'rhyme_muscl_hancock_advection_test'

  call chemi%init( log )
  call thermo%init( log )
  call ig%init_with( chemi, thermo, igid%diatomic, log )

  call mh%init( samr_2d, log )


  rhol = .125d0
  rhor = 1.d0
  p = .1d0
  dt = cfl%courant_number * minval( samr_2d%levels(0)%dx ) / sqrt( ig%gamma *  p / rhol )
  dx = 1.d0 / base_grid_2d(1)
  v = dx / dt

  call ig%prim_vars_to_cons( rhol, v, 0.d0, 0.d0, p, left )
  call ig%prim_vars_to_cons( rhor, v, 0.d0, 0.d0, p, right )

  do j = 1, samr_2d%levels(0)%boxes(1)%dims(2)
    do i = 1, samr_2d%levels(0)%boxes(1)%dims(1)
      if ( i <= samr_2d%levels(0)%boxes(1)%dims(1) / 2 ) then
        samr_2d%levels(0)%boxes(1)%hydro(i,j,1)%u = left%u
      else
        samr_2d%levels(0)%boxes(1)%hydro(i,j,1)%u = right%u
      end if
    end do
  end do

  bc%types(bcid%left) = bcid%periodic
  bc%types(bcid%right) = bcid%periodic
  bc%types(bcid%bottom) = bcid%outflow
  bc%types(bcid%top) = bcid%outflow
  bc%types(bcid%back) = bcid%outflow
  bc%types(bcid%front) = bcid%outflow

  do step = 1, 0
    samr_2d%levels(0)%dt = cfl%dt( ig, samr_2d )

    call ch%write_samr( samr_2d )

    call bc%set_base_grid_boundaries( samr_2d )

    call mh%solve( &
      samr_2d%levels(0)%boxes(1), &
      samr_2d%levels(0)%dx, &
      samr_2d%levels(0)%dt, &
      cfl, ig, irs, sl &
    )

    samr_2d%levels(0)%t = samr_2d%levels(0)%t + samr_2d%levels(0)%dt
    samr_2d%levels(0)%iteration = samr_2d%levels(0)%iteration + 1
  end do

  failed = .false.
end function rhyme_muscl_hancock_advection_test
