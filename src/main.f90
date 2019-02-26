program rhyme
  use rhyme_log
  use rhyme_nombre
  use rhyme_hydro_base
  use rhyme_samr
  use rhyme_samr_bc
  use rhyme_cfl
  use rhyme_ideal_gas
  use rhyme_drawing
  use rhyme_slope_limiter
  use rhyme_iterative_riemann_solver
  use rhyme_muscl_hancock
  use rhyme_param_parser
  use rhyme_chombo
  use rhyme_initial_condition

  implicit none

  type ( log_t ) :: log
  type ( samr_t ) :: samr
  type ( samr_bc_t ) :: bc
  type ( cfl_t ) :: cfl
  type ( ideal_gas_t ) :: ig
  type ( drawing_t ) :: draw
  type ( iterative_riemann_solver_t ) :: irs
  type ( slope_limiter_t ) :: sl
  type ( muscl_hancock_t ) :: mh
  type ( chombo_t ) :: chombo
  type ( initial_condition_t ) :: ic

  integer :: l, b
  character ( len=1024 ) :: exe_filename, param_file


  ! TODO: use getopt to read flag-based additional command line arguments
  call get_command_argument ( 0, exe_filename )
  call get_command_argument ( 1, param_file )
  call get_command_argument ( 2, log%logfile )
  call get_command_argument ( 3, log%errfile )


  call log%init
  call log%write_kw ( 'exe', exe_filename )
  call log%write_kw ( 'param_file', param_file )
  call log%write_kw ( 'log_file', log%logfile )
  call log%write_kw ( 'err_file', log%errfile )


  ! Reading parameters and converting them to code units
  if ( .not. parse_params( param_file, log, ic, bc, cfl, ig, draw, irs, sl, chombo ) ) stop

  ! Initializing
  call log%set_section( 'init' )

  ! Structured AMR
  call ic%init( samr, log )

  ! Boundary Conditions
  call bc%init( samr )

  ! Ideal Gas
  call ig%init

  ! Initial Condition
  call draw%apply( ig, samr )

  ! Iterative Riemann Solver
  call irs%init( ig )

  ! MUSCL-Hancock
  call mh%init_with( cfl, ig, irs, sl, samr )

  ! Chombo Output
  call chombo%init( log )


  ! Main loop
  do while ( samr%levels(0)%t < 0.2d0 )
    call log%begin_iteration( samr%levels(0)%iteration )

    samr%levels(0)%dt = cfl%dt( ig, samr )

    call log%write_kw( 't', samr%levels(0)%t )
    call log%write_kw( 'dt', samr%levels(0)%dt )

    call bc%set_base_grid_boundaries( samr )
    call log%done( 'set_base_grid_boundaries' )

    ! Update structured AMR
    ! Update workspace
    ! Update ghost cells of boxes

    do l = samr%nlevels - 1, 0, -1
      do b = 1, samr%levels(l)%nboxes
        call mh%solve( l, b, samr%levels(l)%boxes(b), samr%levels(l)%dx, samr%levels(l)%dt )
      end do
    end do
    call log%done( 'Hydro solver' )

    ! Store a snapshot if necessary
    if ( modulo(samr%levels(0)%iteration, 2) .eq. 0 ) then
      call chombo%write_samr( samr )
      call log%done( 'Drop an output (chombo) file' )
    end if

    samr%levels(0)%t = samr%levels(0)%t + samr%levels(0)%dt
    samr%levels(0)%iteration = samr%levels(0)%iteration + 1
  end do

  ! Initialize cosmological variables (if COSMO is set)

  ! Setup patch-based GRID

  ! Creating emission spectra for ionizing sources

  ! Initialize ionization cross section

  ! Initialize coefficients

  ! Initialize pre-existing HII regions

  ! Drop a snapshot (if needed)

  ! Create problem domain array (if needed)

  ! Time evolution loop

  !! Compute Timestep

  !! if HYDRO is set
  !!! Copy old hydro states to new ones
  !!! Hydro loop
  !!!! Setup boundary conditions
  !!!! Gather neighboring cells (into a 6x6x6 array)
  !!!! Calculating slope limiter
  !!!! Extrapolate states to cell faces
  !!!! Riemann solver
  !!!! Sampling solution
  !!!! Updating fluxes
  !!! end Hydro loop
  !!! Rewrite new hydro states into old ones
  !! end if Hydro is set

  !! Get active sources

  !! Deoposite photons loop
  !!! Ray tracing
  !!! Solving the ionization equations
  !! end Deposite photons loop

  !! Drop snapshot (if needed)
end program rhyme
