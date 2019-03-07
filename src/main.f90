program rhyme
  use rhyme_log
  use rhyme_nombre
  use rhyme_hydro_base
  use rhyme_samr
  use rhyme_samr_bc
  use rhyme_cfl
  use rhyme_ideal_gas
  use rhyme_chemistry
  use rhyme_thermo_base
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
  type ( chemistry_t ) :: chemistry
  type ( thermo_base_t ) :: thermo
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


  call log%start
  call log%log( 'command line argument:', 'exe', '=', [ exe_filename ] )
  call log%log( 'command line argument:', 'param_file', '=', [ param_file ] )
  call log%log( '', 'log_file', '=', [ log%logfile ] )
  call log%log( '', 'err_file', '=', [ log%errfile ] )


  ! Reading parameters and converting them to code units
  call parse_params( param_file, log, ic, bc, cfl, ig, draw, irs, sl, chombo )

  ! Initializing
  call log%set_section( 'init' )

  ! Chemistry
  call chemistry%init( log )

  ! Thermodynamics
  call thermo%init( log )

  ! Ideal Gas
  call ig%init( chemistry, thermo, log )

  ! Structured AMR
  call ic%init( samr, ig, log )

  ! Boundary Conditions
  call bc%init( samr, log )

  ! Initial Condition
  call draw%apply( ig, samr )

  ! Iterative Riemann Solver
  call irs%init( log )

  ! MUSCL-Hancock
  call mh%init( samr, log )

  ! Chombo Output
  call chombo%init( log )


  ! Main loop
  do while ( samr%levels(0)%t < 0.2d0 )
    call log%set_iteration_section( samr%levels(0)%iteration )

    samr%levels(0)%dt = cfl%dt( ig, samr )

    call log%log( '', 't', '=', [ samr%levels(0)%t ] )
    call log%log( '', 'dt', '=', [ samr%levels(0)%dt ] )

    call log%start_task( 'BC', 'only base grid boundaries')
    call bc%set_base_grid_boundaries( samr )
    call log%done

    ! Update structured AMR
    ! Update workspace
    ! Update ghost cells of boxes

    call log%start_task( 'hydro-solver', 'MUSCL-Hancock Scheme')
    do l = samr%nlevels - 1, 0, -1
      do b = 1, samr%levels(l)%nboxes
        call mh%solve( &
          samr%levels(l)%boxes(b), &
          samr%levels(l)%dx, &
          samr%levels(l)%dt, &
          cfl, ig, irs, sl &
        )
      end do
    end do
    call log%done

    ! Store a snapshot if necessary
    if ( modulo(samr%levels(0)%iteration, 10) .eq. 0 ) then
      call log%start_task( 'snapshot', 'Chombo output' )
      call chombo%write_samr( samr )
      call log%done
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
