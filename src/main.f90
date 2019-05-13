program rhyme
  use rhyme_log
  use rhyme_nombre
  use rhyme_units
  use rhyme_hydro_base
  use rhyme_samr
  use rhyme_samr_bc
  use rhyme_cfl
  use rhyme_ideal_gas
  use rhyme_chemistry
  use rhyme_thermo_base
  use rhyme_drawing
  use rhyme_slope_limiter
  use rhyme_irs
  use rhyme_muscl_hancock
  use rhyme_param_parser
  use rhyme_chombo
  use rhyme_initial_condition

  implicit none

  type ( log_t ) :: logger
  type ( rhyme_units_t ) :: units
  type ( samr_t ) :: samr
  type ( samr_bc_t ) :: bc
  type ( cfl_t ) :: cfl
  type ( chemistry_t ) :: chemistry
  type ( thermo_base_t ) :: thermo
  type ( ideal_gas_t ) :: ig
  type ( drawing_t ) :: draw
  type ( irs_t ) :: irs
  type ( slope_limiter_t ) :: sl
  type ( muscl_hancock_t ) :: mh
  type ( mh_workspace_t ) :: mhws
  type ( chombo_t ) :: chombo
  type ( initial_condition_t ) :: ic

  integer :: l, b
  character ( len=1024 ) :: exe_filename, param_file


  ! TODO: use getopt to read flag-based additional command line arguments
  call get_command_argument ( 0, exe_filename )
  call get_command_argument ( 1, param_file )


  call logger%start
  call logger%log( 'command line argument:', 'exe', '=', [ exe_filename ] )
  call logger%log( 'command line argument:', 'param_file', '=', [ param_file ] )
  call logger%log( '', 'log_file', '=', [ logger%logfile ] )
  call logger%log( '', 'err_file', '=', [ logger%errfile ] )


  ! Reading parameters and converting them to code units
  call load_params( param_file, logger, units, ic, bc, cfl, ig, draw, irs, sl, mh, chombo )

  ! Initializing
  call logger%set_section( 'init' )

  ! Units
  call rhyme_units_init( units, logger )

  ! Chemistry
  call rhyme_chemistry_init( chemistry, units, logger )

  ! Thermodynamics
  call rhyme_thermo_base_init( thermo, units, logger )

  ! Ideal Gas
  call rhyme_ideal_gas_init( ig, chemistry, thermo, units, logger )

  ! Structured AMR
  call rhyme_initial_condition_init( ic, samr, units, logger )

  ! Boundary Conditions
  call bc%init( samr, logger )

  ! Initial Condition ( Drawing )
  call rhyme_drawing_apply( draw, ig, samr, logger )

  ! Iterative Riemann Solver
  call irs%init( logger )

  ! MUSCL-Hancock
  mhws%type = mh%solver_type
  call rhyme_muscl_hancock_init( mh, samr, mhws, logger )

  ! Chombo Output
  call chombo%init( logger )


  ! Main loop
  do while ( samr%levels(0)%t < 0.4d0 )
    call logger%set_iteration_section( samr%levels(0)%iteration )

    samr%levels(0)%dt = rhyme_cfl_time_step( cfl, ig, samr )

    call logger%log( '', 't', '=', [ samr%levels(0)%t ] )
    call logger%log( '', 'dt', '=', [ samr%levels(0)%dt ] )

    call logger%start_task( 'boundary-condition' )
    call bc%set_base_grid_boundaries( samr )
    call logger%done

    ! Update structured AMR
    ! Update ghost cells of boxes

    ! Store a snapshot if necessary
    if ( modulo(samr%levels(0)%iteration, 1) .eq. 0 ) then
      call logger%start_task( 'storing-snapshot')
      call chombo%write_samr( samr )
      call logger%done
    end if


    call logger%start_task( 'hydro-solver', 'MUSCL-Hancock Scheme')
    do l = samr%nlevels - 1, 0, -1
      do b = 1, samr%levels(l)%nboxes
        call rhyme_muscl_hancock_solve( mh, samr%levels(l)%boxes(b), &
          samr%levels(l)%dx, samr%levels(l)%dt, cfl, ig, irs, sl, mhws, logger )
      end do
    end do
    call logger%done

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
