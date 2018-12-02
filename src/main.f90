program rhyme
  use rhyme_nombre
  use rhyme_samr
  use rhyme_samr_boundary_condition
  use rhyme_chemistry
  use rhyme_ideal_gas
  use rhyme_iterative_riemann_solver
  use rhyme_param_parser
  use date_time_module


  implicit none


  type ( samr_t ) :: samr
  type ( samr_boundary_condition_t ) :: bc
  type ( chemistry_t ) :: chemi
  type ( ideal_gas_t ) :: ig


  character(len=1024) :: exe_filename, param_file

  ! Initialize date and time
  ! call init_date_time

  ! Reading parameter file and converting to code units
  call get_command_argument (0, exe_filename)
  call get_command_argument (1, param_file)

  if ( parse_params ( param_file, samr, bc, chemi, ig ) ) stop

  ! Initializing SAMR
  call samr%init

  ! Initializing boundary conditions
  call bc%init ( samr )

  ! Initialize Chemistry
  call chemi%init

  ! Initialize ideal gas
  call ig%init ( chemi )


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
