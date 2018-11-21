program rhyme
  use date_time_module
  use param_parser_module


  implicit none


  character(len=1024) :: executable_name, param_file

  ! Initialize date and time
  ! call init_date_time

  ! Reading parameter file and converting to code units
  call get_command_argument (0, executable_name)
  call get_command_argument (1, param_file)

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
