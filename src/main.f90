program rhyme
  use rhyme_nombre
  use rhyme_hydro_base
  use rhyme_samr
  use rhyme_samr_boundary_condition
  use rhyme_chemistry
  use rhyme_ideal_gas
  use rhyme_iterative_riemann_solver
  use rhyme_param_parser
  use date_time_module


  implicit none


  type workspace_t
    type ( hydro_conserved_t ), allocatable :: UL(:), UR(:), UBo(:), UT(:), UBa(:), UF(:)
    type ( hydro_flux_t ), allocatable :: FL(:), FR(:), FBo(:), FT(:), FBa(:), FF(:)
    type ( hydro_conserved_t ), allocatable :: dUx(:), dUy(:), dUz(:)
  end type workspace_t


  type ( samr_t ) :: samr
  type ( samr_boundary_condition_t ) :: bc
  type ( chemistry_t ) :: chemi
  type ( ideal_gas_t ) :: ig
  type ( iterative_riemann_solver_config_t ) :: irs_config

  type ( workspace_t ) :: ws

  integer :: i1, i2, i3


  character(len=1024) :: exe_filename, param_file

  ! Initialize date and time
  ! call init_date_time

  call get_command_argument ( 0, exe_filename )
  call get_command_argument ( 1, param_file )

  ! Reading parameter file and converting it to the code units
  if ( parse_params ( param_file, samr, bc, chemi, ig, irs_config ) ) stop

  ! Initializing SAMR
  call samr%init

  ! Initializing Boundary Conditions
  call bc%init ( samr )

  ! Initializing Chemistry
  call chemi%init

  ! Initializing Ideal Gas
  call ig%init ( chemi )

  ! Initializing the Workspace
  allocate ( &
  ws%UL ( samr%base_grid ( hyid%x ) ), &
  ws%UR ( samr%base_grid ( hyid%x ) ), &
  ws%UBo ( samr%base_grid ( hyid%y ) ), &
  ws%UT ( samr%base_grid ( hyid%y ) ), &
  ws%UBa ( samr%base_grid ( hyid%z ) ), &
  ws%UF ( samr%base_grid ( hyid%z ) ), &
  ws%FL ( samr%base_grid ( hyid%x ) ), &
  ws%FR ( samr%base_grid ( hyid%x ) ), &
  ws%FBo ( samr%base_grid ( hyid%y ) ), &
  ws%FT ( samr%base_grid ( hyid%y ) ), &
  ws%FBa ( samr%base_grid ( hyid%z ) ), &
  ws%FF ( samr%base_grid ( hyid%z ) ), &
  ws%dUx ( samr%base_grid ( hyid%x ) ), &
  ws%dUy ( samr%base_grid ( hyid%y ) ), &
  ws%dUz ( samr%base_grid ( hyid%z ) ) &
  )

  do i3 = 1, samr%base_grid(3)
    do i2 = 1, samr%base_grid(2)
      do i1 = 1, samr%base_grid(1)

      end do
    end do
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
