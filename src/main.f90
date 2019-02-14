program rhyme
  use rhyme_nombre
  use rhyme_hydro_base
  use rhyme_samr
  use rhyme_samr_bc
  use rhyme_cfl
  use rhyme_ideal_gas
  use rhyme_initial_condition
  use rhyme_slope_limiter
  use rhyme_iterative_riemann_solver
  use rhyme_muscl_hancock
  use rhyme_param_parser
  use date_time_module

  implicit none

  type ( samr_t ) :: samr
  type ( samr_bc_t ) :: bc
  type ( cfl_t ) :: cfl
  type ( ideal_gas_t ) :: ig
  type ( initial_condition_t ) :: ic
  type ( iterative_riemann_solver_t ) :: irs
  type ( slope_limiter_t ) :: sl
  type ( muscl_hancock_t ) :: mh

  integer :: l, b, step = 1
  real(kind=8) :: t, dt

  character(len=1024) :: exe_filename, param_file


  call get_command_argument ( 0, exe_filename )
  call get_command_argument ( 1, param_file )


  ! Reading parameter file and converting it to code units
  if ( .not. parse_params ( param_file, samr, bc, cfl, ig, ic, irs, sl ) ) stop

  ! Initializing Structured AMR
  call samr%init

  ! Initializing Boundary Conditions
  call bc%init ( samr )

  ! Initializing Ideal Gas
  call ig%init

  ! Applying Initial Condition
  call ic%apply ( ig, samr )

  ! Initializing Iterative Riemann Solver
  call irs%init ( ig )

  ! Initializing MUSCL-Hancock
  call mh%init_with ( cfl, ig, irs, sl, samr )

  dt = cfl%dt ( ig, samr )
  step = 0

  do while ( samr%levels(0)%t < 0.2d0 )

    bc%set_base_grid_boundaries ( samr )

    do l = samr%nlevels - 1, 0, -1
      do b = 1, samr%levels(l)%nboxes
        ! bc%set ( samr%levels(l)%boxes(b), samr%ghost_cells )
        call mh%solve ( l, b, samr%levels(l)%boxes(b), samr%levels(l)%dx, dt )
      end do
    end do

    samr%levels(0)%iteration = samr%levels(0)%iteration + 1

    dt = cfl%dt ( ig, samr )
    t = t + dt
    step = step + 1
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
