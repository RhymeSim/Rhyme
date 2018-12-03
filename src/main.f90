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
    type ( hydro_conserved_t ), allocatable :: UL(:,:,:), UR(:,:,:)
    type ( hydro_conserved_t ), allocatable :: Ux(:,:,:)
    type ( hydro_flux_t ), allocatable :: Fx(:,:,:)
    type ( hydro_conserved_t ), allocatable :: phix(:,:,:)
    type ( rp_star_region_t ) :: star
  end type workspace_t


  type ( samr_t ) :: samr
  type ( samr_boundary_condition_t ) :: bc
  type ( chemistry_t ) :: chemi
  type ( ideal_gas_t ) :: ig
  type ( iterative_riemann_solver_config_t ) :: irs_config

  type ( workspace_t ) :: ws

  integer :: i, step = 1
  character(len=128) :: output_name
  real(kind=8) :: t, dt


  character(len=1024) :: exe_filename, param_file

  ! Initialize date and time
  ! call init_date_time

  call get_command_argument ( 0, exe_filename )
  call get_command_argument ( 1, param_file )

  ! Reading parameter file and converting it to the code units
  if ( .not. parse_params ( param_file, samr, bc, chemi, ig, irs_config ) ) stop

  ! Initializing SAMR
  call samr%init

  ! Initializing Boundary Conditions
  call bc%init ( samr )

  ! Initializing Chemistry
  call chemi%init

  ! Initializing Ideal Gas
  call ig%init ( chemi )


  ! IC
  do i = 1, samr%base_grid(1)
    if ( i < samr%base_grid(1) / 2 ) then
      samr%boxes(1)%hydro( i,1,1 )%u = [1.d0, 0.d0, 0.d0, 0.d0, 1.d0]
    else
      samr%boxes(1)%hydro( i,1,1 )%u = [.125d0, 0.d0, 0.d0, 0.d0, .1d0]
    end if
  end do


  ! Initializing the Workspace
  allocate ( &
  ws%UL ( 0:samr%base_grid ( hyid%x )+1, samr%base_grid ( hyid%y ), samr%base_grid ( hyid%z ) ), &
  ws%UR ( 0:samr%base_grid ( hyid%x )+1, samr%base_grid ( hyid%y ), samr%base_grid ( hyid%z ) ), &
  ws%phix ( 0:samr%base_grid ( hyid%x )+1, samr%base_grid ( hyid%y ), samr%base_grid ( hyid%z ) ), &
  ws%Ux ( 0:samr%base_grid ( hyid%x ), samr%base_grid ( hyid%y ), samr%base_grid ( hyid%z ) ), &
  ws%Fx ( 0:samr%base_grid ( hyid%x ), samr%base_grid ( hyid%y ), samr%base_grid ( hyid%z ) ) &
  )

  dt = 0.00001
  t = dt

  do while ( t < 5d0 )
    write (*, '(I0.7,F15.9,A)') step, t, " 5.0"

    if ( .not. bc%set ( samr ) ) then
      print *, "Error in BC, t: ", t
      stop
    end if

    do i = 0, samr%base_grid(1) + 1
      call ig%half_step_extrapolation ( &
      samr%boxes(1)%hydro(i-1, 1, 1), &
      samr%boxes(1)%hydro(i, 1, 1), &
      samr%boxes(1)%hydro(i+1, 1, 1), &
      hyid%x, samr%levels(1)%dx(1), dt, &
      ws%UL(i, 1, 1), ws%UR(i, 1, 1) &
      )

      ws%UL%u(hyid%rho) = max ( ws%UL%u(hyid%rho), epsilon(0.d0) )
      ws%UR%u(hyid%rho) = max ( ws%UR%u(hyid%rho), epsilon(0.d0) )
    end do

    do i = 0, samr%base_grid(1)
      call iterative_riemann_solver ( ig, ws%UR(i, 1, 1), ws%UL(i+1, 1, 1), &
      hyid%x, irs_config, ws%star )

      call irs_sampling ( ig, ws%UR(i, 1, 1), ws%UL(i+1, 1, 1), ws%star, hyid%x, &
      0.d0, dt, ws%Ux(i, 1, 1) )

      call ig%flux_at ( ws%Ux( i, 1, 1 ), hyid%x, ws%Fx( i, 1, 1 ) )
    end do


    do i = 1, samr%base_grid(1)
      samr%boxes(1)%hydro(i,1,1)%u = samr%boxes(1)%hydro(i,1,1)%u &
      + dt / samr%levels(1)%dx(1) * ( ws%Fx( i-1,1,1 )%f - ws%Fx( i,1,1 )%f )
    end do


    if ( mod(step, 100) .eq. 0 ) then
      write(output_name,'("output/",i0.7,".txt")') step

      open ( unit=10, file=output_name, action='write', form='formatted')

      do i = 1, samr%base_grid(1)
        write (10, '(F25.12," ",F25.12," ",F25.12," ",F25.12," ",F25.12)') &
        samr%boxes(1)%hydro(i,1,1)%u(hyid%rho), &
        samr%boxes(1)%hydro(i,1,1)%u(hyid%rho_u), &
        samr%boxes(1)%hydro(i,1,1)%u(hyid%rho_v), &
        samr%boxes(1)%hydro(i,1,1)%u(hyid%rho_w), &
        ig%p(samr%boxes(1)%hydro(i,1,1))
      end do

      close(10)
    end if

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
