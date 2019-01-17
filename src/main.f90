program rhyme
  use rhyme_nombre
  use rhyme_hydro_base
  use rhyme_samr
  use rhyme_samr_boundary_condition
  use rhyme_cfl
  use rhyme_chemistry
  use rhyme_ideal_gas
  use rhyme_initial_condition
  use rhyme_slope_limiter
  use rhyme_iterative_riemann_solver
  use rhyme_muslc_hancock
  use rhyme_param_parser
  use date_time_module

  implicit none

  type ( samr_t ) :: samr
  type ( samr_boundary_condition_t ) :: bc
  type ( cfl_t ) :: cfl
  type ( chemistry_t ) :: chemi
  type ( ideal_gas_t ) :: ig
  type ( initial_condition_t ) :: ic
  type ( iterative_riemann_solver_config_t ) :: irs_config
  type ( slope_limiter_t ) :: sl
  type ( muscl_hancock_t ) :: mh

  integer :: i, l, b, step = 1
  character(len=128) :: output_name
  real(kind=8) :: t, dt

  character(len=1024) :: exe_filename, param_file


  call get_command_argument ( 0, exe_filename )
  call get_command_argument ( 1, param_file )


  ! Reading parameter file and converting it to code units
  if ( .not. parse_params ( param_file, samr, bc, cfl, ig, ic, irs_config, sl ) ) stop

  ! Initializing Structured AMR
  call samr%init

  ! Initializing Boundary Conditions
  call bc%init ( samr )

  ! Initializing Ideal Gas
  call chemi%init
  call ig%init ( chemi )

  ! Initializing MUSCL-Hancock
  call mh%init ( cfl, ig, sl, samr )

  ! Applying Initial Condition
  call ic%apply ( ig, samr, bc )


  dt = cfl%dt ( ig, samr )

  do while ( samr%levels(0)%t < param%final_time )
    mh%setup_workspace ( samr )

    do l = samr%nlevels - 1, 0, -1
      do b = 1, samr%levels(l)%nboxes
        bc%set ( samr%levels(l)%boxes(b), samr%ghost_cells )
        mh%solve ( samr%levels(l)%boxes(b) )
      end do
    end do

    samr%level(0)%iteration = samr%level(0)%iteration + 1
  end do

  do while ( t < .2d0 )
    write (*, '(I0.7,F15.9,A)') step, t, " / .2"

    if ( .not. bc%set ( samr ) ) then
      print *, "Error in BC, t: ", t
      stop
    end if

    do i = 0, samr%levels(0)%boxes(1)%dims(1) + 1

      call sl%run ( cfl, ig, &
      samr%levels(0)%boxes(1)%hydro(i-1, 1, 1), &
      samr%levels(0)%boxes(1)%hydro(i  , 1, 1), &
      samr%levels(0)%boxes(1)%hydro(i+1, 1, 1), &
      ws%phi )

      call ig%half_step_extrapolation ( &
      samr%levels(0)%boxes(1)%hydro(i,1,1), ws%phi, hyid%x, samr%levels(0)%dx(1), dt, ws%UL(i,1,1), ws%UR(i,1,1))

      ws%UL%u(hyid%rho) = max ( ws%UL%u(hyid%rho), epsilon(0.d0) )
      ws%UR%u(hyid%rho) = max ( ws%UR%u(hyid%rho), epsilon(0.d0) )
    end do

    do i = 0, samr%levels(0)%boxes(1)%dims(1)
      call iterative_riemann_solver ( ig, ws%UR(i, 1, 1), ws%UL(i+1, 1, 1), &
      hyid%x, irs_config, ws%star )

      call irs_sampling ( ig, ws%UR(i, 1, 1), ws%UL(i+1, 1, 1), ws%star, hyid%x, &
      0.d0, dt, ws%Ux(i, 1, 1) )

      call ig%flux_at ( ws%Ux( i, 1, 1 ), hyid%x, ws%Fx( i, 1, 1 ) )
    end do


    do i = 1, samr%levels(0)%boxes(1)%dims(1)
      samr%levels(0)%boxes(1)%hydro(i,1,1)%u = samr%levels(0)%boxes(1)%hydro(i,1,1)%u &
      + dt / samr%levels(0)%dx(1) * ( ws%Fx( i-1,1,1 )%f - ws%Fx( i,1,1 )%f )
    end do


    ! if ( mod(step, 100) .eq. 0 ) then
      write(output_name,'("output/",i0.7,".txt")') step

      open ( unit=10, file=output_name, action='write', form='formatted')

      do i = 1, samr%levels(0)%boxes(1)%dims(1)
        write (10, '(F25.12," ",F25.12," ",F25.12," ",F25.12," ",F25.12)') &
        samr%levels(0)%boxes(1)%hydro(i,1,1)%u(hyid%rho), &
        samr%levels(0)%boxes(1)%hydro(i,1,1)%u(hyid%rho_u) / samr%levels(0)%boxes(1)%hydro(i,1,1)%u(hyid%rho), &
        ig%p ( samr%levels(0)%boxes(1)%hydro(i,1,1) ), &
        ig%e_int_sp(samr%levels(0)%boxes(1)%hydro(i,1,1)), &
        samr%levels(0)%boxes(1)%hydro(i,1,1)%u(hyid%e_tot)
      end do

      close(10)
    ! end if


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
