module rhyme_muscl_hancock
  use rhyme_mh_workspace
  use rhyme_cfl
  use rhyme_ideal_gas
  use rhyme_slope_limiter
  use rhyme_iterative_riemann_solver

  implicit none


  type rhyme_muscl_hancock_indices_t
  end type rhyme_muscl_hancock_indices_t

  type ( rhyme_muscl_hancock_indices_t ), parameter :: mhid = rhyme_muscl_hancock_indices_t ()


  type muscl_hancock_t
    type ( mh_workspace_t ) :: ws
    logical :: initialized = .false.
  contains
    procedure :: init => rhyme_muscl_hancock_init
    procedure :: solve => rhyme_muscl_hancock_solve_memory_intensive
    procedure :: solve_memory_intensive => rhyme_muscl_hancock_solve_memory_intensive
  end type muscl_hancock_t

contains

  pure subroutine rhyme_muscl_hancock_init ( this, samr )
    implicit none

    class ( muscl_hancock_t ), intent ( inout ) :: this
    type ( samr_t ), intent ( in ) :: samr

    if ( this%initialized ) return
    if ( this%ws%initialized ) return

    call this%ws%init( samr )

    this%initialized = .true.
  end subroutine rhyme_muscl_hancock_init


  pure subroutine rhyme_muscl_hancock_solve_memory_intensive( this, box, dx, dt, cfl, ig, irs, sl )
    implicit none

    class ( muscl_hancock_t ), intent ( inout ) :: this
    type ( samr_box_t ), intent ( inout ) :: box
    real ( kind=8 ), intent ( in ) :: dx(3), dt
    type ( cfl_t ), intent ( in ) :: cfl
    type ( ideal_gas_t ), intent ( in ) :: ig
    type ( iterative_riemann_solver_t ), intent ( in ) :: irs
    type ( slope_limiter_t ), intent ( in ) :: sl

    integer :: l, b, i, j, k
    integer :: lb(3), ub(3)
    integer :: dirs(3)
    logical :: active_dir(3)
    integer :: flux_factor(3)

    type ( rp_star_region_t ) :: star
    type ( hydro_conserved_t ) :: Delta, evolved_hydro_state

    l = box%level
    b = box%number

    call this%ws%check ( l, b, box )

    active_dir = box%dims > 1
    flux_factor = merge ( 1, 0, active_dir )
    dirs = [ hyid%x, hyid%y, hyid%z ]

    lb = merge ( lbound( box%hydro ), 0, active_dir )
    ub = merge ( ubound( box%hydro ), 2, active_dir )

    do k = lb(3) + 1, ub(3) - 1
      do j = lb(2) + 1, ub(2) - 1
        do i = lb(1) + 1, ub(1) - 1
          if ( active_dir( hyid%x ) ) then
            call sl%run ( cfl, ig, &
              box%hydro( i-1, j, k ), &
              box%hydro( i  , j, k ), &
              box%hydro( i+1, j, k ), &
              Delta &
            )
            call ig%half_step_extrapolation ( &
              box%hydro(i,j,k), Delta, hyid%x, dx(hyid%x), dt, &
              this%ws%levels(l)%boxes(b)%UL( i, j, k, hyid%x ), &
              this%ws%levels(l)%boxes(b)%UR( i, j, k, hyid%x ) &
            )
          end if
          if ( active_dir( hyid%y ) ) then
            call sl%run ( cfl, ig, &
              box%hydro( i, j-1, k ), &
              box%hydro( i, j  , k ), &
              box%hydro( i, j+1, k ), &
              Delta &
            )
            call ig%half_step_extrapolation ( &
              box%hydro(i,j,k), Delta, hyid%y, dx(hyid%y), dt, &
              this%ws%levels(l)%boxes(b)%UL( i, j, k, hyid%y ), &
              this%ws%levels(l)%boxes(b)%UR( i, j, k, hyid%y ) &
            )
          end if
          if ( active_dir( hyid%z ) ) then
            call sl%run ( cfl, ig, &
              box%hydro( i, j, k-1 ), &
              box%hydro( i, j, k   ), &
              box%hydro( i, j, k+1 ), &
              Delta &
            )
            call ig%half_step_extrapolation ( &
              box%hydro(i,j,k), Delta, hyid%z, dx(hyid%z), dt, &
              this%ws%levels(l)%boxes(b)%UL( i, j, k, hyid%z ), &
              this%ws%levels(l)%boxes(b)%UR( i, j, k, hyid%z ) &
            )
          end if
        end do
      end do
    end do


    lb = merge ( lbound( box%hydro ), 0, active_dir )
    ub = merge ( ubound( box%hydro ), 1, active_dir )

    do k = lb(3) + 1, ub(3)
      do j = lb(2) + 1, ub(2)
        do i = lb(1) + 1, ub(1)
          if ( active_dir(hyid%x) ) then
            call irs%solve ( ig, &
              this%ws%levels(l)%boxes(b)%UR( i  , j, k, hyid%x ), &
              this%ws%levels(l)%boxes(b)%UL( i+1, j, k, hyid%x ), &
              hyid%x, star &
            )
            call irs%sampling ( ig, &
              this%ws%levels(l)%boxes(b)%UR( i  , j, k, hyid%x ), &
              this%ws%levels(l)%boxes(b)%UL( i+1, j, k, hyid%x ), &
              star, hyid%x, 0.d0, dt, evolved_hydro_state &
            )
            call ig%flux_at ( evolved_hydro_state, hyid%x, &
              this%ws%levels(l)%boxes(b)%FR( i, j, k, hyid%x ) )
          end if
          if ( active_dir(hyid%y) ) then
            call irs%solve ( ig, &
              this%ws%levels(l)%boxes(b)%UR( i, j  , k, hyid%y ), &
              this%ws%levels(l)%boxes(b)%UL( i, j+1, k, hyid%y ), &
              hyid%y, star &
            )
            call irs%sampling ( ig, &
              this%ws%levels(l)%boxes(b)%UR( i, j  , k, hyid%y ), &
              this%ws%levels(l)%boxes(b)%UL( i, j+1, k, hyid%y ), &
              star, hyid%y, 0.d0, dt, evolved_hydro_state &
            )
            call ig%flux_at ( evolved_hydro_state, hyid%y, &
              this%ws%levels(l)%boxes(b)%FR( i, j, k, hyid%y ) )
          end if
          if ( active_dir(hyid%z) ) then
            call irs%solve ( ig, &
              this%ws%levels(l)%boxes(b)%UR( i, j, k  , hyid%z ), &
              this%ws%levels(l)%boxes(b)%UL( i, j, k+1, hyid%z ), &
              hyid%z, star &
            )
            call irs%sampling ( ig, &
              this%ws%levels(l)%boxes(b)%UR( i, j, k  , hyid%z ), &
              this%ws%levels(l)%boxes(b)%UL( i, j, k+1, hyid%z ), &
              star, hyid%z, 0.d0, dt, evolved_hydro_state &
            )
            call ig%flux_at ( evolved_hydro_state, hyid%z, &
              this%ws%levels(l)%boxes(b)%FR( i, j, k, hyid%z ) )
          end if
        end do
      end do
    end do

    do k = 1, box%dims(3)
      do j = 1, box%dims(2)
        do i = 1, box%dims(1)
          box%hydro(i,j,k)%u = &
            box%hydro(i,j,k)%u &
            + &
            ( &
              flux_factor( hyid%x ) * dt / dx( hyid%x ) &
              * ( &
                this%ws%levels(l)%boxes(b)%FR( i-1, j, k, hyid%x )%f &
                - this%ws%levels(l)%boxes(b)%FR( i, j, k, hyid%x )%f &
              ) &
              + &
              flux_factor( hyid%y ) * dt / dx( hyid%y ) &
              * ( &
                this%ws%levels(l)%boxes(b)%FR( i, j-1, k, hyid%y )%f &
                - this%ws%levels(l)%boxes(b)%FR( i, j, k, hyid%y )%f &
              ) &
              + &
              flux_factor( hyid%z ) * dt / dx( hyid%z ) &
              * ( &
                this%ws%levels(l)%boxes(b)%FR( i, j, k-1, hyid%z )%f &
                - this%ws%levels(l)%boxes(b)%FR( i, j, k, hyid%z )%f &
              ) &
            )
        end do
      end do
    end do
  end subroutine rhyme_muscl_hancock_solve_memory_intensive
end module rhyme_muscl_hancock
