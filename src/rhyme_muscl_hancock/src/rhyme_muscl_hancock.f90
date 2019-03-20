module rhyme_muscl_hancock
  use rhyme_mh_workspace
  use rhyme_cfl
  use rhyme_ideal_gas
  use rhyme_slope_limiter
  use rhyme_irs
  use rhyme_log

  implicit none


  type rhyme_muscl_hancock_indices_t
  end type rhyme_muscl_hancock_indices_t

  type ( rhyme_muscl_hancock_indices_t ), parameter :: mhid = rhyme_muscl_hancock_indices_t ()


  type muscl_hancock_t
    logical :: initialized = .false.
    type ( mh_workspace_t ) :: ws
    logical :: active_axis(3) = .false.
    integer :: active_flux(3) = 0
  contains
    procedure :: init => rhyme_muscl_hancock_init
    procedure :: solve => rhyme_muscl_hancock_solve_cpu_intensive
    procedure :: solve_memory_intensive => rhyme_muscl_hancock_solve_memory_intensive
  end type muscl_hancock_t

contains

  subroutine rhyme_muscl_hancock_init ( this, samr, log )
    implicit none

    class ( muscl_hancock_t ), intent ( inout ) :: this
    type ( samr_t ), intent ( in ) :: samr
    type ( log_t ), intent ( inout ) :: log

    if ( this%initialized ) then
      call log%warn( 'Try to re-initialize muscl_hancock object' )
      return
    end if

    call this%ws%init( samr, log )

    this%active_axis = samr%base_grid > 1
    this%active_flux = merge( 1, 0, this%active_axis )

    this%initialized = .true.
  end subroutine rhyme_muscl_hancock_init


  subroutine rhyme_muscl_hancock_solve_memory_intensive ( this, box, dx, dt, cfl, ig, irs, sl )
    implicit none

    class ( muscl_hancock_t ), intent ( inout ) :: this
    type ( samr_box_t ), intent ( inout ) :: box
    real ( kind=8 ), intent ( in ) :: dx(3), dt
    type ( cfl_t ), intent ( in ) :: cfl
    type ( ideal_gas_t ), intent ( in ) :: ig
    type ( irs_t ), intent ( inout ) :: irs
    type ( slope_limiter_t ), intent ( in ) :: sl

    integer :: l, b, i, j, k, axis
    integer :: lb(3), ub(3)
    type ( hydro_flux_t ) :: dF( hyid%x:hyid%z )
    type ( hydro_conserved_t ) :: delta, evolved_state

    l = box%level
    b = box%number

    call this%ws%check( box )


    lb = merge( 0, 1, this%active_axis )
    ub = merge( box%dims + 1, 1, this%active_axis )

    do k = lb(3), ub(3)
      do j = lb(2), ub(2)
        do i = lb(1), ub(1)

          if ( this%active_axis( hyid%x ) ) then
            call sl%run( cfl, ig, box%hydro( i-1, j, k ), &
              box%hydro( i, j, k ), box%hydro( i+1, j, k ), delta )
            call ig%half_step_extrapolation( &
              box%hydro( i, j, k ), delta, hyid%x, dx( hyid%x ), dt, &
              this%ws%levels(l)%boxes(b)%UL( i, j, k, hyid%x ), &
              this%ws%levels(l)%boxes(b)%UR( i, j, k, hyid%x ) )
          end if

          if ( this%active_axis( hyid%y ) ) then
            call sl%run( cfl, ig, box%hydro( i, j-1, k ), &
              box%hydro( i, j, k ), box%hydro( i, j+1, k ), delta )
            call ig%half_step_extrapolation( &
              box%hydro( i, j, k ), delta, hyid%y, dx( hyid%y ), dt, &
              this%ws%levels(l)%boxes(b)%UL( i, j, k, hyid%y ), &
              this%ws%levels(l)%boxes(b)%UR( i, j, k, hyid%y ) )
          end if

          if ( this%active_axis( hyid%z ) ) then
            call sl%run( cfl, ig, box%hydro( i, j, k-1 ), &
              box%hydro( i, j, k ), box%hydro( i, j, k+1 ), delta )
            call ig%half_step_extrapolation( &
              box%hydro( i, j, k ), delta, hyid%z, dx( hyid%z ), dt, &
              this%ws%levels(l)%boxes(b)%UL( i, j, k, hyid%z ), &
              this%ws%levels(l)%boxes(b)%UR( i, j, k, hyid%z ) )
          end if

        end do
      end do
    end do

    lb = merge( 0, 1, this%active_axis )
    ub = merge( box%dims, 1, this%active_axis )

    do k = lb(3), ub(3)
      do j = lb(2), ub(2)
        do i = lb(1), ub(1)

          if ( this%active_axis( hyid%x ) ) then
            call rhyme_irs_solve( irs, ig, &
              this%ws%levels(l)%boxes(b)%UR( i, j, k, hyid%x ), &
              this%ws%levels(l)%boxes(b)%UL( i+1, j, k, hyid%x ), &
              dx( hyid%x ), dt, hyid%x, evolved_state )
            call ig%flux_at( evolved_state, hyid%x, &
              this%ws%levels(l)%boxes(b)%FR( i, j, k, hyid%x ) )
          end if

          if ( this%active_axis( hyid%y ) ) then
            call rhyme_irs_solve( irs, ig, &
              this%ws%levels(l)%boxes(b)%UR( i, j, k, hyid%y ), &
              this%ws%levels(l)%boxes(b)%UL( i, j+1, k, hyid%y ), &
              dx( hyid%y ), dt, hyid%y, evolved_state )
            call ig%flux_at( evolved_state, hyid%y, &
              this%ws%levels(l)%boxes(b)%FR( i, j, k, hyid%y ) )
          end if

          if ( this%active_axis( hyid%z ) ) then
            call rhyme_irs_solve( irs, ig, &
              this%ws%levels(l)%boxes(b)%UR( i, j, k, hyid%z ), &
              this%ws%levels(l)%boxes(b)%UL( i, j, k+1, hyid%z ), &
              dx( hyid%z ), dt, hyid%z, evolved_state )
            call ig%flux_at( evolved_state, hyid%z, &
              this%ws%levels(l)%boxes(b)%FR( i, j, k, hyid%z ) )
          end if

        end do
      end do
    end do

    do k = 1, box%dims(3)
      do j = 1, box%dims(2)
        do i = 1, box%dims(1)

          do axis = hyid%x, hyid%z
            dF(axis)%f = this%ws%levels(l)%boxes(b)%FR( i-1, j, k, axis )%f &
              - this%ws%levels(l)%boxes(b)%FR( i, j, k, axis )%f
          end do

          if ( this%active_axis( hyid%x ) ) then
            box%hydro( i, j, k )%u = box%hydro( i, j, k )%u + dt / dx( hyid%x ) * dF( hyid%x )%f
          end if

          if ( this%active_axis( hyid%y ) ) then
            box%hydro( i, j, k )%u = box%hydro( i, j, k )%u + dt / dx( hyid%y ) * dF( hyid%y )%f
          end if

          if ( this%active_axis( hyid%z ) ) then
            box%hydro( i, j, k )%u = box%hydro( i, j, k )%u + dt / dx( hyid%z ) * dF( hyid%z )%f
          end if

        end do
      end do
    end do

  end subroutine rhyme_muscl_hancock_solve_memory_intensive


  subroutine rhyme_muscl_hancock_solve_cpu_intensive ( this, box, dx, dt, cfl, ig, irs, sl )
    implicit none

    class ( muscl_hancock_t ), intent ( inout ) :: this
    type ( samr_box_t ), intent ( inout ) :: box
    real ( kind=8 ), intent ( in ) :: dx(3), dt
    type ( cfl_t ), intent ( in ) :: cfl
    type ( ideal_gas_t ), intent ( in ) :: ig
    type ( irs_t ), intent ( inout ) :: irs
    type ( slope_limiter_t ), intent ( in ) :: sl

    integer :: l, b, i, j, k

    type ( hydro_conserved_t ) :: delta( -1:1, hyid%x:hyid%z )
    type ( hydro_conserved_t ) :: half_step_left( -1:1, hyid%x:hyid%z )
    type ( hydro_conserved_t ) :: half_step_right( -1:1, hyid%x:hyid%z )
    type ( hydro_conserved_t ) :: left_edge_state( hyid%x:hyid%z )
    type ( hydro_conserved_t ) :: right_edge_state( hyid%x:hyid%z )
    type ( hydro_flux_t ) :: left_flux( hyid%x:hyid%z )
    type ( hydro_flux_t ) :: right_flux( hyid%x:hyid%z )
    type ( hydro_flux_t ) :: dF( hyid%x:hyid%z )

    l = box%level
    b = box%number

    call this%ws%check( box )

    do k = 1, box%dims(3)
      do j = 1, box%dims(2)
        do i = 1, box%dims(1)
          if ( this%active_axis( hyid%x ) ) then
            call sl%run( cfl, ig, &
              box%hydro( i-2, j, k ), &
              box%hydro( i-1, j, k ), &
              box%hydro( i  , j, k ), &
              delta( -1, hyid%x ) )

            call ig%half_step_extrapolation( box%hydro( i-1, j, k ), &
              delta( -1, hyid%x ), hyid%x, dx( hyid%x ), dt, &
              half_step_left( -1, hyid%x ), half_step_right( -1, hyid% x) )

            call sl%run( cfl, ig, &
              box%hydro( i-1, j, k ), &
              box%hydro( i  , j, k ), &
              box%hydro( i+1, j, k ), &
              delta( 0, hyid%x ) )

            call ig%half_step_extrapolation( box%hydro( i, j, k ), &
              delta( 0, hyid%x ), hyid%x, dx( hyid%x ), dt, &
              half_step_left( 0, hyid%x ), half_step_right( 0, hyid% x) )

            call sl%run( cfl, ig, &
              box%hydro( i  , j, k ), &
              box%hydro( i+1, j, k ), &
              box%hydro( i+2, j, k ), &
              delta( 1, hyid%x ) )

            call ig%half_step_extrapolation( box%hydro( i+1, j, k ), &
              delta( 1, hyid%x ), hyid%x, dx( hyid%x ), dt, &
              half_step_left( 1, hyid%x ), half_step_right( 1, hyid% x) )

            call rhyme_irs_solve( irs, ig, &
              half_step_right( -1, hyid%x ), half_step_left( 0, hyid%x ), &
              dx( hyid%x ), dt, hyid%x, left_edge_state( hyid%x ) )

            call ig%flux_at( left_edge_state( hyid%x ), hyid%x, left_flux( hyid%x ) )

            call rhyme_irs_solve( irs, ig, &
              half_step_right( 0, hyid%x ), half_step_left( 1, hyid%x ), &
              dx( hyid%x ), dt, hyid%x, right_edge_state( hyid%x ) )

            call ig%flux_at( right_edge_state( hyid%x ), hyid%x, right_flux( hyid%x ) )

            dF( hyid%x )%f = left_flux( hyid%x )%f - right_flux( hyid%x )%f

            this%ws%levels(l)%boxes(b)%UR( i, j, k, 1 )%u = &
              box%hydro( i, j, k )%u + dt / dx( hyid%x ) * dF( hyid%x )%f
          end if
        end do
      end do
    end do

    do k = 1, box%dims(3)
      do j = 1, box%dims(2)
        do i = 1, box%dims(1)
          box%hydro( i, j, k )%u = this%ws%levels(l)%boxes(b)%UR( i, j, k, 1 )%u
        end do
      end do
    end do

  end subroutine rhyme_muscl_hancock_solve_cpu_intensive
end module rhyme_muscl_hancock
