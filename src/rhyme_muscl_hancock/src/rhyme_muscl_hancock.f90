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
    procedure :: solve => rhyme_muscl_hancock_solve_memory_intensive
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
    this%active_flux = merge ( 1, 0, this%active_axis )

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

    integer :: l, b, i, j, k, uid
    integer :: lb(3), ub(3)
    type ( hydro_flux_t ) :: dF( hyid%x:hyid%z )

    type ( hydro_conserved_t ) :: Delta, evolved_hydro_state

    l = box%level
    b = box%number

    call this%ws%check( box )

    lb = merge ( 0, 1, this%active_axis )
    ub = merge ( box%dims + 1, 1, this%active_axis )

    do k = lb(3), ub(3)
      do j = lb(2), ub(2)
        do i = lb(1), ub(1)
          if ( this%active_axis( hyid%x ) ) then
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
          if ( this%active_axis( hyid%y ) ) then
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
          if ( this%active_axis( hyid%z ) ) then
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


    lb = merge ( 0, 1, this%active_axis )
    ub = merge ( box%dims, 1, this%active_axis )

    do k = lb(3), ub(3)
      do j = lb(2), ub(2)
        do i = lb(1), ub(1)
          if ( this%active_axis(hyid%x) ) then
            call rhyme_irs_solve( irs, ig, &
              this%ws%levels(l)%boxes(b)%UR( i  , j, k, hyid%x ), &
              this%ws%levels(l)%boxes(b)%UL( i+1, j, k, hyid%x ), &
              0.d0, dt, hyid%x, evolved_hydro_state &
            )
            call ig%flux_at( evolved_hydro_state, hyid%x, &
              this%ws%levels(l)%boxes(b)%FR( i, j, k, hyid%x ) )
          end if
          if ( this%active_axis(hyid%y) ) then
            call rhyme_irs_solve( irs, ig, &
              this%ws%levels(l)%boxes(b)%UR( i, j  , k, hyid%y ), &
              this%ws%levels(l)%boxes(b)%UL( i, j+1, k, hyid%y ), &
              0.d0, dt, hyid%y, evolved_hydro_state &
            )
            call ig%flux_at( evolved_hydro_state, hyid%y, &
              this%ws%levels(l)%boxes(b)%FR( i, j, k, hyid%y ) )
          end if
          if ( this%active_axis(hyid%z) ) then
            call rhyme_irs_solve( irs, ig, &
              this%ws%levels(l)%boxes(b)%UR( i, j, k  , hyid%z ), &
              this%ws%levels(l)%boxes(b)%UL( i, j, k+1, hyid%z ), &
              0.d0, dt, hyid%z, evolved_hydro_state &
            )
            call ig%flux_at( evolved_hydro_state, hyid%z, &
              this%ws%levels(l)%boxes(b)%FR( i, j, k, hyid%z ) )
          end if
        end do
      end do
    end do

    do k = 1, box%dims(3)
      do j = 1, box%dims(2)
        do i = 1, box%dims(1)

          do uid = hyid%rho, hyid%e_tot
            dF(:)%f(uid) = this%ws%levels(l)%boxes(b)%FR( i-1,j,k,:)%f(uid) &
              - this%ws%levels(l)%boxes(b)%FR(i,j,k,:)%f(uid)
          end do

          if ( this%active_axis( hyid%x ) ) then
            box%hydro(i,j,k)%u = box%hydro(i,j,k)%u + dt / dx(hyid%x) * dF(hyid%x)%f
          end if

          if ( this%active_axis( hyid%y ) ) then
            box%hydro(i,j,k)%u = box%hydro(i,j,k)%u + dt / dx(hyid%y) * dF(hyid%y)%f
          end if

          if ( this%active_axis( hyid%z ) ) then
            box%hydro(i,j,k)%u = box%hydro(i,j,k)%u + dt / dx(hyid%z) * dF(hyid%z)%f
          end if
        end do
      end do
    end do
  end subroutine rhyme_muscl_hancock_solve_memory_intensive
end module rhyme_muscl_hancock
