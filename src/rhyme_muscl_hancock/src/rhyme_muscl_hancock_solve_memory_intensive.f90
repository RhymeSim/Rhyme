submodule ( rhyme_muscl_hancock ) rhyme_mh_solve_memory_intensive_submodule
contains
  pure module subroutine rhyme_muscl_hancock_solve_memory_intensive ( &
    this, box, dx, dt, cfl, ig, irs, sl )
    implicit none

    class ( muscl_hancock_t ), intent ( inout ) :: this
    type ( samr_box_t ), intent ( inout ) :: box
    real ( kind=8 ), intent ( in ) :: dx(3), dt
    type ( cfl_t ), intent ( in ) :: cfl
    type ( ideal_gas_t ), intent ( in ) :: ig
    type ( irs_t ), intent ( inout ) :: irs
    type ( slope_limiter_t ), intent ( in ) :: sl

    integer :: l, b, i, j, k
    integer :: lb(3), ub(3)
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
              0.d0, dt, hyid%x, evolved_state )
            call ig%flux_at( evolved_state, hyid%x, &
              this%ws%levels(l)%boxes(b)%FR( i, j, k, hyid%x ) )
          end if

          if ( this%active_axis( hyid%y ) ) then
            call rhyme_irs_solve( irs, ig, &
              this%ws%levels(l)%boxes(b)%UR( i, j, k, hyid%y ), &
              this%ws%levels(l)%boxes(b)%UL( i, j+1, k, hyid%y ), &
              0.d0, dt, hyid%y, evolved_state )
            call ig%flux_at( evolved_state, hyid%y, &
              this%ws%levels(l)%boxes(b)%FR( i, j, k, hyid%y ) )
          end if

          if ( this%active_axis( hyid%z ) ) then
            call rhyme_irs_solve( irs, ig, &
              this%ws%levels(l)%boxes(b)%UR( i, j, k, hyid%z ), &
              this%ws%levels(l)%boxes(b)%UL( i, j, k+1, hyid%z ), &
              0.d0, dt, hyid%z, evolved_state )
            call ig%flux_at( evolved_state, hyid%z, &
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
              this%active_flux( hyid%x ) * dt / dx( hyid%x ) &
              * ( &
                this%ws%levels(l)%boxes(b)%FR( i-1, j, k, hyid%x )%f &
                - this%ws%levels(l)%boxes(b)%FR( i, j, k, hyid%x )%f &
              ) &
              + &
              this%active_flux( hyid%y ) * dt / dx( hyid%y ) &
              * ( &
                this%ws%levels(l)%boxes(b)%FR( i, j-1, k, hyid%y )%f &
                - this%ws%levels(l)%boxes(b)%FR( i, j, k, hyid%y )%f &
              ) &
              + &
              this%active_flux( hyid%z ) * dt / dx( hyid%z ) &
              * ( &
                this%ws%levels(l)%boxes(b)%FR( i, j, k-1, hyid%z )%f &
                - this%ws%levels(l)%boxes(b)%FR( i, j, k, hyid%z )%f &
              ) &
            )
        end do
      end do
    end do
  end subroutine rhyme_muscl_hancock_solve_memory_intensive

end submodule rhyme_mh_solve_memory_intensive_submodule
