submodule ( rhyme_muscl_hancock ) rhyme_mh_solve_cpu_intensive_submodule
contains
  pure module subroutine rhyme_muscl_hancock_solve_cpu_intensive ( &
    this, box, dx, dt, cfl, ig, irs, sl )
    implicit none

    class ( muscl_hancock_t ), intent ( inout ) :: this
    type ( samr_box_t ), intent ( inout ) :: box
    real ( kind=8 ), intent ( in ) :: dx(3), dt
    type ( cfl_t ), intent ( in ) :: cfl
    type ( ideal_gas_t ), intent ( in ) :: ig
    type ( irs_t ), intent ( inout ) :: irs
    type ( slope_limiter_t ), intent ( in ) :: sl

    type ( hydro_conserved_t ) :: delta( -1:1, hyid%x:hyid%z )
    type ( hydro_conserved_t ) :: half_step_left( -1:1, hyid%x:hyid%z )
    type ( hydro_conserved_t ) :: half_step_right( -1:1, hyid%x:hyid%z )
    type ( hydro_conserved_t ) :: edge_state( -1:0, hyid%x:hyid%z )
    type ( hydro_flux_t ) :: flux( -1:0, hyid%x:hyid%z )
    type ( hydro_flux_t ) :: dF( hyid%x:hyid%z )

    integer, parameter :: axes(3) = [ hyid%x, hyid%y, hyid%z ]

    integer :: axis, idx
    integer :: l, b, i, j, k
    integer :: li(3), ci(3), ri(3), ub(3)

    l = box%level
    b = box%number

    call this%ws%check( box )


    do k = 1, box%dims(3)
      do j = 1, box%dims(2)
        do i = 1, box%dims(1)

          this%ws%levels(l)%boxes(b)%UR( i, j, k, 1 )%u = box%hydro( i, j, k )%u

          do axis = hyid%x, hyid%z

            if ( .not. this%active_axis( axis ) ) cycle

            do idx = -1, 1
              li = merge( [i, j, k] + idx - 1, [i, j, k], axes .eq. axis )
              ci = merge( [i, j, k] + idx    , [i, j, k], axes .eq. axis )
              ri = merge( [i, j, k] + idx + 1, [i, j, k], axes .eq. axis )

              call sl%run( cfl, ig, &
                box%hydro( li(1), li(2), li(3) ), &
                box%hydro( ci(1), ci(2), ci(3) ), &
                box%hydro( ri(1), ri(2), ri(3) ), &
                delta( idx, axis ) )

              call ig%half_step_extrapolation( &
                box%hydro( ci(1), ci(2), ci(3) ), &
                delta( idx, axis ), axis, dx( axis ), dt, &
                half_step_left( idx, axis ), half_step_right( idx, axis ) )
            end do

            do idx = -1, 0
              call rhyme_irs_solve( irs, ig, &
                half_step_right( idx, axis ), &
                half_step_left( idx+1, axis ), &
                0.d0, dt, axis, &
                edge_state( idx, axis ) )

              call ig%flux_at( edge_state( idx, axis ), axis, flux( idx, axis ) )
            end do

            dF( axis )%f = flux( -1, axis )%f - flux( 0, axis )%f

            this%ws%levels(l)%boxes(b)%UR( i, j, k, 1 )%u = &
              this%ws%levels(l)%boxes(b)%UR( i, j, k, 1 )%u &
              + dt / dx( axis ) * dF( axis )%f
          end do

        end do
      end do
    end do

    ub = box%dims
    box%hydro(1:ub(1),1:ub(2),1:ub(3)) = &
      this%ws%levels(l)%boxes(b)%UR(1:ub(1),1:ub(2),1:ub(3),1)
  end subroutine rhyme_muscl_hancock_solve_cpu_intensive

end submodule rhyme_mh_solve_cpu_intensive_submodule
