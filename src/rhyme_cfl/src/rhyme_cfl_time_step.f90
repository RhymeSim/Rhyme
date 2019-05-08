submodule ( rhyme_cfl ) rhyme_cfl_time_step_smod
contains
  pure module function rhyme_cfl_time_step ( cfl, ig, samr ) result ( dt )
    use rhyme_ideal_gas

    implicit none

    class ( cfl_t ), intent(in) :: cfl
    type ( ideal_gas_t ), intent(in) :: ig
    type ( samr_t ), intent(in) :: samr
    real ( kind=8 ) :: dt

    integer :: i, j, k
    real ( kind=8 ) :: max_u, u

    max_u = 0.d0

    do k = 1, samr%levels(0)%boxes(1)%dims(3)
      do j = 1, samr%levels(0)%boxes(1)%dims(2)
        do i = 1, samr%levels(0)%boxes(1)%dims(1)

          u = calc_u( samr%levels(0)%boxes(1)%hydro(i,j,k) )
          if ( u > max_u ) max_u = u

        end do
      end do
    end do

    dt = cfl%courant_number * minval( samr%levels(0)%dx ) / max_u

  contains
    pure real( kind=8 ) function calc_u ( U ) result ( v )
      implicit none

      type ( hydro_conserved_t ), intent ( in ) :: U

      v = sqrt( sum( U%u( hyid%rho_u:hyid%rho_w )**2 ) / U%u(hyid%rho)**2 ) &
        + ig%cs( U )
    end function calc_u
  end function rhyme_cfl_time_step
end submodule rhyme_cfl_time_step_smod
