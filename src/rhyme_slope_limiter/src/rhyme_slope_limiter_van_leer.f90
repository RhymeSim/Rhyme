submodule ( rhyme_slope_limiter ) rhyme_sl_van_leer_smod
contains
  pure module subroutine rhyme_slope_limiter_van_leer ( sl, UL, U, UR, delta )
    implicit none

    class ( slope_limiter_t ), intent ( in ) :: sl
    type ( hydro_conserved_t ), intent ( in ) :: UL, U, UR
    type ( hydro_conserved_t ), intent ( out ) :: delta

    real ( kind=8 ), dimension ( hyid%rho:hyid%e_tot ) :: d_L, d_R, d, r
    integer :: i

    d_L = U%u - UL%u
    d_R = UR%u - U%u

    d = .5d0 * ( 1.d0 + sl%w ) * d_L + .5d0 * ( 1.d0 + sl%w ) * d_R

    call rhyme_slope_limiter_r( UL, U, UR, r )

    do i = hyid%rho, hyid%e_tot
      if ( r(i) > 0.d0 ) then
        delta%u(i) = min( &
          2.d0 * r(i) / ( 1.d0 + r(i) ), rhyme_slope_limiter_xi_R( sl, r(i) ) &
        ) * d(i)
      else
        delta%u(i) = 0.d0
      end if
    end do
  end subroutine rhyme_slope_limiter_van_leer
end submodule rhyme_sl_van_leer_smod
