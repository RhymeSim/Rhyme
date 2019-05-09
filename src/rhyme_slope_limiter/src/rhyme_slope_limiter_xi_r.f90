submodule ( rhyme_slope_limiter ) rhyme_sl_xi_r_smod
contains
  pure module function rhyme_slope_limiter_xi_r ( sl, cfl, r ) result ( xi_r )
    implicit none

    class ( slope_limiter_t ), intent ( in ) :: sl
    type ( cfl_t ), intent ( in ) :: cfl
    real ( kind=8 ), intent ( in ) :: r

    real ( kind=8 ) :: xi_r

    xi_r = 2.d0 * ( 2.d0 / ( 1.d0 - cfl%courant_number ) ) / &
    ( 1.d0 - sl%w + ( 1.d0 + sl%w ) * r )
  end function rhyme_slope_limiter_xi_r
end submodule rhyme_sl_xi_r_smod
