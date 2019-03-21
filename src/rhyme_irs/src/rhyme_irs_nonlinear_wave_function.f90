submodule ( rhyme_irs ) rhyme_irs_nonlinear_wave_function_submodule
contains
  pure module subroutine rhyme_irs_nonlinear_wave_function ( ig, state, p, star )
    implicit none

    type ( ideal_gas_t ), intent ( in ) :: ig
    type ( rp_side_t ), intent ( in ) :: state
    real ( kind=8 ), intent ( in ) :: p
    type ( rp_star_side_t ), intent ( inout ) :: star

    real ( kind=8 ) :: factor
    real ( kind=8 ) :: Ak, Bk

    Ak = 2.d0 / ( ig%gp1 * state%rho )
    Bk = ig%gm1 * state%p / ig%gp1

    if ( p > state%p ) then
      factor = sqrt( AK / (Bk + p) )
      star%f = ( p - state%p ) * factor
      star%fprime = factor * ( 1.d0 - ( p - state%p ) / ( 2.d0 * (Bk + p) ) )
    else
      star%f = 2.d0 * state%cs / ig%gm1 * ( (p / state%p)**ig%gm1_2g - 1.d0 )
      star%fprime =  1.d0 / ( state%rho * state%cs ) * ( p / state%p )**( -ig%gp1_2g )
    end if
  end subroutine rhyme_irs_nonlinear_wave_function
end submodule  rhyme_irs_nonlinear_wave_function_submodule
