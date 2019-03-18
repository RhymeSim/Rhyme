submodule ( rhyme_irs ) rhyme_irs_guess_p_star_submodule
contains
  pure module function rhyme_irs_guess_p_star ( L, R, dir ) result ( p_star )
    implicit none

    type ( rp_side_t ), intent ( in ) :: L, R
    integer, intent ( in ) :: dir
    real ( kind=8 ) :: p_star

    p_star = ( &
      R%rho * R%cs * L%p &
      + L%rho * L%cs * R%p &
      + R%cs * L%cs * ( L%rho * L%v(dir) - R%rho * R%v(dir) ) &
    ) / ( R%rho * R%cs + L%rho * L%cs )
  end function rhyme_irs_guess_p_star
end submodule rhyme_irs_guess_p_star_submodule
