submodule ( rhyme_chemistry ) rhyme_chemistry_one_over_mu_smod
contains
  pure module function rhyme_chemistry_one_over_mu ( chemistry, X, Y, f ) result ( one__mu )
    implicit none

    type ( chemistry_t ), intent ( in ) :: chemistry
    real ( kind=8 ), intent ( in ) :: X, Y, f(3)
    real ( kind=8 ) :: one__mu

    one__mu = X * ( 1.d0 + f(1) ) / chemistry%amu%H + &
      Y * ( 1.d0 + f(2) + 2 * f(3) ) / chemistry%amu%He
  end function rhyme_chemistry_one_over_mu
end submodule rhyme_chemistry_one_over_mu_smod
