submodule ( rhyme_chemistry ) mu_smod
contains
  pure module function rhyme_chemistry_mu ( chemistry, X, Y, f ) result ( mu )
    implicit none

    type ( chemistry_t ), intent ( in ) :: chemistry
    real ( kind=8 ), intent ( in ) :: X, Y, f(3)
    real ( kind=8 ) :: mu

    mu = 1.d0 / rhyme_chemistry_one_over_mu( chemistry, X, Y, f )
  end function rhyme_chemistry_mu
end submodule mu_smod
