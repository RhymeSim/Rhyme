submodule ( rhyme_chemistry ) one_over_mu_smod
contains
  pure module function rhyme_chemistry_one_over_mu ( chemistry, X, Y, f ) &
    result ( one__mu )
    implicit none

    type ( chemistry_t ), intent ( in ) :: chemistry
    real ( kind=8 ), intent ( in ) :: X, Y, f( NSPE )
    real ( kind=8 ) :: one__mu

    ! TODO: currently it's working only for 3 species, HI, HeI, HeII
    one__mu = X * ( 1.d0 + f(1) ) / chemistry%amu%H + &
      Y * ( 1.d0 + f(2) + 2 * f(3) ) / chemistry%amu%He
  end function rhyme_chemistry_one_over_mu
end submodule one_over_mu_smod
