submodule ( rhyme_chemistry ) one_over_mu_smod
contains
  pure module function rhyme_chemistry_one_over_mu ( chemistry, X, Y, f ) &
    ! NB: currently it's working only for 3 species, HI, HeI, HeII
    result ( one__mu )
    implicit none

    type ( chemistry_t ), intent ( in ) :: chemistry
    real ( kind=8 ), intent ( in ) :: X, Y, f(3)
    real ( kind=8 ) :: one__mu

    real ( kind=8 ) :: x_red, y_red

    x_red = X / chemistry%amu%H
    y_red = Y / chemistry%amu%He

    one__mu = x_red + y_red &
      + ( 1d0 - f(1) ) * x_red &
      + ( 1d0 - f(2) ) * y_red &
      + 2 * ( 1d0 - f(2) - f(3) )  * y_red
  end function rhyme_chemistry_one_over_mu
end submodule one_over_mu_smod
