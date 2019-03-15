submodule ( rhyme_irs ) irs_guess_p_star_submodule
contains
  pure module function rhyme_irs_guess_p_star ( &
    rhor, csr, ur, pr, rhol, csl, ul, pl ) result ( p_star )
    implicit none

    real ( kind=8 ), intent ( in ) :: rhor, csr, ur, pr, rhol, csl, ul, pl
    real ( kind=8 ) :: p_star

    p_star = ( &
      rhor * csr * pl + rhol * csl * pr + csr * csl * ( rhol * ul - rhor * ur ) &
    ) / ( rhor * csr + rhol * csl )
  end function rhyme_irs_guess_p_star
end submodule irs_guess_p_star_submodule
