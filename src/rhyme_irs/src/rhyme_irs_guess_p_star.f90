submodule ( rhyme_irs ) rhyme_irs_guess_p_star_submodule
contains
  pure module function rhyme_irs_guess_p_star ( ig, L, R, dir, tol ) result ( p_star )
    implicit none

    type ( ideal_gas_t ), intent ( in ) :: ig
    type ( rp_side_t ), intent ( in ) :: L, R
    integer, intent ( in ) :: dir
    real ( kind=8 ), intent ( in ) :: tol
    real ( kind=8 ) :: p_star(5)

    real ( kind=8 ) :: L_g, R_g

    ! RAMSES guess
    p_star(1) = ( &
      R%rho * R%cs * L%p &
      + L%rho * L%cs * R%p &
      + R%cs * L%cs * ( L%rho * L%v(dir) - R%rho * R%v(dir) ) &
    ) / ( R%rho * R%cs + L%rho * L%cs )
    if ( p_star(1) < 0.d0 ) p_star(1) = tol

    ! Two rarefaction approximation
    p_star(2) = ( &
      ( &
        L%cs + R%cs - .5d0 * ig%gm1 * ( R%v(dir) - L%v(dir) ) &
      ) / ( &
        ( L%cs / L%p )**ig%gm1_2g + ( R%cs / R%p )**ig%gm1_2g &
      ) &
    )**real( 1.d0 / ig%gm1_2g, kind=8 )
    if ( p_star(2) < 0.d0 ) p_star(2) = tol

    ! Two shock approximation
    L_g = rhyme_irs_guess_p_star_g_K( ig, L )
    R_g = rhyme_irs_guess_p_star_g_K( ig, R )
    p_star(3) = ( L_g * L%p + R_g * R%p - ( R%v(dir) - L%v(dir) ) ) / ( L_g + R_g )
    p_star(3) = max( p_star(3) , tol )

    ! p_PV
    p_star(4) = .5d0 * ( L%p + R%p ) &
      - .125d0 * ( R%v(dir) - L%v(dir) ) * ( L%rho + R%rho ) * ( L%cs + R%cs )
    p_star(4) = max( p_star(4), tol )

    ! Arithmetic mean
    p_star(5) = .5d0 * ( L%p + R%p )
  end function rhyme_irs_guess_p_star


  real ( kind=8 ) pure function rhyme_irs_guess_p_star_g_K ( ig, s ) result ( g_K )
    implicit none

    type ( ideal_gas_t ), intent ( in ) :: ig
    type ( rp_side_t ), intent ( in ) :: s

    g_K = sqrt( ( 2.d0 / ( ig%gp1 * s%rho ) ) / ( s%p + ig%gm1_gp1 * s%p ) )
  end function rhyme_irs_guess_p_star_g_K
end submodule rhyme_irs_guess_p_star_submodule
