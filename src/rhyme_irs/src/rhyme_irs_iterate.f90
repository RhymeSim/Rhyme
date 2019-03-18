submodule ( rhyme_irs ) rhyme_irs_iterate_submodule
contains
  pure module subroutine rhyme_irs_iterate ( cfg, ig, L, R, dir, solution )
    implicit none

    class ( irs_t ), intent ( in ) :: cfg
    type ( ideal_gas_t ), intent ( in ) :: ig
    type ( hydro_conserved_t ), intent ( in ) :: L, R
    integer, intent ( in ) :: dir
    type ( riemann_problem_solution_t ), intent ( out ) :: solution

    real ( kind=8 ) :: ps_pL, ps_pR
    real ( kind=8 ) :: p_star_prev

    integer :: i

    ! Filling left and right states
    solution%left%rho = L%u(hyid%rho)
    solution%right%rho = R%u(hyid%rho)

    solution%left%v = L%u( hyid%rho_u:hyid%rho_w ) / L%u(hyid%rho)
    solution%right%v = R%u( hyid%rho_u:hyid%rho_w ) / R%u(hyid%rho)

    solution%left%p = ig%p(L)
    solution%right%p = ig%p(R)

    solution%left%cs = ig%Cs(L)
    solution%right%cs = ig%Cs(R)


    solution%star%p = max( &
      rhyme_irs_guess_p_star( solution%left, solution%right, dir ), &
      cfg%tolerance &
    )

    p_star_prev = solution%star%p

    do i = 1, cfg%n_iteration
      call rhyme_irs_nonlinear_wave_function( &
        ig, solution%left, solution%star%p, solution%star%left )
      call rhyme_irs_nonlinear_wave_function( &
        ig, solution%right, solution%star%p, solution%star%right )

      solution%star%p = solution%star%p - ( &
        solution%star%left%f &
        + solution%star%right%f &
        + ( solution%right%v(dir) - solution%left%v(dir) ) &
      ) / ( solution%star%left%fprime + solution%star%right%fprime )

      if ( 2 * abs( solution%star%p - p_star_prev ) &
        / ( solution%star%p + p_star_prev ) < cfg%tolerance ) exit

      p_star_prev = solution%star%p
    end do

    solution%star%u = 0.5d0 * ( &
      ( solution%right%v(dir) + solution%left%v(dir) ) &
      + ( solution%star%right%f - solution%star%left%f ) &
    )

    ps_pL = solution%star%p / solution%left%p
    ps_pR = solution%star%p / solution%right%p


    if ( solution%star%p > solution%left%p ) then
      solution%star%left%is_shock = .true.
      solution%star%left%shock%rho = solution%left%rho * (ig%gm1_gp1 + ps_pL) / (ig%gm1_gp1 * ps_pL + 1.0)
      solution%star%left%shock%speed = solution%left%v(dir) - solution%left%cs * sqrt(ig%gp1_2g * ps_pL + ig%gm1_2g)
    else
      solution%star%left%is_shock = .false.
      solution%star%left%fan%rho = solution%left%rho * ps_pL**real(ig%g_inv, kind=8)
      solution%star%left%fan%cs = solution%left%cs * ps_pL**real(ig%gm1_2g, kind=8)
      solution%star%left%fan%speedH = solution%left%v(dir) - solution%left%cs
      solution%star%left%fan%speedT = solution%star%u - solution%star%left%fan%cs
    end if

    if ( solution%star%p > solution%right%p ) then
      solution%star%right%is_shock = .true.
      solution%star%right%shock%rho = solution%right%rho * (ig%gm1_gp1 + ps_pR) / (ig%gm1_gp1 * ps_pR + 1.0)
      solution%star%right%shock%speed = solution%right%v(dir) + solution%right%cs * sqrt(ig%gp1_2g * ps_pR + ig%gm1_2g)
    else
      solution%star%right%is_shock = .false.
      solution%star%right%fan%rho = solution%right%rho * ps_pR**real(ig%g_inv, kind=8)
      solution%star%right%fan%cs = solution%right%cs * ps_pR**real(ig%gm1_2g, kind=8)
      solution%star%right%fan%speedH = solution%right%v(dir) + solution%right%cs
      solution%star%right%fan%speedT = solution%star%u + solution%star%right%fan%cs
    end if
  end subroutine rhyme_irs_iterate
end submodule rhyme_irs_iterate_submodule
