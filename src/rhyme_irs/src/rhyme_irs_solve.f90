submodule ( rhyme_irs ) irs_solve_submodule
contains
  pure module subroutine rhyme_irs_solve ( cfg, ig, L, R, dir, solution )
    implicit none

    class ( irs_t ), intent ( in ) :: cfg
    type ( ideal_gas_t ), intent ( in ) :: ig
    type ( hydro_conserved_t ), intent ( in ) :: L, R
    integer, intent ( in ) :: dir
    type ( riemann_problem_solution_t ), intent ( out ) :: solution

    real ( kind=8 ) :: pL, pR, vL, vR, csL, csR, fL, fR, fprimeL, fprimeR
    real ( kind=8 ) :: ps_pL, ps_pR
    real ( kind=8 ) :: p_star, p_star_prev

    integer :: i

    pL = ig%p(L)
    pR = ig%p(R)

    csL = ig%Cs(L)
    csR = ig%Cs(R)

    vL = L%u( hyid%rho_vel(dir) ) / L%u(hyid%rho)
    vR = R%u( hyid%rho_vel(dir) ) / R%u(hyid%rho)

    p_star = max( &
      rhyme_irs_guess_p_star( &
        R%u(hyid%rho), csR, vR, pR, L%u(hyid%rho), csL, vL, pL &
      ), cfg%tolerance )

    p_star_prev = p_star

    do i = 1, cfg%n_iteration
      call rhyme_irs_nonlinear_waves( &
        ig, L%u(hyid%rho), pL, p_star, fL, fprimeL )
      call rhyme_irs_nonlinear_waves( &
        ig, R%u(hyid%rho), pR, p_star, fR, fprimeR )

      p_star = p_star - ( fL + fR + (vR - vL) ) / ( fprimeL + fprimeR )

      if ( abs( p_star - p_star_prev ) &
        / ( .5d0 * (p_star + p_star_prev) ) < cfg%tolerance ) exit
      p_star_prev = p_star
    end do

    solution%star%p = p_star
    solution%star%u = 0.5d0 * ( (vR + vL) + (fR - fL) )

    ps_pL = solution%star%p / pL
    ps_pR = solution%star%p / pR


    if ( p_star > pL ) then
      solution%star%left%is_shock = .true.
      solution%star%left%shock%rho = L%u(hyid%rho) * (ig%gm1_gp1 + ps_pL) / (ig%gm1_gp1 * ps_pL + 1.0)
      solution%star%left%shock%speed = vL - csL * sqrt(ig%gp1_2g * ps_pL + ig%gm1_2g)
    else
      solution%star%left%is_shock = .false.
      solution%star%left%fan%rho = L%u(hyid%rho) * ps_pL**real(ig%g_inv, kind=8)
      solution%star%left%fan%cs = csL * ps_pL**real(ig%gm1_2g, kind=8)
      solution%star%left%fan%speedH = vL - csL
      solution%star%left%fan%speedT = solution%star%u - solution%star%left%fan%cs
    end if

    if ( p_star > pR ) then
      solution%star%right%is_shock = .true.
      solution%star%right%shock%rho = R%u(hyid%rho) * (ig%gm1_gp1 + ps_pR) / (ig%gm1_gp1 * ps_pR + 1.0)
      solution%star%right%shock%speed = vR + csR * sqrt(ig%gp1_2g * ps_pR + ig%gm1_2g)
    else
      solution%star%right%is_shock = .false.
      solution%star%right%fan%rho = R%u(hyid%rho) * ps_pR**real(ig%g_inv, kind=8)
      solution%star%right%fan%cs = csR * ps_pR**real(ig%gm1_2g, kind=8)
      solution%star%right%fan%speedH = vR + csR
      solution%star%right%fan%speedT = solution%star%u + solution%star%right%fan%cs
    end if
  end subroutine rhyme_irs_solve
end submodule irs_solve_submodule
