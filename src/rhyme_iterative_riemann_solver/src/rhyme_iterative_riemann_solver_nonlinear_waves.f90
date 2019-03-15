submodule ( rhyme_iterative_riemann_solver ) irs_nonlinear_waves_submodule
contains
  pure subroutine rhyme_iterative_riemann_solver_nonlinear_waves ( ig, rho, p_k, p, f, fprime)
    implicit none

    type ( ideal_gas_t ), intent ( in ) :: ig
    real ( kind=8 ), intent ( in ) :: rho, p_k, p
    real ( kind=8 ), intent ( out ) :: f, fprime

    real(kind=8) :: factor, cs
    real(kind=8) :: Ak, Bk

    Ak = 2.d0 / (ig%gp1 * rho)
    Bk = ig%gm1 * p_k / ig%gp1

    if (p > p_k) then
      factor = sqrt(AK / (Bk + p))
      f = (p - p_k) * factor
      fprime = factor * (1.d0 - (p - p_k) / (2.d0 * (Bk + p)))
    else
      cs = sqrt(ig%gamma * p_k / rho)

      f = 2.d0 * cs / ig%gm1 * ((p / p_k)**ig%gm1_2g - 1.d0)
      fprime =  1.d0 / (rho * cs) * (p / p_k)**(-ig%gp1_2g)
    end if
  end subroutine rhyme_iterative_riemann_solver_nonlinear_waves
end submodule  irs_nonlinear_waves_submodule
