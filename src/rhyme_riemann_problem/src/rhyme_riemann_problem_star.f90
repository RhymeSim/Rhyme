submodule(rhyme_riemann_problem) star_submodule
contains
   module subroutine rhyme_riemann_problem_star(rp, sol, axis)
      implicit none

      type(riemann_problem_t), intent(in) :: rp
      type(riemann_problem_solution_t), intent(inout) :: sol
      integer, intent(in) :: axis

      real(kind=8) :: ps_pl, ps_pr

      ! Riemann Solver
      select case (rp%solver)
      case (rpid%exact_rs)
         sol%star%p = &
            rhyme_irs_exec( &
            rp%irs, &
            sol%left%rho, sol%left%v, sol%left%p, sol%left%cs, sol%star%left%f, &
            sol%right%rho, sol%right%v, sol%right%p, sol%right%cs, sol%star%right%f, &
            axis)
         ! case (rpid%deep_rs)
         !    sol%star%p = &
         !       rhyme_deep_rs_exec( &
         !       rp%drs, &
         !       sol%left%rho, sol%left%p, &
         !       sol%left%rho, sol%left%p, &
         !       sol%right%v(axis) - sol%left%v(axis))
      case default
         return
      end select

      if (sol%star%p < rp%w_vacuum(cid%p)) sol%star%p = rp%w_vacuum(cid%p)

      sol%star%u = &
         0.5d0*( &
         (sol%right%v(axis) + sol%left%v(axis)) &
         + (sol%star%right%f - sol%star%left%f) &
         )

      ps_pl = sol%star%p/sol%left%p
      ps_pr = sol%star%p/sol%right%p

      if (sol%star%p > sol%left%p) then
         sol%star%left%is_shock = .true.
         sol%star%left%shock%rho = sol%left%rho*(gm1_gp1 + ps_pl)/(gm1_gp1*ps_pl + 1d0)
         sol%star%left%shock%speed = sol%left%v(axis) - sol%left%cs*sqrt(gp1_2g*ps_pl + gm1_2g)
      else
         sol%star%left%is_shock = .false.
         sol%star%left%fan%rho = sol%left%rho*ps_pl**real(g_inv, kind=8)
         sol%star%left%fan%cs = sol%left%cs*ps_pl**real(gm1_2g, kind=8)
         sol%star%left%fan%speedH = sol%left%v(axis) - sol%left%cs
         sol%star%left%fan%speedT = sol%star%u - sol%star%left%fan%cs
      end if

      if (sol%star%p > sol%right%p) then
         sol%star%right%is_shock = .true.
         sol%star%right%shock%rho = sol%right%rho*(gm1_gp1 + ps_pr)/(gm1_gp1*ps_pr + 1d0)
         sol%star%right%shock%speed = sol%right%v(axis) + sol%right%cs*sqrt(gp1_2g*ps_pr + gm1_2g)
      else
         sol%star%right%is_shock = .false.
         sol%star%right%fan%rho = sol%right%rho*ps_pr**real(g_inv, kind=8)
         sol%star%right%fan%cs = sol%right%cs*ps_pr**real(gm1_2g, kind=8)
         sol%star%right%fan%speedH = sol%right%v(axis) + sol%right%cs
         sol%star%right%fan%speedT = sol%star%u + sol%star%right%fan%cs
      end if
   end subroutine rhyme_riemann_problem_star
end submodule star_submodule
