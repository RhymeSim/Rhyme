module rhyme_riemann_problem
   use rhyme_units
   use rhyme_hydro_base
   use rhyme_thermo_base
   use rhyme_logger

   implicit none

   real(kind=8), private :: gm1 = 0.d0
   real(kind=8), private :: gp1 = 0.d0
   real(kind=8), private :: gm1_gp1 = 0.d0
   real(kind=8), private :: gm1_2g = 0.d0
   real(kind=8), private :: gp1_2g = 0.d0
   real(kind=8), private :: g_inv = 0.d0

   type riemann_problem_t
      real(kind=8), dimension(cid%rho:cid%p) :: w_vacuum = 0d0
   end type riemann_problem_t

   type rp_shock_t
      real(kind=8) :: rho = 0.d0, speed = 0.d0
   end type rp_shock_t

   type rp_fan_t
      real(kind=8) :: rho = 0.d0, cs = 0.d0, speedH = 0.d0, speedT = 0.d0
   end type rp_fan_t

   type rp_star_side_t
      logical :: is_shock = .false.
      type(rp_shock_t) :: shock
      type(rp_fan_t) :: fan
      real(kind=8) :: f, fprime
   end type rp_star_side_t

   type rp_star_t
      real(kind=8) :: u = 0.d0, p = 0.d0
      type(rp_star_side_t) :: left, right
   end type rp_star_t

   type rp_side_t
      real(kind=8) :: rho = 0.d0, v(NDIM) = 0.d0, p = 0.d0
      real(kind=8) :: cs = 0.d0
   end type rp_side_t

   type riemann_problem_solution_t
      type(rp_star_t) :: star
      type(rp_side_t) :: left, right
   end type riemann_problem_solution_t

   interface
      module subroutine rhyme_riemann_problem_init(rp, logger)
         type(riemann_problem_t), intent(inout) :: rp
         type(logger_t), intent(inout) :: logger
      end subroutine rhyme_riemann_problem_init

      pure module subroutine rhyme_riemann_problem_solve(rp, l, r, dx, dt, axis, u)
         type(riemann_problem_t), intent(in) :: rp
         real(kind=8), dimension(cid%rho:cid%e_tot), intent(in) :: l, r
         real(kind=8), intent(in) :: dx, dt
         integer, intent(in) :: axis
         real(kind=8), intent(inout) :: u(cid%rho:cid%e_tot)
      end subroutine rhyme_riemann_problem_solve

      pure module subroutine rhyme_riemann_problem_sampling(solution, axis, dx, dt, u)
         type(riemann_problem_solution_t), intent(in) :: solution
         integer, intent(in) :: axis
         real(kind=8), intent(in) :: dx, dt
         real(kind=8), intent(out) :: u(cid%rho:cid%e_tot)
      end subroutine rhyme_riemann_problem_sampling

      pure module subroutine rhyme_riemann_problem_iterate(rp, solution, axis)
         type(riemann_problem_t), intent(in) :: rp
         type(riemann_problem_solution_t), intent(inout) :: solution
         integer, intent(in) :: axis
      end subroutine rhyme_riemann_problem_iterate

      pure module function riemann_problem_w_k(s) result(u)
         type(rp_side_t), intent(in) :: s
         real(kind=8) :: u(cid%rho:cid%e_tot)
      end function riemann_problem_w_k

      pure module function riemann_problem_w_starl_sho(s, axis) result(u)
         type(riemann_problem_solution_t), intent(in) :: s
         integer, intent(in) :: axis
         real(kind=8) :: u(cid%rho:cid%e_tot)
      end function riemann_problem_w_starl_sho

      pure module function riemann_problem_w_starr_sho(s, axis) result(u)
         type(riemann_problem_solution_t), intent(in) :: s
         integer, intent(in) :: axis
         real(kind=8) :: u(cid%rho:cid%e_tot)
      end function riemann_problem_w_starr_sho

      pure module function riemann_problem_w_kfan(s, dxdt, axis, is_right) result(u)
         type(rp_side_t), intent(in) :: s
         real(kind=8), intent(in) :: dxdt
         integer, intent(in) :: axis
         logical, intent(in) :: is_right
         real(kind=8) :: u(cid%rho:cid%e_tot)
      end function riemann_problem_w_kfan

      pure module function riemann_problem_w_starl_fan(s, axis) result(u)
         type(riemann_problem_solution_t), intent(in) :: s
         integer, intent(in) :: axis
         real(kind=8) :: u(cid%rho:cid%e_tot)
      end function riemann_problem_w_starl_fan

      pure module function riemann_problem_w_starr_fan(s, axis) result(u)
         type(riemann_problem_solution_t), intent(in) :: s
         integer, intent(in) :: axis
         real(kind=8) :: u(cid%rho:cid%e_tot)
      end function riemann_problem_w_starr_fan

      pure module function riemann_problem_rp_side_to_cons(s) result(u)
         type(rp_side_t), intent(in) :: s
         real(kind=8) :: u(cid%rho:cid%e_tot)
      end function riemann_problem_rp_side_to_cons
   end interface
end module rhyme_riemann_problem
