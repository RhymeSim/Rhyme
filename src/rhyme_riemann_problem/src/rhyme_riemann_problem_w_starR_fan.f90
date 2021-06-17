submodule(rhyme_riemann_problem) w_starR_fan_submodule
contains
   pure module function riemann_problem_w_starR_fan(s, axis) result(u)
      implicit none

      type(riemann_problem_solution_t), intent(in) :: s
      integer, intent(in) :: axis
      real(kind=8) :: u(cid%rho:cid%e_tot)

      real(kind=8) :: v(NDIM)

      v = s%right%v
      v(axis) = s%star%u

      call conv_prim_vars_to_cons(s%star%right%fan%rho, v, s%star%p, u)
   end function riemann_problem_w_starR_fan
end submodule w_starR_fan_submodule
