submodule(rhyme_riemann_problem) w_k_submodule
contains
   pure module function riemann_problem_w_k(s) result(u)
      implicit none

      type(rp_side_t), intent(in) :: s
      real(kind=8) :: u(cid%rho:cid%e_tot)

      call conv_prim_vars_to_cons(s%rho, s%v, s%p, u)
   end function riemann_problem_w_k
end submodule w_k_submodule
