submodule(rhyme_irs) rhyme_irs_w_starR_sho_submodule
contains
pure module function irs_w_starR_sho(s, axis) result(u)
   implicit none

   type(riemann_problem_solution_t), intent(in) :: s
   integer, intent(in) :: axis
   real(kind=8) :: u(cid%rho:cid%e_tot)

   real(kind=8) :: v(NDIM)

   v = s%right%v
   v(axis) = s%star%u

   call conv_prim_vars_to_cons(s%star%right%shock%rho, v, s%star%p, u)
end function irs_w_starR_sho
end submodule rhyme_irs_w_starR_sho_submodule
