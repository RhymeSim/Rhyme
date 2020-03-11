submodule(rhyme_irs) rhyme_irs_rp_side_to_cons_submodule
contains
pure module function irs_rp_side_to_cons(s) result(u)
   implicit none

   type(rp_side_t), intent(in) :: s
   real(kind=8) :: u(cid%rho:cid%e_tot)

   call conv_prim_vars_to_cons(s%rho, s%v, s%p, u)
end function irs_rp_side_to_cons
end submodule rhyme_irs_rp_side_to_cons_submodule
