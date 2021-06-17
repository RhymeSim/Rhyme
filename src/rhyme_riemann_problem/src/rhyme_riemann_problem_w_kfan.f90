submodule(rhyme_riemann_problem) w_kfan_submodule
contains
   pure module function riemann_problem_w_kfan(s, dxdt, axis, is_right) result(u)
      implicit none

      type(rp_side_t), intent(in) :: s
      real(kind=8), intent(in) :: dxdt
      integer, intent(in) :: axis
      logical, intent(in) :: is_right
      real(kind=8) :: u(cid%rho:cid%e_tot)

      real(kind=8) :: rho, v(NDIM), p, cs

      if (is_right) then
         cs = -s%cs
      else
         cs = s%cs
      end if

      rho = s%rho*( &
            2.d0/gp1 + gm1_gp1/cs*(s%v(axis) - dxdt) &
            )**real(2.d0/gm1, kind=8)

      v = s%v
      v(axis) = 2.d0/gp1*(cs + gm1/2.d0*s%v(axis) + dxdt)

      p = s%p*( &
          2.d0/gp1 + gm1_gp1/cs*(s%v(axis) - dxdt) &
          )**real(1.d0/gm1_2g, kind=8)

      call conv_prim_vars_to_cons(rho, v, p, u)
   end function riemann_problem_w_kfan
end submodule w_kfan_submodule
