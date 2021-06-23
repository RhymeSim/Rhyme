submodule(rhyme_riemann_problem) solve_submodule
contains
   module subroutine rhyme_riemann_problem_solve(rp, l, r, dx, dt, axis, u)
      implicit none

      type(riemann_problem_t), intent(in) :: rp
      real(kind=8), dimension(cid%rho:cid%e_tot), intent(in) :: l, r
      real(kind=8), intent(in) :: dx, dt
      integer, intent(in) :: axis
      real(kind=8), intent(inout) :: u(cid%rho:cid%e_tot)

      type(riemann_problem_solution_t) :: sol
      real(kind=8) :: dxdt, du, du_crit, e_rho
      real(kind=8) :: s_starl, s_starr
      logical :: vacuum_left, vacuum_right, vacuum_both_sides

      dxdt = dx/dt

      ! Filling left and right densities
      sol%left%rho = max(l(cid%rho), rp%w_vacuum(cid%rho))
      sol%right%rho = max(r(cid%rho), rp%w_vacuum(cid%rho))

      e_rho = nearest(rp%w_vacuum(cid%rho), 1d0)
      vacuum_right = sol%right%rho < e_rho .and. sol%left%rho > e_rho
      vacuum_left = sol%left%rho < e_rho .and. sol%right%rho > e_rho
      vacuum_both_sides = sol%left%rho < e_rho .and. sol%right%rho < e_rho

      if (vacuum_right) then

         sol%left%v(1:NDIM) = l(cid%rho_u:cid%rho_u + NDIM - 1)/l(cid%rho)
         sol%left%p = max(calc_p(l), rp%w_vacuum(cid%p))
         sol%left%cs = max(calc_cs(l), tiny(0d0))
         sol%right%v = 0d0
         sol%right%p = rp%w_vacuum(cid%p)
         sol%right%cs = 0d0

         s_starl = sol%left%v(axis) - 2*sol%left%cs/gm1

         if (dxdt > s_starl) then  ! W_0
            call conv_prim_to_cons(rp%w_vacuum, u)
         else if (dxdt > sol%left%v(axis) - sol%left%cs) then  ! W_lfan
            u = riemann_problem_w_kfan(sol%left, dxdt, axis, is_right=.false.)
         else  ! W_l
            u = riemann_problem_rp_side_to_cons(sol%left)
         end if

      else if (vacuum_left) then

         sol%left%v = 0d0
         sol%left%p = rp%w_vacuum(cid%p)
         sol%left%cs = 0d0
         sol%right%v(1:NDIM) = r(cid%rho_u:cid%rho_u + NDIM - 1)/r(cid%rho)
         sol%right%p = max(calc_p(r), rp%w_vacuum(cid%p))
         sol%right%cs = max(calc_cs(r), tiny(0d0))

         s_starr = sol%right%v(axis) + 2*sol%right%cs/gm1

         if (dxdt > sol%right%v(axis) + sol%right%cs) then  ! W_r
            u = riemann_problem_rp_side_to_cons(sol%right)
         else if (dxdt > s_starr) then  ! W_rfan
            u = riemann_problem_w_kfan(sol%right, dxdt, axis, is_right=.true.)
         else  ! W_0
            call conv_prim_to_cons(rp%w_vacuum, u)
         end if

      else if (vacuum_both_sides) then
         call conv_prim_to_cons(rp%w_vacuum, u)
      else  ! Non-vacuum cases

         sol%left%v(1:NDIM) = l(cid%rho_u:cid%rho_u + NDIM - 1)/l(cid%rho)
         sol%left%p = max(calc_p(l), rp%w_vacuum(cid%p))
         sol%left%cs = max(calc_cs(l), tiny(0d0))
         sol%right%v(1:NDIM) = r(cid%rho_u:cid%rho_u + NDIM - 1)/r(cid%rho)
         sol%right%p = max(calc_p(r), rp%w_vacuum(cid%p))
         sol%right%cs = max(calc_cs(r), tiny(0d0))

         s_starl = sol%left%v(axis) - 2*sol%left%cs/gm1
         s_starr = sol%right%v(axis) + 2*sol%right%cs/gm1

         du = sol%right%v(axis) - sol%left%v(axis)
         du_crit = 2*(sol%left%cs + sol%right%cs)/gm1

         if (du_crit < du) then  ! Generation of vacuum
            if (dxdt > s_starr) then  ! W_r0
               if (dxdt > sol%right%v(axis) + sol%right%cs) then  ! W_r
                  u = riemann_problem_rp_side_to_cons(sol%right)
               else  ! W_rfan
                  u = riemann_problem_w_kfan(sol%right, dxdt, axis, is_right=.true.)
               end if
            else if (dxdt > s_starl) then  ! W_0
               call conv_prim_to_cons(rp%w_vacuum, u)
            else  ! W_l0
               if (dxdt > sol%left%v(axis) - sol%left%cs) then  ! W_lfan
                  u = riemann_problem_w_kfan(sol%left, dxdt, axis, is_right=.false.)
               else  ! W_l
                  u = riemann_problem_rp_side_to_cons(sol%left)
               end if
            end if
         else  ! Normal cases
            call rhyme_riemann_problem_star(rp, sol, axis)
            call rhyme_riemann_problem_sampling(sol, axis, dx, dt, u)
         end if
      end if
   end subroutine rhyme_riemann_problem_solve
end submodule solve_submodule
