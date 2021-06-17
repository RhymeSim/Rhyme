submodule(rhyme_riemann_problem) sampling_submodule
contains
   pure module subroutine rhyme_riemann_problem_sampling(solution, axis, dx, dt, u)
      implicit none

      type(riemann_problem_solution_t), intent(in) :: solution
      integer, intent(in) :: axis
      real(kind=8), intent(in) :: dx, dt
      real(kind=8), intent(out) :: u(cid%rho:cid%e_tot)

      real(kind=8) :: dxdt

      dxdt = dx/dt

      if (dxdt > solution%star%u) then  ! Right side
         if (solution%star%right%is_shock) then  ! Right shock
            if (dxdt < solution%star%right%shock%speed) then  ! W_*R^sho
               u = riemann_problem_w_starR_sho(solution, axis)
            else  ! W_R
               u = riemann_problem_w_k(solution%right)
            end if
         else  ! Right rarefaction
            if (dxdt < solution%star%right%fan%speedT) then  ! W_*R^fan
               u = riemann_problem_w_starR_fan(solution, axis)
            else if (dxdt < solution%star%right%fan%speedH) then  ! W_Rfan
               u = riemann_problem_w_kfan(solution%right, dxdt, axis, is_right=.true.)
            else  ! W_R
               u = riemann_problem_w_k(solution%right)
            end if
         end if
      else  ! Left side
         if (solution%star%left%is_shock) then  ! Left shock
            if (dxdt > solution%star%left%shock%speed) then  ! W_*L^sho
               u = riemann_problem_w_starL_sho(solution, axis)
            else  ! W_L
               u = riemann_problem_w_k(solution%left)
            end if
         else  ! Left rarefaction
            if (dxdt > solution%star%left%fan%speedT) then  ! W_*L^fan
               u = riemann_problem_w_starL_fan(solution, axis)
            else if (dxdt > solution%star%left%fan%speedH) then  ! W_Lfan
               u = riemann_problem_w_kfan(solution%left, dxdt, axis, is_right=.false.)
            else  ! W_L
               u = riemann_problem_w_k(solution%left)
            end if
         end if
      end if
   end subroutine rhyme_riemann_problem_sampling
end submodule sampling_submodule
