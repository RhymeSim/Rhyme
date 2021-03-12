submodule(rhyme_irs) rhyme_irs_iterate_submodule
contains
   pure module subroutine rhyme_irs_iterate(irs, solution, axis)
      implicit none

      type(irs_t), intent(in) :: irs
      type(riemann_problem_solution_t), intent(inout) :: solution
      integer, intent(in) :: axis

      real(kind=8) :: ps_pl, ps_pr
      real(kind=8) :: p_star_prev, guessed_p(6)

      integer :: i, guess_id

      guessed_p = rhyme_irs_guess_p_star(solution%left, solution%right, &
                                         axis, irs%w_vacuum(cid%p))

      do guess_id = 1, size(guessed_p)
         solution%star%p = guessed_p(guess_id)
         p_star_prev = tiny(0.d0)

         do i = 1, irs%n_iteration
            call rhyme_irs_nonlinear_wave_function( &
               solution%left, solution%star%p, solution%star%left)
            call rhyme_irs_nonlinear_wave_function( &
               solution%right, solution%star%p, solution%star%right)

            solution%star%p = &
               solution%star%p - ( &
               solution%star%left%f + solution%star%right%f &
               + (solution%right%v(axis) - solution%left%v(axis)) &
               )/(solution%star%left%fprime + solution%star%right%fprime)

            if (solution%star%p < 0.d0) exit

            if (2*abs(solution%star%p - p_star_prev) &
                /(solution%star%p + p_star_prev) < irs%tolerance) exit

            if (solution%star%p < 0d0) then
               solution%star%p = abs(solution%star%p*irs%tolerance)
            end if

            p_star_prev = solution%star%p
         end do

         if (solution%star%p > 0.d0) exit
      end do

      if (solution%star%p < 0.d0) solution%star%p = irs%w_vacuum(cid%p)

      solution%star%u = &
         0.5d0*( &
         (solution%right%v(axis) + solution%left%v(axis)) &
         + (solution%star%right%f - solution%star%left%f) &
         )

      ps_pl = solution%star%p/solution%left%p
      ps_pr = solution%star%p/solution%right%p

      if (solution%star%p > solution%left%p) then
         solution%star%left%is_shock = .true.
         solution%star%left%shock%rho = solution%left%rho*(gm1_gp1 + ps_pl)/(gm1_gp1*ps_pl + 1d0)
         solution%star%left%shock%speed = solution%left%v(axis) - solution%left%cs*sqrt(gp1_2g*ps_pl + gm1_2g)
      else
         solution%star%left%is_shock = .false.
         solution%star%left%fan%rho = solution%left%rho*ps_pl**real(g_inv, kind=8)
         solution%star%left%fan%cs = solution%left%cs*ps_pl**real(gm1_2g, kind=8)
         solution%star%left%fan%speedH = solution%left%v(axis) - solution%left%cs
         solution%star%left%fan%speedT = solution%star%u - solution%star%left%fan%cs
      end if

      if (solution%star%p > solution%right%p) then
         solution%star%right%is_shock = .true.
         solution%star%right%shock%rho = solution%right%rho*(gm1_gp1 + ps_pr)/(gm1_gp1*ps_pr + 1d0)
         solution%star%right%shock%speed = solution%right%v(axis) + solution%right%cs*sqrt(gp1_2g*ps_pr + gm1_2g)
      else
         solution%star%right%is_shock = .false.
         solution%star%right%fan%rho = solution%right%rho*ps_pr**real(g_inv, kind=8)
         solution%star%right%fan%cs = solution%right%cs*ps_pr**real(gm1_2g, kind=8)
         solution%star%right%fan%speedH = solution%right%v(axis) + solution%right%cs
         solution%star%right%fan%speedT = solution%star%u + solution%star%right%fan%cs
      end if
   end subroutine rhyme_irs_iterate
end submodule rhyme_irs_iterate_submodule
