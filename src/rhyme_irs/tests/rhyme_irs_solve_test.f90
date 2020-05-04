logical function rhyme_irs_solve_test() result(failed)
   use rhyme_irs_factory
   use rhyme_physics_factory
   use rhyme_irs_tests_factory
   use rhyme_thermo_base_factory
   use rhyme_logger_factory
   use rhyme_assertion

   implicit none

   type(assertion_t) :: irs_tester

   type(irs_t) :: irs
   type(physics_t) :: physics
   type(thermo_base_t) :: thermo
   type(logger_t) :: logger

   irs_tester = .describe."irs_solve"

   call rhyme_nombre_init

   irs = irs_factory%generate()
   physics = ph_factory%generate('SI')
   logger = log_factory%generate()

   thermo = th_factory%generate(physics, thid%diatomic)
   call rhyme_thermo_base_init(thermo, physics, logger)

   call rhyme_irs_init(irs, logger)

   ! Non-vacuum cases
   call irs_solve_test_cases(rhyme_irs_Sod_test, irs, irs_tester)
   call irs_solve_test_cases(rhyme_irs_123_test, irs, irs_tester)
   call irs_solve_test_cases(rhyme_irs_left_blast_wave_test, irs, irs_tester)
   call irs_solve_test_cases(rhyme_irs_right_blast_wave_test, irs, irs_tester)
   call irs_solve_test_cases(rhyme_irs_two_shocks_collision_test, irs, irs_tester)

   failed = irs_tester%failed()

   ! TODO: vacuum cases
end function rhyme_irs_solve_test

subroutine irs_solve_test_cases(func, irs, tester)
   use rhyme_irs_factory
   use rhyme_assertion

   implicit none

   external :: func
   type(irs_t) :: irs
   type(assertion_t) :: tester

   real(kind=8), dimension(cid%rho:cid%e_tot) :: l, r, u, u_exp
   type(riemann_problem_solution_t) :: solution
   real(kind=8) :: dx, dt
   integer :: axis

   axis = 1 ! x-axis

   call func(l, r, solution)
   call rhyme_irs_iterate(irs, solution, axis)

   ! Testing the right side of the solution
   dx = 1.d0
   if (solution%star%right%is_shock) then
      ! right side of the right shock (outside the shock)
      dt = dx/solution%star%right%shock%speed - epsilon(0.d0)
      call rhyme_irs_solve(irs, l, r, dx, dt, axis, u)

      call tester%expect(.notToBeNaN.u.hint.'right_side_of_shock')
      call tester%expect(u.toBe.r.within.16.hint.'right_side_of_shock')

      ! Inside the right shock
      dt = dx/solution%star%right%shock%speed + epsilon(0.d0)
      call rhyme_irs_solve(irs, l, r, dx, dt, axis, u)

      call conv_prim_vars_to_cons( &
         solution%star%right%shock%rho, &
         [solution%star%u, 0.d0, 0.d0], &
         solution%star%p, &
         u_exp &
         )

      call tester%expect(.notToBeNaN.u.hint.'inside_right_shock')
      call tester%expect(u.toBe.u_exp.hint.'inside_right_shock')
   else
      ! right side of the right fan (outside the fan)
      dt = dx/solution%star%right%fan%speedH - epsilon(0.d0)
      call rhyme_irs_solve(irs, l, r, dx, dt, axis, u)

      call tester%expect(.notToBeNaN.u.hint.'right_side_of_fan')
      call tester%expect(u.toBe.r.hint.'right_side_of_fan')

      ! left side of the right fan (outside the fan)
      dt = dx/solution%star%right%fan%speedT + 2*epsilon(0.d0)
      call rhyme_irs_solve(irs, l, r, dx, dt, axis, u)

      call conv_prim_vars_to_cons( &
         solution%star%right%fan%rho, &
         [solution%star%u, 0.d0, 0.d0], &
         solution%star%p, &
         u_exp &
         )

      call tester%expect(.notToBeNaN.u.hint.'behind_right_fan')
      call tester%expect(u.toBe.u_exp.hint.'behind_right_fan')

      ! Inside the right fan
      dt = ( &
           dx/solution%star%right%fan%speedH &
           + dx/solution%star%right%fan%speedT &
           )/2
      call rhyme_irs_solve(irs, l, r, dx, dt, axis, u)

      u_exp = irs_w_kfan(solution%right, dx/dt, axis, is_right=.true.)

      call tester%expect(.notToBeNaN.u.hint.'inside_right_fan')
      call tester%expect(u.toBe.u_exp.hint.'inside_right_fan')
   end if

   ! Testing the left side of the solution
   dx = -1.d0
   if (solution%star%left%is_shock) then
      ! left side of the left shock (outside the shock)
      dt = dx/solution%star%left%shock%speed - epsilon(0.d0)
      call rhyme_irs_solve(irs, l, r, dx, dt, axis, u)

      call tester%expect(.notToBeNaN.u.hint.'left_side_of_shock')
      call tester%expect(u.toBe.l.hint.'left_side_of_shock')

      ! Inside the left shock
      dt = dx/solution%star%left%shock%speed + epsilon(0.d0)
      call rhyme_irs_solve(irs, l, r, dx, dt, axis, u)

      call conv_prim_vars_to_cons( &
         solution%star%left%shock%rho, &
         [solution%star%u, 0.d0, 0.d0], &
         solution%star%p, &
         u_exp &
         )

      call tester%expect(.notToBeNaN.u.hint.'inside_left_shock')
      call tester%expect(u.toBe.u_exp.hint.'inside_left_shock')
   else
      ! left side of the left fan (outside the fan)
      dt = dx/solution%star%left%fan%speedH - epsilon(0.d0)
      call rhyme_irs_solve(irs, l, r, dx, dt, axis, u)

      call tester%expect(.notToBeNaN.u.hint.'left_side_of_fan')
      call tester%expect(u.toBe.l.hint.'left_side_of_fan')

      ! right side of the left fan (outside the fan)
      dt = dx/solution%star%left%fan%speedT + epsilon(0.d0)
      call rhyme_irs_solve(irs, l, r, dx, dt, axis, u)

      call conv_prim_vars_to_cons( &
         solution%star%left%fan%rho, &
         [solution%star%u, 0.d0, 0.d0], &
         solution%star%p, &
         u_exp &
         )

      call tester%expect(.notToBeNaN.u.hint.'behind_left_fan')
      call tester%expect(u.toBe.u_exp.within.14.hint.'behind_left_fan')

      ! Inside the left fan
      dt = ( &
           dx/solution%star%left%fan%speedH &
           + dx/solution%star%left%fan%speedT &
           )/2
      call rhyme_irs_solve(irs, l, r, dx, dt, axis, u)

      u_exp = irs_w_kfan(solution%left, dx/dt, axis, is_right=.false.)

      call tester%expect(.notToBeNaN.u.hint.'inside_left_fan')
      call tester%expect(u.toBe.u_exp.hint.'inside_left_fan')
   end if
end subroutine irs_solve_test_cases
