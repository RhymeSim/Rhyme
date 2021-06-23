logical function rhyme_riemann_problem_sampling_test() result(failed)
   use rhyme_riemann_problem_factory
   use rhyme_units_factory
   use rhyme_riemann_problem_tests_factory
   use rhyme_thermo_base_factory
   use rhyme_logger_factory
   use rhyme_assertion

   implicit none

   type(assertion_t) :: tester

   type(riemann_problem_t) :: rp
   type(units_t) :: units
   type(thermo_base_t) :: thermo
   type(logger_t) :: logger
   real(kind=8), dimension(cid%rho:cid%e_tot) :: l, r, u
   type(riemann_problem_solution_t) :: solution

   integer :: i, axis
   real(kind=8) :: x, rho_, v, p, e_int

   tester = .describe."riemann_problem_sampling"

   call rhyme_nombre_init

   rp = riemann_problem_factory_generate('default')
   units = units_factory_generate('SI')
   logger = logger_factory_generate('default')
   axis = 1 ! x-axis

   thermo = thermo_base_factory_generate('diatomic')
   call rhyme_thermo_base_init(thermo, units, logger)

   call rhyme_riemann_problem_init(rp, units, thermo, logger)

   call rhyme_riemann_problem_Sod_test(l, r, solution)
   call rhyme_riemann_problem_star(rp, solution, axis)

   open (unit=1, file="./sod_shock_tube_t_0_2_analytical.txt", action='read', form='formatted')
   do i = 1, 12
      read (1, *)
   end do

   do i = 1, 500
      call tester%reset

      read (1, *) x, rho_, v, p
      e_int = p/0.4d0

      call rhyme_riemann_problem_sampling(solution, axis, &
                                          -.5d0 + real(i - 1, kind=8)/499.d0, .2d0, u)

      print *, i
      call tester%expect(rho_.toBe.u(cid%rho) .within.16.hint.'rho')
      call tester%expect(rho_*v.toBe.u(cid%rho_u) .within.14.hint.'rho_u')
      call tester%expect(p.toBe.calc_p(u) .within.16.hint.'p')
      call tester%expect(e_int.toBe.u(cid%rho)*calc_sp_int_e(u) .within.15.hint.'e_int')

      failed = tester%failed()

      if (failed) then
         close (1)
         return
      end if
   end do

   close (1)
end function rhyme_riemann_problem_sampling_test
