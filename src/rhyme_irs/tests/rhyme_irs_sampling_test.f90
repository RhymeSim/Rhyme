logical function rhyme_irs_sampling_test() result(failed)
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
   real(kind=8), dimension(cid%rho:cid%e_tot) :: l, r, u
   type(riemann_problem_solution_t) :: solution

   integer :: i, axis
   real(kind=8) :: x, rho_, v, p, e_int

   irs_tester = .describe."irs_sampling"

   call rhyme_nombre_init

   irs = irs_factory%generate()
   physics = ph_factory%generate()
   logger = log_factory%generate()
   axis = 1 ! x-axis

   thermo = th_factory%generate(physics, thid%diatomic)
   call rhyme_thermo_base_init(thermo, physics, logger)

   call rhyme_irs_init(irs, logger)

   call rhyme_irs_Sod_test(l, r, solution)
   call rhyme_irs_iterate(irs, solution, axis)

   open (unit=1, file="./sod_shock_tube_t_0_2_analytical.txt", action='read', form='formatted')
   do i = 1, 12
      read (1, *)
   end do

   do i = 1, 500
      call irs_tester%reset

      read (1, *) x, rho_, v, p
      e_int = p/0.4d0

      call rhyme_irs_sampling(solution, axis, &
                              -.5d0 + real(i - 1, kind=8)/499.d0, .2d0, u)

      print *, i
      call irs_tester%expect(rho_.toBe.u(cid%rho) .within.16.hint.'rho')
      call irs_tester%expect(rho_*v.toBe.u(cid%rho_u) .within.14.hint.'rho_u')
      call irs_tester%expect(p.toBe.calc_p(u) .within.16.hint.'p')
      call irs_tester%expect(e_int.toBe.u(cid%rho)*calc_sp_int_e(u) .within.15.hint.'e_int')

      failed = irs_tester%failed()

      if (failed) then
         close (1)
         return
      end if
   end do

   close (1)
end function rhyme_irs_sampling_test
