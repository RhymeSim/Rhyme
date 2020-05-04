logical function rhyme_irs_iterate_test() result(failed)
   use rhyme_irs_factory
   use rhyme_physics_factory
   use rhyme_irs_tests_factory
   use rhyme_thermo_base_factory
   use rhyme_logger_factory
   use rhyme_assertion

   implicit none

   type(assertion_t) :: tester

   type(irs_t) :: irs
   type(physics_t) :: physics
   type(thermo_base_t) :: thermo
   type(logger_t) :: logger

   tester = .describe."irs_iterate"

   call rhyme_nombre_init

   irs = irs_factory%generate()
   physics = ph_factory%generate('SI')
   logger = log_factory%generate()

   thermo = th_factory%generate(physics, thid%diatomic)
   call rhyme_thermo_base_init(thermo, physics, logger)

   call rhyme_irs_init(irs, logger)

   call rhyme_irs_iterate_test_cases(rhyme_irs_Sod_test, "Sod", 5)
   call rhyme_irs_iterate_test_cases(rhyme_irs_123_test, "123 test", 4)
   call rhyme_irs_iterate_test_cases(rhyme_irs_left_blast_wave_test, "left blast wave", 5)
   call rhyme_irs_iterate_test_cases(rhyme_irs_right_blast_wave_test, "right blast wave", 5)
   call rhyme_irs_iterate_test_cases(rhyme_irs_two_shocks_collision_test, "two shock collision", 3)

   failed = tester%failed()

contains
   subroutine rhyme_irs_iterate_test_cases(func, test_name, sig_fig)
      implicit none

      external :: func
      character(len=*) :: test_name
      integer :: sig_fig ! Significant figures
      integer :: axis

      real(kind=8), dimension(cid%rho:cid%e_tot) :: l, r
      type(riemann_problem_solution_t) :: expected_solution, solution
      real(kind=8) :: ex_p, ex_u, ex_left_rho, ex_right_rho
      real(kind=8) :: star_left_rho, star_right_rho

      call func(l, r, expected_solution)
      call rhyme_irs_factory_set_sides(l, r, solution)

      axis = 1 ! x-axis

      ex_p = expected_solution%star%p
      ex_u = expected_solution%star%u

      call rhyme_irs_iterate(irs, solution, axis)

      if (expected_solution%star%left%is_shock) then
         ex_left_rho = expected_solution%star%left%shock%rho
         star_left_rho = solution%star%left%shock%rho
      else
         ex_left_rho = expected_solution%star%left%fan%rho
         star_left_rho = solution%star%left%fan%rho
      end if

      if (expected_solution%star%right%is_shock) then
         ex_right_rho = expected_solution%star%right%shock%rho
         star_right_rho = solution%star%right%shock%rho
      else
         ex_right_rho = expected_solution%star%right%fan%rho
         star_right_rho = solution%star%right%fan%rho
      end if

      call tester%expect(.notToBeNaN.solution%star%p)
      call tester%expect(solution%star%p &
                         .toBe.ex_p.within.sig_fig.hint.test_name//' p')

      call tester%expect(.notToBeNaN.solution%star%u)
      call tester%expect(solution%star%u &
                         .toBe.ex_u.within.sig_fig.hint.test_name//' u')

      call tester%expect( &
         solution%star%left%is_shock.toBe.expected_solution%star%left%is_shock &
         .hint.test_name//' left is shock?')

      call tester%expect(.notToBeNaN.star_left_rho)
      call tester%expect(star_left_rho &
                         .toBe.ex_left_rho.within.sig_fig.hint.test_name//' rhol')

      call tester%expect( &
         solution%star%right%is_shock.toBe.expected_solution%star%right%is_shock &
         .hint.test_name//' right is shock')

      call tester%expect(.notToBeNaN.star_right_rho)
      call tester%expect(star_right_rho &
                         .toBe.ex_right_rho.within.sig_fig.hint.test_name//' rhor')
   end subroutine rhyme_irs_iterate_test_cases
end function rhyme_irs_iterate_test
