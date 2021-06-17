logical function rhyme_riemann_problem_w_k_test() result(failed)
   use rhyme_riemann_problem_factory
   use rhyme_units_factory
   use rhyme_hydro_base_factory
   use rhyme_thermo_base_factory
   use rhyme_logger_factory
   use rhyme_assertion

   implicit none

   type(assertion_t) :: tester

   type(riemann_problem_t) :: rp
   type(units_t) :: units
   type(thermo_base_t) :: thermo
   type(logger_t) :: logger
   type(rp_side_t) :: state
   real(kind=8), dimension(cid%rho:cid%e_tot) :: u, u_exp

   tester = .describe."riemann_problem_w_k"

   call rhyme_nombre_init

   rp = riemann_problem_factory_generate('default')
   units = units_factory_generate('SI')

   thermo = thermo_base_factory_generate('diatomic')
   call rhyme_thermo_base_init(thermo, units, logger)

   call rhyme_riemann_problem_init(rp, logger)

   u_exp = hy_factory%generate_conserved()

   state%rho = hy_factory%rho
   state%v = hy_factory%v
   state%p = hy_factory%p

   u = riemann_problem_w_k(state)

   call tester%expect(u.toBe.u_exp.within.15)

   failed = tester%failed()
end function rhyme_riemann_problem_w_k_test
