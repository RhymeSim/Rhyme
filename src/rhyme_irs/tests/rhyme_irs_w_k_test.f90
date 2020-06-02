logical function rhyme_irs_w_k_test() result(failed)
   use rhyme_irs_factory
   use rhyme_units_factory
   use rhyme_hydro_base_factory
   use rhyme_thermo_base_factory
   use rhyme_logger_factory
   use rhyme_assertion

   implicit none

   type(assertion_t) :: irs_tester

   type(irs_t) :: irs
   type(units_t) :: units
   type(thermo_base_t) :: thermo
   type(logger_t) :: logger
   type(rp_side_t) :: state
   real(kind=8), dimension(cid%rho:cid%e_tot) :: u, u_exp

   irs_tester = .describe."irs_w_k"

   call rhyme_nombre_init

   irs = irs_factory_generate('default')
   units = units_factory_generate('SI')

   thermo = thermo_base_factory_generate('diatomic')
   call rhyme_thermo_base_init(thermo, units, logger)

   call rhyme_irs_init(irs, logger)

   u_exp = hy_factory%generate_conserved()

   state%rho = hy_factory%rho
   state%v = hy_factory%v
   state%p = hy_factory%p

   u = irs_w_k(state)

   call irs_tester%expect(u.toBe.u_exp.within.15)

   failed = irs_tester%failed()
end function rhyme_irs_w_k_test
