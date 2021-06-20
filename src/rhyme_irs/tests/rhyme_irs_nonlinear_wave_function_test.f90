logical function rhyme_irs_nonlinear_wave_function_test() result(failed)
   use rhyme_irs_factory
   use rhyme_units_factory
   use rhyme_thermo_base_factory
   use rhyme_logger_factory
   use rhyme_assertion

   implicit none

   type(assertion_t) :: irs_tester

   type(irs_t) :: irs
   type(units_t) :: units
   type(thermo_base_t) :: thermo
   type(logger_t) :: logger

   real(kind=8) :: state_rho, state_p, state_cs
   real(kind=8) :: p_star = 3.45d2, f, fprime, f_prev, fprime_prev
   integer :: i

   irs_tester = .describe."irs_nonlinear_wave_function"

   call rhyme_nombre_init

   irs = irs_factory_generate('default')
   units = units_factory_generate('SI')
   logger = logger_factory_generate('default')

   thermo = thermo_base_factory_generate('diatomic')
   call rhyme_thermo_base_init(thermo, units, logger)

   call rhyme_irs_init(irs, thermo, logger)

   state_rho = 1.23d3
   state_cs = 1.23d1

   call rhyme_irs_nonlinear_wave_function( &
      state_rho, state_p, state_cs, p_star, f, fprime)

   f_prev = f
   fprime_prev = fprime

   do i = 1, 800
      ! p = 2.34d0*p
      call irs_tester%reset

      call rhyme_irs_nonlinear_wave_function( &
         state_rho, state_p, state_cs, p_star, f, fprime)

      call irs_tester%expect((fprime < 0.0) .toBe..false.)
      call irs_tester%expect(fprime_prev.toBe.fprime.hint."f'prev == f'")
      call irs_tester%expect(f.toBe.f_prev.hint."fprev == f")

      failed = irs_tester%failed()
      if (failed) return

      f_prev = f
      fprime_prev = fprime
   end do
end function rhyme_irs_nonlinear_wave_function_test
