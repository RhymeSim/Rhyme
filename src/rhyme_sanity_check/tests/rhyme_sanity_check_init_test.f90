logical function rhyme_sanity_check_init_test() result(failed)
   use rhyme_sanity_check_factory
   use rhyme_units_factory
   use rhyme_logger_factory
   use rhyme_nombre_assertion
   use rhyme_assertion

   implicit none

   type(assertion_t) :: tester

   type(sanity_check_t) :: sc
   type(units_t) :: units
   type(logger_t) :: logger

   tester = .describe."sanity_check_init"

   sc = sanity_check_factory_generate('default')
   units = units_factory_generate('SI')
   logger = logger_factory_generate('default')

   call rhyme_nombre_init
   call rhyme_units_init(units, logger)

   call rhyme_sanity_check_init(sc, units, logger)

   call tester%expect(sc%properties.toBe..true..hint.'Properties')

   call tester%expect(sc%rho_unit.toBe. (.parse.sc%rho_unit_str) .hint.'rho unit')
   call tester%expect(sc%vx_unit.toBe. (.parse.sc%vx_unit_str) .hint.'vx unit')
   call tester%expect(sc%vy_unit.toBe. (.parse.sc%vy_unit_str) .hint.'vy unit')
   call tester%expect(sc%vz_unit.toBe. (.parse.sc%vz_unit_str) .hint.'vz unit')
   call tester%expect(sc%e_tot_unit.toBe. (.parse.sc%e_tot_unit_str) .hint.'e_tot unit')
   call tester%expect(sc%temp_unit.toBe. (.parse.sc%temp_unit_str) .hint.'temp unit')

   failed = tester%failed()
end function rhyme_sanity_check_init_test
