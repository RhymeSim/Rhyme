logical function rhyme_deep_rs_exec_test() result(failed)
   use rhyme_deep_rs_factory
   use rhyme_units_factory
   use rhyme_logger_factory
   use rhyme_assertion

   implicit none

   type(assertion_t) :: tester

   type(deep_rs_t) :: drs
   type(units_t) :: units
   type(logger_t) :: logger

   real(kind=8) :: p_star

   tester = .describe."deep_rs_exec"

   units = units_factory_generate('radamesh')
   logger = logger_factory_generate('default')

   call rhyme_nombre_init
   call rhyme_units_init(units, logger)

   call rhyme_deep_rs_init(drs, units, logger)
   p_star = rhyme_deep_rs_exec( &
            drs, 1d0, 8.63360831d-11, 1d0, 8.63360831d-11, 0d0)

   call tester%expect(p_star.toBe.1.4896335759294049E-013)

   failed = tester%failed()
end function rhyme_deep_rs_exec_test
