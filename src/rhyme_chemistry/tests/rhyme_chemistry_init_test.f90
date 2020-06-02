logical function rhyme_chemistry_init_test() result(failed)
   use rhyme_chemistry_factory
   use rhyme_units_factory
   use rhyme_logger_factory
   use rhyme_assertion

   implicit none

   type(assertion_t) :: tester

   type(chemistry_t) :: chemistry
   type(units_t) :: units
   type(logger_t) :: logger

   integer :: ei

   tester = .describe."chemistry_init"

   chemistry = chemistry_factory_generate('H+He')
   units = units_factory_generate('SI')
   logger = logger_factory_generate('default')

   call rhyme_nombre_init()
   call rhyme_units_init(units, logger)

   call rhyme_chemistry_init(chemistry, units, logger)

   do ei = 1, 2
      call tester%expect(chemistry%elements(ei)%symb.toBe.chemistry_factory_element_names(ei) .hint.'element names')
      call tester%expect(chemistry%element_abundances(ei) .toBe.chemistry_factory_element_abundances(ei) .hint.'element abundances')
   end do

   call tester%expect(chemistry%rho_to_number_density.toBe. (1d-6/hydrogen_mass%conv) .hint.'rho_to_number_density')

   failed = tester%failed()
end function rhyme_chemistry_init_test
