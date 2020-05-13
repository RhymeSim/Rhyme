logical function rhyme_chemistry_init_test() result(failed)
   use rhyme_chemistry_factory
   use rhyme_logger_factory
   use rhyme_assertion

   implicit none

   type(assertion_t) :: tester

   type(chemistry_t) :: chem
   type(logger_t) :: logger

   integer :: si, ei

   tester = .describe."chemistry_init"

   chem = chemistry_factory_generate('H+He')
   logger = logger_factory_generate('default')

   call rhyme_chemistry_init(chem, logger)

   call tester%expect(chem%pt%species(1)%element.toBe.'H'.hint.'Check periodic table')

   do si = 1, NSPE
      call tester%expect(chem%species(si)%symb.toBe.chemistry_factory_species_names(si) .hint.'species names')
      call tester%expect(chem%species_abundances(si) .toBe.chemistry_factory_species_abundances(si) .hint.'species abundances')
   end do

   call tester%expect(size(chem%elements) .toBe.2.hint.'elements size')

   do ei = 1, 2
      call tester%expect(chem%elements(ei)%element.toBe.chemistry_factory_element_names(ei) .hint.'element names')
      call tester%expect(chem%elements(ei)%ionized.toBe.0.hint.'element ionization')
      call tester%expect(chem%element_abundances(ei) .toBe.chemistry_factory_element_abundances(ei) .hint.'element abundances')
   end do

   failed = tester%failed()
end function rhyme_chemistry_init_test
