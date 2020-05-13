logical function rhyme_chemistry_init_test() result(failed)
   use rhyme_chemistry_factory
   use rhyme_logger_factory
   use rhyme_assertion

   implicit none

   type(assertion_t) :: tester

   type(chemistry_t) :: chemistry
   type(logger_t) :: logger

   integer :: si, ei

   tester = .describe."chemistry_init"

   chemistry = chemistry_factory_generate('H+He')
   logger = logger_factory_generate('default')

   call rhyme_chemistry_init(chemistry, logger)

   call tester%expect(chemistry%pt%species(1)%element.toBe.'H'.hint.'Check periodic table')

   do si = 1, NSPE
      call tester%expect(chemistry%species(si)%symb.toBe.chemistry(si) .hint.'species names')
      call tester%expect(chemistry%species_abundances(si) .toBe.chemistry(si) .hint.'species abundances')
   end do

   call tester%expect(size(chemistry%elements) .toBe.2.hint.'elements size')

   do ei = 1, 2
      call tester%expect(chemistry%elements(ei)%element.toBe.chemistry(ei) .hint.'element names')
      call tester%expect(chemistry%elements(ei)%ionized.toBe.0.hint.'element ionization')
      call tester%expect(chemistry%element_abundances(ei) .toBe.chemistry(ei) .hint.'element abundances')
   end do

   failed = tester%failed()
end function rhyme_chemistry_init_test
