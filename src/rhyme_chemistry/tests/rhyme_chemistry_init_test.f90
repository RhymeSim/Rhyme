logical function rhyme_chemistry_init_test() result(failed)
   use rhyme_chemistry_factory
   use rhyme_logger_factory
   use rhyme_assertion

   implicit none

   type(assertion_t) :: tester

   type(chemistry_t) :: chem
   type(logger_t) :: logger

   integer :: si

   tester = .describe."chemistry_init"

   chem = chemistry_factory_generate('H+He')
   logger = logger_factory_generate('default')

   call rhyme_nombre_init()
   call rhyme_chemistry_init(chem, logger)

   call tester%expect(chem%pt%elements(1)%symb.toBe.'H'.hint.'Check periodic table')
   call tester%expect(chem%pt%elements(2)%symb.toBe.'He'.hint.'Check periodic table')

   do si = 1, NSPE
      call tester%expect(chem%species(si)%s%symb.toBe.chemistry_factory_H_He(si))
   end do

   failed = tester%failed()
end function rhyme_chemistry_init_test
