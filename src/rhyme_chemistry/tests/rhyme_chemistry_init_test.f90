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

   chem = chemistry_factory%generate()
   logger = log_factory%generate()

   chem%species_name = 'HII'

   call rhyme_nombre_init()
   call rhyme_chemistry_init(chem, logger)

   call tester%expect(chem%pt%elements(1)%symb.toBe.'H'.hint.'Periodic table')

   do si = 1, size(chem%species_name)
      call tester%expect(chem%species(si)%s%symb.toBe.'HII')
   end do

   failed = tester%failed()

   call chemistry_factory%final
end function rhyme_chemistry_init_test
