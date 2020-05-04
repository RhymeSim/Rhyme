logical function rhyme_chemistry_test() result(failed)
   use rhyme_chemistry_factory
   use rhyme_assertion

   implicit none

   type(assertion_t) :: tester

   type(chemistry_t) :: chem

   tester = .describe."chemistry"

   chem = chemistry_factory_generate('H+He')

   failed = tester%failed()
end function rhyme_chemistry_test
