logical function rhyme_chemistry_test() result(failed)
   use rhyme_chemistry_factory
   use rhyme_assertion

   implicit none

   type(assertion_t) :: tester

   type(chemistry_t) :: chem

   tester = .describe."chemistry"

   call chemistry_factory%init('Case1')
   chem = chemistry_factory%generate()

   call tester%expect(chemid%idx.toBe.0.hint.'Placeholder test')

   failed = tester%failed()

   call chemistry_factory%final
end function rhyme_chemistry_test
