logical function rhyme_chemistry_equality_test() result(failed)
   use rhyme_chemistry_factory
   use rhyme_assertion

   implicit none

   type(assertion_t) :: tester

   type(chemistry_t) :: chem(2)

   tester = .describe."chemistry_equality"

   call chemistry_factory%init('Case1')
   chem(1) = chemistry_factory%generate()

   call chemistry_factory%init('Case2')
   chem(2) = chemistry_factory%generate()

   call tester%expect(.false..toBe..true..hint.'Placeholder test')

   failed = tester%failed()

   call chemistry_factory%final
end function rhyme_chemistry_equality_test
