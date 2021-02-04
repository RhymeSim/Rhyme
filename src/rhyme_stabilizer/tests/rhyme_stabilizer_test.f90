logical function rhyme_stabilizer_test() result(failed)
   use rhyme_stabilizer_factory
   use rhyme_assertion

   implicit none

   type(assertion_t) :: tester

   type(stabilizer_t) :: st

   tester = .describe."stabilizer"

   st = stabilizer_factory_generate('default')

   call tester%expect(stid%idx.toBe.0.hint.'Placeholder test')

   failed = tester%failed()
end function rhyme_stabilizer_test
