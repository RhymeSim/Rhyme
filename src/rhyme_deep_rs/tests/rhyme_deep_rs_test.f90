logical function rhyme_deep_rs_test() result(failed)
   use rhyme_deep_rs_factory
   use rhyme_assertion

   implicit none

   type(assertion_t) :: tester

   type(deep_rs_t) :: drs

   tester = .describe."deep_rs"

   drs = deep_rs_factory_generate('default')

   call tester%expect(drsid%idx.toBe.0.hint.'Placeholder test')

   failed = tester%failed()
end function rhyme_deep_rs_test
