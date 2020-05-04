logical function rhyme_xyz_test() result(failed)
   use rhyme_xyz_factory
   use rhyme_assertion

   implicit none

   type(assertion_t) :: tester

   type(xyz_t) :: xxx

   tester = .describe."xyz"

   xxx = xyz_factory_generate('default')

   call tester%expect(xxxid%idx.toBe.0.hint.'Placeholder test')

   failed = tester%failed()
end function rhyme_xyz_test
