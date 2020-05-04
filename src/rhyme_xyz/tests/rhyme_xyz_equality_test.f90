logical function rhyme_xyz_equality_test() result(failed)
   use rhyme_xyz_factory
   use rhyme_assertion

   implicit none

   type(assertion_t) :: tester

   type(xyz_t) :: xxx(2)

   tester = .describe."xyz_equality"

   xxx(1) = xyz_factory_generate('default')
   xxx(2) = xyz_factory_generate('default')

   call tester%expect(.false..toBe..true..hint.'Placeholder test')

   failed = tester%failed()
end function rhyme_xyz_equality_test
