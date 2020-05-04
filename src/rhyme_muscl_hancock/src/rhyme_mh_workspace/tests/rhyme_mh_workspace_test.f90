logical function rhyme_mh_workspace_test() result(failed)
   use rhyme_mh_workspace_factory
   use rhyme_assertion

   implicit none

   type(assertion_t) :: tester

   type(mh_workspace_t) :: mhws

   call tester%expect(mhws%type.toBe.mhwsid%memory_intensive)
   call tester%expect(lbound(mhws%levels, 1) .toBe.0)
   call tester%expect(ubound(mhws%levels, 1) .toBe.samrid%max_nlevels)

   failed = tester%failed()
end function rhyme_mh_workspace_test
