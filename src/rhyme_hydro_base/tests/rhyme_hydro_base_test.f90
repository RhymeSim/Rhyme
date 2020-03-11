logical function rhyme_hydro_base_test() result(failed)
   use rhyme_hydro_base_factory
   use rhyme_assertion

   implicit none

   type(assertion_t) :: hy_tester

   hy_tester = .describe."rhyme_hydro_base"

   failed = hy_tester%failed()
end function rhyme_hydro_base_test
