logical function rhyme_tiling_test() result(failed)
   use rhyme_tiling_factory
   use rhyme_assertion

   implicit none

   type(assertion_t) :: tester

   type(tiling_t) :: tile

   tester = .describe."tiling"

   tile = tiling_factory_generate('default')

   call tester%expect(tileid%unset.toBe.-1234.hint.'Placeholder test')

   failed = tester%failed()
end function rhyme_tiling_test
