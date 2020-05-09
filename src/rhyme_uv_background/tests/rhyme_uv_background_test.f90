logical function rhyme_uv_background_test() result(failed)
   use rhyme_uv_background_factory
   use rhyme_assertion

   implicit none

   type(assertion_t) :: tester

   type(uv_background_t) :: uvb

   tester = .describe."uv_background"

   uvb = uv_background_factory_generate('default')

   call tester%expect(uvbid%unset.toBe.-1.hint.'unset')
   call tester%expect(uvbid%HM12.toBe.1.hint.'Haardt & Madau 12')

   failed = tester%failed()
end function rhyme_uv_background_test