logical function rhyme_uv_background_equality_test() result(failed)
   use rhyme_uv_background_factory
   use rhyme_uv_background_assertion
   use rhyme_assertion

   implicit none

   type(assertion_t) :: tester

   type(uv_background_t) :: uvb(2)

   tester = .describe."uv_background_equality"

   uvb(1) = uv_background_factory_generate('HM12')
   uvb(2) = uv_background_factory_generate('HM12')

   call tester%expect(uvb(1) .toBe.uvb(2))

   failed = tester%failed()
end function rhyme_uv_background_equality_test
