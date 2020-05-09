module rhyme_uv_background_assertion
   use rhyme_uv_background
   use rhyme_assertion

   implicit none

   interface operator(.toBe.)
      module procedure rhyme_uv_background_assertion_tobe
   end interface operator(.toBe.)

contains

   module function rhyme_uv_background_assertion_tobe(uvb1, uvb2) result(test)
      implicit none

      type(uv_background_t), intent(in) :: uvb1, uvb2
      type(test_t) :: test

      test%op = 'to_be'

      write (test%val, *) uvb1
      write (test%exp, *) uvb2

      test%is_passed = uvb1 == uvb2
   end function rhyme_uv_background_assertion_tobe
end module rhyme_uv_background_assertion
