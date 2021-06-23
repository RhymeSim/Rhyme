module rhyme_deep_rs_assertion
   use rhyme_deep_rs
   use rhyme_assertion

   implicit none

   interface operator(.toBe.)
      module procedure rhyme_deep_rs_assertion_tobe
   end interface operator(.toBe.)

contains

   module function rhyme_deep_rs_assertion_tobe(drs1, drs2) result(test)
      implicit none

      type(deep_rs_t), intent(in) :: drs1, drs2
      type(test_t) :: test

      test%op = 'to_be'

      write (test%val, *) drs1
      write (test%exp, *) drs2

      test%is_passed = .false.
   end function rhyme_deep_rs_assertion_tobe
end module rhyme_deep_rs_assertion
