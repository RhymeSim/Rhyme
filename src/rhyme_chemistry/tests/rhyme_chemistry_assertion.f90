module rhyme_chemistry_assertion
   use rhyme_chemistry
   use rhyme_assertion

   implicit none

   interface operator(.toBe.)
      module procedure rhyme_chemistry_assertion_tobe
   end interface operator(.toBe.)

contains

   module function rhyme_chemistry_assertion_tobe(chem1, chem2) result(test)
      implicit none

      type(chemistry_t), intent(in) :: chem1, chem2
      type(test_t) :: test

      test%op = 'to_be'

      write (test%val, *) chem1
      write (test%exp, *) chem2

      test%is_passed = chem1 == chem2
   end function rhyme_chemistry_assertion_tobe
end module rhyme_chemistry_assertion
