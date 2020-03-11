module rhyme_xyz_assertion
   use rhyme_xyz
   use rhyme_assertion

   implicit none

   interface operator(.toBe.)
      module procedure rhyme_xyz_assertion_tobe
   end interface operator(.toBe.)

contains

   module function rhyme_xyz_assertion_tobe(xxx1, xxx2) result(test)
      implicit none

      type(xyz_t), intent(in) :: xxx1, xxx2
      type(test_t) :: test

      test%op = 'to_be'

      write (test%val, *) xxx1
      write (test%exp, *) xxx2

      test%is_passed = xxx1 == xxx2
   end function rhyme_xyz_assertion_tobe
end module rhyme_xyz_assertion
