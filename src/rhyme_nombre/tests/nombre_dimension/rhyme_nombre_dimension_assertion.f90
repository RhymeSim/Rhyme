module rhyme_nombre_dimension_assertion
   use rhyme_nombre_dimension
   use rhyme_assertion

   implicit none

   interface operator(.toBe.)
      module procedure rhyme_nombre_dimension_assertion_tobe
   end interface operator(.toBe.)

contains

   pure module function rhyme_nombre_dimension_assertion_tobe(d_1, d_2) result(test)
      implicit none

      type(nombre_dimension_t), intent(in) :: d_1, d_2
      type(test_t) :: test

      test%op = 'to_be'

      write (test%val, *) d_1
      write (test%exp, *) d_2

      test%is_passed = d_1 == d_2
   end function rhyme_nombre_dimension_assertion_tobe
end module rhyme_nombre_dimension_assertion
