module rhyme_nombre_derived_unit_assertion
   use rhyme_nombre_derived_unit
   use rhyme_assertion

   implicit none

   interface operator(.toBe.)
      module procedure rhyme_nombre_derived_unit_assertion_tobe
   end interface operator(.toBe.)

contains

   module function rhyme_nombre_derived_unit_assertion_tobe(du_1, du_2) result(test)
      implicit none

      type(nombre_unit_t), intent(in) :: du_1, du_2
      type(test_t) :: test

      test%op = 'to_be'

      write (test%val, *) du_1
      write (test%exp, *) du_2

      test%is_passed = (du_1 == du_2)
   end function rhyme_nombre_derived_unit_assertion_tobe
end module rhyme_nombre_derived_unit_assertion
