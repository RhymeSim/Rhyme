module rhyme_periodic_table_assertion
   use rhyme_periodic_table
   use rhyme_assertion

   implicit none

   interface operator(.toBe.)
      module procedure rhyme_periodic_table_assertion_tobe
   end interface operator(.toBe.)

contains

   module function rhyme_periodic_table_assertion_tobe(pt1, pt2) result(test)
      implicit none

      type(periodic_table_t), intent(in) :: pt1, pt2
      type(test_t) :: test

      test%op = 'to_be'

      write (test%val, *) pt1
      write (test%exp, *) pt2

      test%is_passed = pt1 == pt2
   end function rhyme_periodic_table_assertion_tobe
end module rhyme_periodic_table_assertion
