module rhyme_periodic_table_assertion
   use rhyme_periodic_table
   use rhyme_assertion

   implicit none

   interface operator(.toBe.)
   end interface operator(.toBe.)
end module rhyme_periodic_table_assertion
