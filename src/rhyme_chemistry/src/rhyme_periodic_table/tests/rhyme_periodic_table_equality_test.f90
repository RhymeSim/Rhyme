logical function rhyme_periodic_table_equality_test() result(failed)
   use rhyme_periodic_table_factory
   use rhyme_assertion

   implicit none

   type(assertion_t) :: tester

   type(periodic_table_t) :: pt(2)

   tester = .describe."periodic_table_equality"

   call periodic_table_factory%init('Case1')
   pt(1) = periodic_table_factory%generate()

   call periodic_table_factory%init('Case2')
   pt(2) = periodic_table_factory%generate()

   call tester%expect(.false..toBe..true..hint.'Placeholder test')

   failed = tester%failed()

   call periodic_table_factory%final
end function rhyme_periodic_table_equality_test
