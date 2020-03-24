logical function rhyme_periodic_table_test() result(failed)
   use rhyme_periodic_table_factory
   use rhyme_assertion

   implicit none

   type(assertion_t) :: tester

   type(periodic_table_t) :: pt

   tester = .describe."periodic_table"

   call periodic_table_factory%init('Case1')
   pt = periodic_table_factory%generate()

   call tester%expect(ptid%idx.toBe.0.hint.'Placeholder test')

   failed = tester%failed()

   call periodic_table_factory%final
end function rhyme_periodic_table_test
