logical function rhyme_periodic_table_get_element_by_name_test() result(failed)
   use rhyme_periodic_table_factory
   use rhyme_logger_factory
   use rhyme_assertion

   implicit none

   type(assertion_t) :: tester

   type(periodic_table_t) :: pt
   type(logger_t) :: logger

   type(periodic_table_element_t) :: element

   tester = .describe."periodic_table_get_element_by_name"

   pt = periodic_table_factory_generate('empty')
   logger = logger_factory_generate('default')

   call rhyme_periodic_table_init(pt, logger)

   element = rhyme_periodic_table_get_element_by_name(pt, 'H')
   call tester%expect(element%symb.toBe.'H'.hint.'H symb')

   element = pt%get_element_by_name('He')
   call tester%expect(element%symb.toBe.'He'.hint.'He symb')

   failed = tester%failed()
end function rhyme_periodic_table_get_element_by_name_test
