logical function rhyme_periodic_table_get_species_by_name_test() result(failed)
   use rhyme_periodic_table_factory
   use rhyme_logger_factory
   use rhyme_assertion

   implicit none

   type(assertion_t) :: tester

   type(periodic_table_t) :: pt
   type(logger_t) :: logger

   type(species_t) :: species

   tester = .describe."periodic_table_get_species_by_name"

   pt = periodic_table_factory_generate('empty')
   logger = logger_factory_generate('default')

   call rhyme_periodic_table_init(pt, logger)

   species = rhyme_periodic_table_get_species_by_name(pt, 'HI')
   call tester%expect(species%symb.toBe.'HI'.hint.'HI symb')

   species = rhyme_periodic_table_get_species_by_name(pt, 'HII')
   call tester%expect(species%symb.toBe.'HII'.hint.'HII symb')

   species = pt%get_species_by_name('HeI')
   call tester%expect(species%symb.toBe.'HeI'.hint.'HeI symb')

   species = pt%get_species_by_name('HeII')
   call tester%expect(species%symb.toBe.'HeII'.hint.'HeII symb')

   species = pt.getspeciesbyname.'HeIII'
   call tester%expect(species%symb.toBe.'HeIII'.hint.'HeIII symb')

   failed = tester%failed()
end function rhyme_periodic_table_get_species_by_name_test
