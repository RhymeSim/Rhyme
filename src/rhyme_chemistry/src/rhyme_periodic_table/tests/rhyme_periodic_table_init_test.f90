logical function rhyme_periodic_table_init_test() result(failed)
   use rhyme_periodic_table_factory
   use rhyme_logger_factory
   use rhyme_assertion

   implicit none

   type(assertion_t) :: tester

   type(periodic_table_t) :: pt
   type(logger_t) :: logger

   tester = .describe."periodic_table_init"

   pt = periodic_table_factory%generate()
   logger = log_factory%generate()

   call rhyme_nombre_init()
   call rhyme_periodic_table_init(pt, logger)

   call tester%expect(periodic_table%elements(ptid%H)%symb.toBe.'H'.hint.'H symb')
   call tester%expect(periodic_table%elements(ptid%H)%atomic_number.toBe.1.hint.'H number')
   call tester%expect(periodic_table%elements(ptid%H)%atomic_weight == (1.00811d0.u.atomic_mass_unit) .toBe..true..hint.'H weight')
   call tester%expect(periodic_table%elements(ptid%H)%species%symb.toBe.'HII'.hint.'HII symb')
   call tester%expect(periodic_table%elements(ptid%H)%species%ionized.toBe.1.hint.'HII ionized')

   call tester%expect(periodic_table%elements(ptid%He)%symb.toBe.'He'.hint.'He symb')
   call tester%expect(periodic_table%elements(ptid%He)%atomic_number.toBe.2.hint.'He number')
 call tester%expect(periodic_table%elements(ptid%He)%atomic_weight == (4.002602d0.u.atomic_mass_unit) .toBe..true..hint.'He weight')
   call tester%expect(periodic_table%elements(ptid%He)%species%symb.toBe.'HeII'.hint.'HeII symb')
   call tester%expect(periodic_table%elements(ptid%He)%species%ionized.toBe.1.hint.'HeII ionized')
   call tester%expect(periodic_table%elements(ptid%He)%species%next%symb.toBe.'HeIII'.hint.'HeIII symb')
   call tester%expect(periodic_table%elements(ptid%He)%species%next%ionized.toBe.2.hint.'HeIII ionized')

   failed = tester%failed()

   call periodic_table_factory%final
end function rhyme_periodic_table_init_test
