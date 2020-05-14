logical function rhyme_periodic_table_init_test() result(failed)
   use rhyme_periodic_table_factory
   use rhyme_logger_factory
   use rhyme_assertion

   implicit none

   type(assertion_t) :: tester

   type(periodic_table_t) :: pt
   type(logger_t) :: logger

   tester = .describe."periodic_table_init"

   pt = periodic_table_factory_generate('empty')
   logger = logger_factory_generate('default')

   call rhyme_periodic_table_init(pt, logger)

   call tester%expect(pt%elements(1)%symb.toBe.'H'.hint.'H symb')
   call tester%expect(pt%elements(1)%atomic_number.toBe.1.hint.'H number')
   call tester%expect(pt%elements(1)%atomic_weight.toBe.1.00811e0.hint.'H weight')
   call tester%expect(pt%elements(1)%species(1)%symb.toBe.'HII'.hint.'HII symb')
   call tester%expect(pt%elements(1)%species(1)%ionized.toBe.1.hint.'HI ionized')
   call tester%expect(associated(pt%elements(1)%species(1)%RI_A) .toBe..true..hint.'HI RI_A')
   call tester%expect(associated(pt%elements(1)%species(1)%RI_B) .toBe..true..hint.'HI RI_A')
   call tester%expect(associated(pt%elements(1)%species(1)%CI) .toBe..true..hint.'HI CI')
   call tester%expect(associated(pt%elements(1)%species(1)%CIE_A) .toBe..true..hint.'HI CIE_A')
   call tester%expect(associated(pt%elements(1)%species(1)%CIE_B) .toBe..true..hint.'HI CIE_B')
   call tester%expect(associated(pt%elements(1)%species(1)%IE_A) .toBe..true..hint.'HI IE_A')
   call tester%expect(associated(pt%elements(1)%species(1)%IE_B) .toBe..true..hint.'HI IE_B')

   call tester%expect(pt%elements(2)%symb.toBe.'He'.hint.'He symb')
   call tester%expect(pt%elements(2)%atomic_number.toBe.2.hint.'He number')
   call tester%expect(pt%elements(2)%atomic_weight.toBe.4.002602e0.hint.'He weight')
   call tester%expect(pt%elements(2)%species(1)%symb.toBe.'HeII'.hint.'HeII symb')
   call tester%expect(pt%elements(2)%species(1)%ionized.toBe.1.hint.'HeII ionized')
   call tester%expect(associated(pt%elements(2)%species(1)%RI_A) .toBe..true..hint.'HeII RI_A')
   call tester%expect(associated(pt%elements(2)%species(1)%RI_B) .toBe..true..hint.'HeII RI_A')
   call tester%expect(associated(pt%elements(2)%species(1)%CI) .toBe..true..hint.'HeII CI')
   call tester%expect(associated(pt%elements(2)%species(1)%CIE_A) .toBe..true..hint.'HeII CIE_A')
   call tester%expect(associated(pt%elements(2)%species(1)%CIE_B) .toBe..true..hint.'HeII CIE_B')
   call tester%expect(associated(pt%elements(2)%species(1)%IE_A) .toBe..true..hint.'HeII IE_A')
   call tester%expect(associated(pt%elements(2)%species(1)%IE_B) .toBe..true..hint.'HeII IE_B')

   call tester%expect(pt%elements(2)%symb.toBe.'He'.hint.'He symb')
   call tester%expect(pt%elements(2)%atomic_number.toBe.2.hint.'He number')
   call tester%expect(pt%elements(2)%atomic_weight.toBe.4.002602d0.hint.'He weight')
   call tester%expect(pt%elements(2)%species(2)%symb.toBe.'HeIII'.hint.'HeIII symb')
   call tester%expect(pt%elements(2)%species(2)%ionized.toBe.2.hint.'HeIII ionized')
   call tester%expect(associated(pt%elements(2)%species(2)%RI_A) .toBe..true..hint.'HeIII RI_A')
   call tester%expect(associated(pt%elements(2)%species(2)%RI_B) .toBe..true..hint.'HeIII RI_A')
   call tester%expect(associated(pt%elements(2)%species(2)%CI) .toBe..true..hint.'HeIII CI')
   call tester%expect(associated(pt%elements(2)%species(2)%CIE_A) .toBe..true..hint.'HeIII CIE_A')
   call tester%expect(associated(pt%elements(2)%species(2)%CIE_B) .toBe..true..hint.'HeIII CIE_B')
   call tester%expect(associated(pt%elements(2)%species(2)%IE_A) .toBe..true..hint.'HeIII IE_A')
   call tester%expect(associated(pt%elements(2)%species(2)%IE_B) .toBe..true..hint.'HeIII IE_B')

   failed = tester%failed()
end function rhyme_periodic_table_init_test
