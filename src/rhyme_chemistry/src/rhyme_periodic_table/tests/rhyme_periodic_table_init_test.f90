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

   call rhyme_nombre_init()
   call rhyme_periodic_table_init(pt, logger)

   call tester%expect(pt%species(1)%element.toBe.'H'.hint.'H symb')
   call tester%expect(pt%species(1)%atomic_number.toBe.1.hint.'H number')
   call tester%expect(pt%species(1)%atomic_weight == (1.00811d0.u.atomic_mass_unit) .toBe..true..hint.'H weight')
   call tester%expect(pt%species(1)%symb.toBe.'HI'.hint.'HI symb')
   call tester%expect(pt%species(1)%ionized.toBe.0.hint.'HI ionized')
   call tester%expect(associated(pt%species(1)%RI_A) .toBe..false..hint.'HI RI_A')
   call tester%expect(associated(pt%species(1)%RI_B) .toBe..false..hint.'HI RI_A')
   call tester%expect(associated(pt%species(1)%CI) .toBe..false..hint.'HI CI')
   call tester%expect(associated(pt%species(1)%CIE_A) .toBe..false..hint.'HI CIE_A')
   call tester%expect(associated(pt%species(1)%CIE_B) .toBe..false..hint.'HI CIE_B')
   call tester%expect(associated(pt%species(1)%IE_A) .toBe..false..hint.'HI IE_A')
   call tester%expect(associated(pt%species(1)%IE_B) .toBe..false..hint.'HI IE_B')

   call tester%expect(pt%species(2)%element.toBe.'H'.hint.'H symb')
   call tester%expect(pt%species(2)%atomic_number.toBe.1.hint.'H number')
   call tester%expect(pt%species(2)%atomic_weight == (1.00811d0.u.atomic_mass_unit) .toBe..true..hint.'H weight')
   call tester%expect(pt%species(2)%symb.toBe.'HII'.hint.'HII symb')
   call tester%expect(pt%species(2)%ionized.toBe.1.hint.'HII ionized')
   call tester%expect(associated(pt%species(2)%RI_A) .toBe..true..hint.'HII RI_A')
   call tester%expect(associated(pt%species(2)%RI_B) .toBe..true..hint.'HII RI_A')
   call tester%expect(associated(pt%species(2)%CI) .toBe..true..hint.'HII CI')
   call tester%expect(associated(pt%species(2)%CIE_A) .toBe..true..hint.'HII CIE_A')
   call tester%expect(associated(pt%species(2)%CIE_B) .toBe..true..hint.'HII CIE_B')
   call tester%expect(associated(pt%species(2)%IE_A) .toBe..true..hint.'HII IE_A')
   call tester%expect(associated(pt%species(2)%IE_B) .toBe..true..hint.'HII IE_B')

   call tester%expect(pt%species(3)%element.toBe.'He'.hint.'He symb')
   call tester%expect(pt%species(3)%atomic_number.toBe.2.hint.'He number')
   call tester%expect(pt%species(3)%atomic_weight == (4.002602d0.u.atomic_mass_unit) .toBe..true..hint.'He weight')
   call tester%expect(pt%species(3)%symb.toBe.'HeI'.hint.'HeI symb')
   call tester%expect(pt%species(3)%ionized.toBe.0.hint.'HeI ionized')
   call tester%expect(associated(pt%species(3)%RI_A) .toBe..false..hint.'HeI RI_A')
   call tester%expect(associated(pt%species(3)%RI_B) .toBe..false..hint.'HeI RI_A')
   call tester%expect(associated(pt%species(3)%CI) .toBe..false..hint.'HeI CI')
   call tester%expect(associated(pt%species(3)%CIE_A) .toBe..false..hint.'HeI CIE_A')
   call tester%expect(associated(pt%species(3)%CIE_B) .toBe..false..hint.'HeI CIE_B')
   call tester%expect(associated(pt%species(3)%IE_A) .toBe..false..hint.'HeI IE_A')
   call tester%expect(associated(pt%species(3)%IE_B) .toBe..false..hint.'HeI IE_B')

   call tester%expect(pt%species(4)%element.toBe.'He'.hint.'He symb')
   call tester%expect(pt%species(4)%atomic_number.toBe.2.hint.'He number')
   call tester%expect(pt%species(4)%atomic_weight == (4.002602d0.u.atomic_mass_unit) .toBe..true..hint.'He weight')
   call tester%expect(pt%species(4)%symb.toBe.'HeII'.hint.'HeII symb')
   call tester%expect(pt%species(4)%ionized.toBe.1.hint.'HeII ionized')
   call tester%expect(associated(pt%species(4)%RI_A) .toBe..true..hint.'HeII RI_A')
   call tester%expect(associated(pt%species(4)%RI_B) .toBe..true..hint.'HeII RI_A')
   call tester%expect(associated(pt%species(4)%CI) .toBe..true..hint.'HeII CI')
   call tester%expect(associated(pt%species(4)%CIE_A) .toBe..true..hint.'HeII CIE_A')
   call tester%expect(associated(pt%species(4)%CIE_B) .toBe..true..hint.'HeII CIE_B')
   call tester%expect(associated(pt%species(4)%IE_A) .toBe..true..hint.'HeII IE_A')
   call tester%expect(associated(pt%species(4)%IE_B) .toBe..true..hint.'HeII IE_B')

   call tester%expect(pt%species(5)%element.toBe.'He'.hint.'He symb')
   call tester%expect(pt%species(5)%atomic_number.toBe.2.hint.'He number')
   call tester%expect(pt%species(5)%atomic_weight == (4.002602d0.u.atomic_mass_unit) .toBe..true..hint.'He weight')
   call tester%expect(pt%species(5)%symb.toBe.'HeIII'.hint.'HeIII symb')
   call tester%expect(pt%species(5)%ionized.toBe.2.hint.'HeIII ionized')
   call tester%expect(associated(pt%species(5)%RI_A) .toBe..true..hint.'HeIII RI_A')
   call tester%expect(associated(pt%species(5)%RI_B) .toBe..true..hint.'HeIII RI_A')
   call tester%expect(associated(pt%species(5)%CI) .toBe..true..hint.'HeIII CI')
   call tester%expect(associated(pt%species(5)%CIE_A) .toBe..true..hint.'HeIII CIE_A')
   call tester%expect(associated(pt%species(5)%CIE_B) .toBe..true..hint.'HeIII CIE_B')
   call tester%expect(associated(pt%species(5)%IE_A) .toBe..true..hint.'HeIII IE_A')
   call tester%expect(associated(pt%species(5)%IE_B) .toBe..true..hint.'HeIII IE_B')

   failed = tester%failed()
end function rhyme_periodic_table_init_test
