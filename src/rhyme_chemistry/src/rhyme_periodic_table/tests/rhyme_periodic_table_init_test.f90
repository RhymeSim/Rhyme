logical function rhyme_periodic_table_init_test() result(failed)
   use rhyme_periodic_table_factory
   use rhyme_logger_factory
   use rhyme_assertion

   implicit none

   type(assertion_t) :: tester

   type(periodic_table_t) :: pt
   type(logger_t) :: logger

   type(element_species_t), pointer :: HI, HII, HeI, HeII, HeIII

   tester = .describe."periodic_table_init"

   pt = periodic_table_factory_generate('empty')
   logger = logger_factory_generate('default')

   call rhyme_nombre_init()
   call rhyme_periodic_table_init(pt, logger)

   call tester%expect(pt%elements(ptid%H)%symb.toBe.'H'.hint.'H symb')
   call tester%expect(pt%elements(ptid%H)%atomic_number.toBe.1.hint.'H number')
   call tester%expect(pt%elements(ptid%H)%atomic_weight == (1.00811d0.u.atomic_mass_unit) .toBe..true..hint.'H weight')

   HI => pt%elements(ptid%H)%species
   call tester%expect(HI%symb.toBe.'HI'.hint.'HI symb')
   call tester%expect(HI%ionized.toBe.0.hint.'HI ionized')
   call tester%expect(associated(HI%RI_A) .toBe..false..hint.'HI RI_A')
   call tester%expect(associated(HI%RI_B) .toBe..false..hint.'HI RI_A')
   call tester%expect(associated(HI%CI) .toBe..false..hint.'HI CI')
   call tester%expect(associated(HI%CIE_A) .toBe..false..hint.'HI CIE_A')
   call tester%expect(associated(HI%CIE_B) .toBe..false..hint.'HI CIE_B')

   HII => pt%elements(ptid%H)%species%next
   call tester%expect(HII%symb.toBe.'HII'.hint.'HII symb')
   call tester%expect(HII%ionized.toBe.1.hint.'HII ionized')
   call tester%expect(associated(HII%RI_A) .toBe..true..hint.'HII RI_A')
   call tester%expect(associated(HII%RI_B) .toBe..true..hint.'HII RI_A')
   call tester%expect(associated(HII%CI) .toBe..true..hint.'HII CI')
   call tester%expect(associated(HII%CIE_A) .toBe..true..hint.'HII CIE_A')
   call tester%expect(associated(HII%CIE_B) .toBe..true..hint.'HII CIE_B')

   call tester%expect(pt%elements(ptid%He)%symb.toBe.'He'.hint.'He symb')
   call tester%expect(pt%elements(ptid%He)%atomic_number.toBe.2.hint.'He number')
   call tester%expect(pt%elements(ptid%He)%atomic_weight == (4.002602d0.u.atomic_mass_unit) .toBe..true..hint.'He weight')

   HeI => pt%elements(ptid%He)%species
   call tester%expect(HeI%symb.toBe.'HeI'.hint.'HeI symb')
   call tester%expect(HeI%ionized.toBe.0.hint.'HeI ionized')
   call tester%expect(associated(HeI%RI_A) .toBe..false..hint.'HeI RI_A')
   call tester%expect(associated(HeI%RI_B) .toBe..false..hint.'HeI RI_A')
   call tester%expect(associated(HeI%CI) .toBe..false..hint.'HeI CI')
   call tester%expect(associated(HeI%CIE_A) .toBe..false..hint.'HeI CIE_A')
   call tester%expect(associated(HeI%CIE_B) .toBe..false..hint.'HeI CIE_B')

   HeII => pt%elements(ptid%He)%species%next
   call tester%expect(HeII%symb.toBe.'HeII'.hint.'HeII symb')
   call tester%expect(HeII%ionized.toBe.1.hint.'HeII ionized')
   call tester%expect(associated(HeII%RI_A) .toBe..true..hint.'HeII RI_A')
   call tester%expect(associated(HeII%RI_B) .toBe..true..hint.'HeII RI_A')
   call tester%expect(associated(HeII%CI) .toBe..true..hint.'HeII CI')
   call tester%expect(associated(HeII%CIE_A) .toBe..true..hint.'HeII CIE_A')
   call tester%expect(associated(HeII%CIE_B) .toBe..true..hint.'HeII CIE_B')

   HeIII => pt%elements(ptid%He)%species%next%next
   call tester%expect(HeIII%symb.toBe.'HeIII'.hint.'HeIII symb')
   call tester%expect(HeIII%ionized.toBe.2.hint.'HeIII ionized')
   call tester%expect(associated(HeIII%RI_A) .toBe..true..hint.'HeIII RI_A')
   call tester%expect(associated(HeIII%RI_B) .toBe..true..hint.'HeIII RI_A')
   call tester%expect(associated(HeIII%CI) .toBe..true..hint.'HeIII CI')
   call tester%expect(associated(HeIII%CIE_A) .toBe..true..hint.'HeIII CIE_A')
   call tester%expect(associated(HeIII%CIE_B) .toBe..true..hint.'HeIII CIE_B')

   failed = tester%failed()
end function rhyme_periodic_table_init_test
