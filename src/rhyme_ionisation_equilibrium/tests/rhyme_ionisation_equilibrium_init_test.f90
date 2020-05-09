logical function rhyme_ionisation_equilibrium_init_test() result(failed)
   use rhyme_ionisation_equilibrium_factory
   use rhyme_chemistry_factory
   use rhyme_logger_factory
   use rhyme_assertion

   implicit none

   type(assertion_t) :: tester

   type(ionisation_equilibrium_t) :: ie
   type(chemistry_t) :: chemistry
   type(logger_t) :: logger

   integer :: si

   tester = .describe."ionisation_equilibrium_init"

   chemistry = chemistry_factory_generate('H+He')

   ie = ionisation_equilibrium_factory_generate('CaseA')
   logger = logger_factory_generate('default')

   call rhyme_nombre_init
   call rhyme_chemistry_init(chemistry, logger)
   call rhyme_ionisation_equilibrium_init(ie, chemistry, logger)

   do si = 1, NSPE
      call tester%expect(associated(ie%RI(si)%run) .toBe..true..hint.'ie RI')
      call tester%expect(associated(ie%CI(si)%run) .toBe..true..hint.'ie CI')
      call tester%expect(associated(ie%CIE(si)%run) .toBe..true..hint.'ie CIE')
   end do

   failed = tester%failed()
end function rhyme_ionisation_equilibrium_init_test
