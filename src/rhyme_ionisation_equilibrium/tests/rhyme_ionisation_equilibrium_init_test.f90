logical function rhyme_ionisation_equilibrium_init_test() result(failed)
   use rhyme_ionisation_equilibrium_factory
   use rhyme_physics_factory
   use rhyme_chemistry_factory
   use rhyme_logger_factory
   use rhyme_nombre_assertion
   use rhyme_assertion

   implicit none

   type(assertion_t) :: tester

   type(ionisation_equilibrium_t) :: ie
   type(physics_t) :: physics
   type(chemistry_t) :: chemistry
   type(logger_t) :: logger

   integer :: si

   tester = .describe."ionisation_equilibrium_init"

   physics = physics_factory_generate('SI')
   chemistry = chemistry_factory_generate('H+He')
   logger = logger_factory_generate('default')

   ie = ionisation_equilibrium_factory_generate('CaseA-cgs')

   call rhyme_nombre_init
   call rhyme_physics_init(physics, logger)
   call rhyme_chemistry_init(chemistry, physics, logger)

   call rhyme_ionisation_equilibrium_init(ie, physics, chemistry, logger)

   do si = 1, NSPE
      call tester%expect(associated(ie%species(si)%RI) .toBe..true..hint.'ie RI')
      call tester%expect(associated(ie%species(si)%CI) .toBe..true..hint.'ie CI')
      call tester%expect(associated(ie%species(si)%CIE) .toBe..true..hint.'ie CIE')
      call tester%expect(associated(ie%species(si)%CPIE) .toBe..true..hint.'ie CPIE')
   end do

   call tester%expect(ie%table_temp_range(1)%u.toBe.physics%temperature.hint.'temp unit')
   call tester%expect(ie%table_temp_range(2)%u.toBe.physics%temperature.hint.'temp unit')
   call tester%expect(ie%table_density_range(1)%u.toBe.physics%rho.hint.'density unit')
   call tester%expect(ie%table_density_range(2)%u.toBe.physics%rho.hint.'density unit')

   failed = tester%failed()
end function rhyme_ionisation_equilibrium_init_test
