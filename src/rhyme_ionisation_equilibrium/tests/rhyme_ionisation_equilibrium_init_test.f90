logical function rhyme_ionisation_equilibrium_init_test() result(failed)
   use rhyme_ionisation_equilibrium_factory
   use rhyme_physics_factory
   use rhyme_chemistry_factory
   use rhyme_uv_background_factory
   use rhyme_logger_factory
   use rhyme_nombre_assertion
   use rhyme_assertion

   implicit none

   type(assertion_t) :: tester

   type(ionisation_equilibrium_t) :: ie
   type(physics_t) :: physics
   type(chemistry_t) :: chemistry
   type(uv_background_t) :: uvb
   type(logger_t) :: logger

   integer :: si
   real(kind=8) :: z

   tester = .describe."ionisation_equilibrium_init"

   physics = physics_factory_generate('SI')
   chemistry = chemistry_factory_generate('H+He')
   uvb = uv_background_factory_generate('HM12')
   logger = logger_factory_generate('default')

   ie = ionisation_equilibrium_factory_generate('CaseA-cgs')

   call rhyme_nombre_init
   call rhyme_chemistry_init(chemistry, logger)
   call rhyme_physics_init(physics, logger)
   z = 1.37

   call rhyme_ionisation_equilibrium_init(ie, physics, chemistry, uvb, z, logger)

   do si = 1, NSPE
      call tester%expect(associated(ie%RI(si)%run) .toBe..true..hint.'ie RI')
      call tester%expect(associated(ie%CI(si)%run) .toBe..true..hint.'ie CI')
      call tester%expect(associated(ie%CIE(si)%run) .toBe..true..hint.'ie CIE')
   end do

   call tester%expect(ie%table_temp_range(1)%u.toBe.physics%temperature.hint.'temp unit')
   call tester%expect(ie%table_temp_range(2)%u.toBe.physics%temperature.hint.'temp unit')
   call tester%expect(ie%table_density_range(1)%u.toBe.physics%rho.hint.'density unit')
   call tester%expect(ie%table_density_range(2)%u.toBe.physics%rho.hint.'density unit')

   call tester%expect(ie%uvb_photoheating.notToBe.-1e0.hint.'uvb_photoheating')
   call tester%expect(ie%gamma_uvb.notToBe.-1e0.hint.'gamma_uvb')

   failed = tester%failed()
end function rhyme_ionisation_equilibrium_init_test
