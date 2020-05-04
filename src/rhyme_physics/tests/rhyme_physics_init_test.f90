logical function rhyme_physics_init_test() result(failed)
   use rhyme_physics_factory
   use rhyme_logger_factory
   use rhyme_nombre_assertion
   use rhyme_assertion

   implicit none

   type(assertion_t) :: ph_tester

   type(physics_t) :: physics
   type(logger_t) :: logger

   ph_tester = .describe."rhyme_physics_init"

   call rhyme_nombre_init

   physics = physics_factory_generate('SI')
   logger = logger_factory_generate('default')

   call rhyme_physics_init(physics, logger)

   call ph_tester%expect(associated(physics%rho) .toBe..true..hint.'rho, associated')
   call ph_tester%expect(associated(physics%length) .toBe..true..hint.'length, associated')
   call ph_tester%expect(associated(physics%time) .toBe..true..hint.'time, associated')
   call ph_tester%expect(associated(physics%velocity) .toBe..true..hint.'velocity, associated')
   call ph_tester%expect(associated(physics%pressure) .toBe..true..hint.'pressure, associated')
   call ph_tester%expect(associated(physics%temperature) .toBe..true..hint.'temperature, associated')
   call ph_tester%expect(associated(physics%kb%u) .toBe..true..hint.'Boltzmann, associated')
   call ph_tester%expect(associated(physics%r%u) .toBe..true..hint.'R, associated')
   call ph_tester%expect(associated(physics%amu%u) .toBe..true..hint.'1 amu, associated')

   call ph_tester%expect(physics%rho.toBe. (kilo*gram/meter**3) .hint.'rho')
   call ph_tester%expect(physics%length.toBe.meter.hint.'length')
   call ph_tester%expect(physics%time.toBe.second.hint.'time')
   call ph_tester%expect(physics%velocity.toBe. (meter/second) .hint.'velocity')
   call ph_tester%expect(physics%pressure.toBe. (kilo*gram/meter**3*meter**2/second**2) .hint.'pressure')
   call ph_tester%expect(physics%temperature.toBe.kelvin.hint.'temperature')
   call ph_tester%expect(physics%kb%u.toBe. (kilo*gram/meter**3*meter**5/second**2/kelvin) .hint.'Boltzmann')
   call ph_tester%expect(physics%r%u.toBe. (kilo*gram/meter**3*meter**5/second**2/mole/kelvin) .hint.'R')
   call ph_tester%expect(physics%amu%u.toBe. (kilo*gram/meter**3*meter**3) .hint.'1 amu')

   failed = ph_tester%failed()
end function rhyme_physics_init_test
