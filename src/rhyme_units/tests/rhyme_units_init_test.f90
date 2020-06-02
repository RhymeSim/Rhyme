logical function rhyme_units_init_test() result(failed)
   use rhyme_units_factory
   use rhyme_logger_factory
   use rhyme_nombre_assertion
   use rhyme_assertion

   implicit none

   type(assertion_t) :: ph_tester

   type(units_t) :: units
   type(logger_t) :: logger

   ph_tester = .describe."rhyme_units_init"

   call rhyme_nombre_init

   units = units_factory_generate('SI')
   logger = logger_factory_generate('default')

   call rhyme_units_init(units, logger)

   call ph_tester%expect(associated(units%rho) .toBe..true..hint.'rho, associated')
   call ph_tester%expect(associated(units%length) .toBe..true..hint.'length, associated')
   call ph_tester%expect(associated(units%time) .toBe..true..hint.'time, associated')
   call ph_tester%expect(associated(units%velocity) .toBe..true..hint.'velocity, associated')
   call ph_tester%expect(associated(units%pressure) .toBe..true..hint.'pressure, associated')
   call ph_tester%expect(associated(units%temperature) .toBe..true..hint.'temperature, associated')
   call ph_tester%expect(associated(units%kb%u) .toBe..true..hint.'Boltzmann, associated')
   call ph_tester%expect(associated(units%r%u) .toBe..true..hint.'R, associated')
   call ph_tester%expect(associated(units%amu%u) .toBe..true..hint.'1 amu, associated')

   call ph_tester%expect(units%rho.toBe. (kilo*gram/meter**3) .hint.'rho')
   call ph_tester%expect(units%length.toBe.meter.hint.'length')
   call ph_tester%expect(units%time.toBe.second.hint.'time')
   call ph_tester%expect(units%velocity.toBe. (meter/second) .hint.'velocity')
   call ph_tester%expect(units%pressure.toBe. (kilo*gram/meter**3*meter**2/second**2) .hint.'pressure')
   call ph_tester%expect(units%temperature.toBe.kelvin.hint.'temperature')
   call ph_tester%expect(units%kb%u.toBe. (kilo*gram/meter**3*meter**5/second**2/kelvin) .hint.'Boltzmann')
   call ph_tester%expect(units%r%u.toBe. (kilo*gram/meter**3*meter**5/second**2/mole/kelvin) .hint.'R')
   call ph_tester%expect(units%amu%u.toBe. (kilo*gram/meter**3*meter**3) .hint.'1 amu')

   failed = ph_tester%failed()
end function rhyme_units_init_test
