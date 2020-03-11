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

   physics = ph_factory%generate()
   logger = log_factory%generate()

   call rhyme_physics_init(physics, logger)

   ! call ph_tester%expect( associated( physics%rho ) .toBe. .true. .hint. 'rho, associated' )
   ! call ph_tester%expect( associated( physics%length ) .toBe. .true. .hint. 'length, associated' )
   ! call ph_tester%expect( associated( physics%time ) .toBe. .true. .hint. 'time, associated' )
   ! call ph_tester%expect( associated( physics%velocity ) .toBe. .true. .hint. 'velocity, associated' )
   ! call ph_tester%expect( associated( physics%pressure ) .toBe. .true. .hint. 'pressure, associated' )
   ! call ph_tester%expect( associated( physics%temperature ) .toBe. .true. .hint. 'temperature, associated' )
   ! call ph_tester%expect( associated( physics%kb%u ) .toBe. .true. .hint. 'Boltzmann, associated' )
   ! call ph_tester%expect( associated( physics%r%u ) .toBe. .true. .hint. 'R, associated' )
   ! call ph_tester%expect( associated( physics%amu%u ) .toBe. .true. .hint. '1 amu, associated' )
   !
   ! call ph_tester%expect( physics%rho .toBe. ph_factory%rho .hint. 'rho' )
   ! call ph_tester%expect( physics%length .toBe. ph_factory%length .hint. 'length' )
   ! call ph_tester%expect( physics%time .toBe. ph_factory%time .hint. 'time' )
   ! call ph_tester%expect( physics%velocity .toBe. ph_factory%velocity .hint. 'velocity' )
   ! call ph_tester%expect( physics%pressure .toBe. ph_factory%pressure .hint. 'pressure' )
   ! call ph_tester%expect( physics%temperature .toBe. ph_factory%temperature .hint. 'temperature' )
   ! call ph_tester%expect( physics%kb%u .toBe. ph_factory%kb_unit .hint. 'Boltzmann' )
   ! call ph_tester%expect( physics%r%u .toBe. ph_factory%r_unit .hint. 'R' )
   ! call ph_tester%expect( physics%amu%u .toBe. ph_factory%amu_unit .hint. '1 amu' )

   failed = ph_tester%failed()
end function rhyme_physics_init_test
