logical function rhyme_physics_init_test () result ( failed )
  use rhyme_physics_factory
  use rhyme_log_factory
  use rhyme_assertion

  implicit none

  type ( assertion_t ) :: ph_tester

  type ( physics_t ) :: physics
  type ( log_t ) :: logger

  ph_tester = .describe. "rhyme_physics_init"

  physics = ph_factory%generate()
  logger = log_factory%generate()

  call rhyme_physics_init( physics, logger )

  call ph_tester%expect( associated( physics%rho ) .toBe. .true. .hint. 'rho, associated' )
  call ph_tester%expect( associated( physics%length ) .toBe. .true. .hint. 'length, associated' )
  call ph_tester%expect( associated( physics%time ) .toBe. .true. .hint. 'time, associated' )
  call ph_tester%expect( associated( physics%velocity ) .toBe. .true. .hint. 'velocity, associated' )
  call ph_tester%expect( associated( physics%pressure ) .toBe. .true. .hint. 'pressure, associated' )
  call ph_tester%expect( associated( physics%temperature ) .toBe. .true. .hint. 'temperature, associated' )

  call ph_tester%expect( physics%rho .unitEqualsTo. ph_factory%rho .toBe. .true. .hint. 'rho' )
  call ph_tester%expect( physics%length .unitEqualsTo. ph_factory%length .toBe. .true. .hint. 'length' )
  call ph_tester%expect( physics%time .unitEqualsTo. ph_factory%time .toBe. .true. .hint. 'time' )
  call ph_tester%expect( physics%velocity .unitEqualsTo. ph_factory%velocity .toBe. .true. .hint. 'velocity' )
  call ph_tester%expect( physics%pressure .unitEqualsTo. ph_factory%pressure .toBe. .true. .hint. 'pressure' )
  call ph_tester%expect( physics%temperature .unitEqualsTo. ph_factory%temperature .toBe. .true. .hint. 'temperature' )

  failed = ph_tester%failed()
end function rhyme_physics_init_test
