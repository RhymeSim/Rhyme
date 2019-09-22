logical function rhyme_chemistry_init_test () result (failed)
  use rhyme_chemistry_factory
  use rhyme_physics_factory
  use rhyme_logger_factory
  use rhyme_nombre_assertion
  use rhyme_assertion

  implicit none

  type ( assertion_t ) :: ch_tester

  type ( chemistry_t ) :: chemistry
  type ( physics_t ) :: physics
  type ( logger_t ) :: logger

  type ( nombre_unit_t ), pointer :: m__mol, m

  ch_tester = .describe. "chemistry init"

  call rhyme_nombre_init

  chemistry = ch_factory%generate()
  physics = ph_factory%generate()
  logger = log_factory%generate()

  call rhyme_chemistry_init( chemistry, physics, logger )

  m__mol => physics%rho * physics%length**3 / mole
  m => physics%rho * physics%length**3

  call ch_tester%expect( chemistry%molar%e%v .toBe. 5.48580d-7 )
  call ch_tester%expect( chemistry%molar%e%u .toBe. m__mol )
  call ch_tester%expect( chemistry%molar%H%v .toBe. 1.00794d-3 )
  call ch_tester%expect( chemistry%molar%H%u .toBe. m__mol )
  call ch_tester%expect( chemistry%molar%He%v .toBe. 4.002602d-3 )
  call ch_tester%expect( chemistry%molar%He%u .toBe. m__mol )

  call ch_tester%expect( chemistry%atomic%e%v .toBe. 9.1093835d-31 )
  call ch_tester%expect( chemistry%atomic%e%u .toBe. m )
  call ch_tester%expect( chemistry%atomic%H%v .toBe. 1.6737236d-27 )
  call ch_tester%expect( chemistry%atomic%H%u .toBe. m )
  call ch_tester%expect( chemistry%atomic%He%v .toBe. 6.6464764d-27 )
  call ch_tester%expect( chemistry%atomic%He%u .toBe. m )

  failed = ch_tester%failed()
end function rhyme_chemistry_init_test
