logical function rhyme_chemistry_mu_test () result (failed)
  use rhyme_chemistry_factory
  use rhyme_physics_factory
  use rhyme_logger_factory
  use rhyme_assertion

  implicit none

  type ( assertion_t ) :: ch_tester

  type ( chemistry_t ) :: chemistry
  type ( physics_t ) :: physics
  type ( logger_t ) :: logger
  real(kind=8) :: X, Y, f( NSPE )

  ch_tester = .describe. "chemistry mu"

  call rhyme_nombre_units_init

  chemistry = ch_factory%generate()
  physics = ph_factory%generate()
  logger = log_factory%generate()

  call rhyme_chemistry_init( chemistry, physics, logger )

  X = .75d0
  Y = .25d0
#if NSPE == 3
  f = [.5d0, .25d0, .25d0]

  call ch_tester%expect( rhyme_chemistry_mu( chemistry, X, Y, f) &
    .toBe. 1.d0 / rhyme_chemistry_one_over_mu( chemistry, X, Y, f) )
#endif

  failed = ch_tester%failed()
end function rhyme_chemistry_mu_test
