logical function rhyme_chemistry_one_over_mu_test () result (failed)
  use rhyme_chemistry_factory
  use rhyme_physics_factory
  use rhyme_log_factory
  use rhyme_assertion

  implicit none

  type ( assertion_t ) :: ch_tester

  type ( chemistry_t ) :: chemistry
  type ( physics_t ) :: physics
  type ( log_t ) :: logger

  ch_tester = .describe. "chemistry one_over_mu"

  chemistry = ch_factory%generate()
  physics = ph_factory%generate()
  logger = log_factory%generate()

  call rhyme_chemistry_init( chemistry, physics, logger )

#if NSPE == 3
  call ch_tester%expect( rhyme_chemistry_one_over_mu( chemistry, 1.d0, 0.d0, [0.d0, 0.d0, 0.d0] ) &
    .toBe. 1.d0 / chemistry%amu%H )
  call ch_tester%expect( rhyme_chemistry_one_over_mu( chemistry, 0.d0, 1.d0, [0.d0, 0.d0, 0.d0] ) &
    .toBe. 1.d0 / chemistry%amu%He )
  call ch_tester%expect( rhyme_chemistry_one_over_mu( chemistry, 1.d0, 0.d0, [1.d0, 0.d0, 0.d0] ) &
    .toBe. 2.d0 / chemistry%amu%H )
  call ch_tester%expect( rhyme_chemistry_one_over_mu( chemistry, 0.d0, 1.d0, [0.d0, 1.d0, 0.d0] ) &
    .toBe. 2.d0 / chemistry%amu%He )
  call ch_tester%expect( rhyme_chemistry_one_over_mu( chemistry, 0.d0, 1.d0, [0.d0, 0.d0, 1.d0] ) &
    .toBe. 3.d0 / chemistry%amu%He )
#endif

  failed = ch_tester%failed()
end function rhyme_chemistry_one_over_mu_test
