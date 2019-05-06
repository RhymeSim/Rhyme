logical function rhyme_chemistry_one_over_mu_test () result (failed)
  use rhyme_chemistry
  use rhyme_units_factory
  use rhyme_assertion

  implicit none

  type ( assertion_t ) :: ch_tester

  type ( chemistry_t ) :: chemi
  type ( rhyme_units_t ) :: units
  type ( log_t ) :: log

  ch_tester = .describe. "chemistry one_over_mu"

  units = units_factory%generate()
  call rhyme_chemistry_init( chemi, units, log )

  call ch_tester%expect( rhyme_chemistry_one_over_mu( chemi, 1.d0, 0.d0, [0.d0, 0.d0, 0.d0] ) &
    .toBe. 1.d0 / chemi%amu%H )
  call ch_tester%expect( rhyme_chemistry_one_over_mu( chemi, 0.d0, 1.d0, [0.d0, 0.d0, 0.d0] ) &
    .toBe. 1.d0 / chemi%amu%He )
  call ch_tester%expect( rhyme_chemistry_one_over_mu( chemi, 1.d0, 0.d0, [1.d0, 0.d0, 0.d0] ) &
    .toBe. 2.d0 / chemi%amu%H )
  call ch_tester%expect( rhyme_chemistry_one_over_mu( chemi, 0.d0, 1.d0, [0.d0, 1.d0, 0.d0] ) &
    .toBe. 2.d0 / chemi%amu%He )
  call ch_tester%expect( rhyme_chemistry_one_over_mu( chemi, 0.d0, 1.d0, [0.d0, 0.d0, 1.d0] ) &
    .toBe. 3.d0 / chemi%amu%He )

  failed = ch_tester%failed()
end function rhyme_chemistry_one_over_mu_test
