logical function rhyme_chemistry_one_over_mu_test () result (failed)
  use rhyme_chemistry
  use rhyme_assertion

  implicit none

  type ( assertion_t ) :: ch_tester

  type ( chemistry_t ) :: chemi
  type ( log_t ) :: log

  ch_tester = .describe. "chemistry one_over_mu"

  call chemi%init( log )

  call ch_tester%expect( chemi%one_over_mu(1.d0, 0.d0, [0.d0, 0.d0, 0.d0]) .toBe. 1.d0 / chemi%amu%H )
  call ch_tester%expect( chemi%one_over_mu(0.d0, 1.d0, [0.d0, 0.d0, 0.d0]) .toBe. 1.d0 / chemi%amu%He )
  call ch_tester%expect( chemi%one_over_mu(1.d0, 0.d0, [1.d0, 0.d0, 0.d0]) .toBe. 2.d0 / chemi%amu%H )
  call ch_tester%expect( chemi%one_over_mu(0.d0, 1.d0, [0.d0, 1.d0, 0.d0]) .toBe. 2.d0 / chemi%amu%He )
  call ch_tester%expect( chemi%one_over_mu(0.d0, 1.d0, [0.d0, 0.d0, 1.d0]) .toBe. 3.d0 / chemi%amu%He )

  failed = ch_tester%failed()
end function rhyme_chemistry_one_over_mu_test
