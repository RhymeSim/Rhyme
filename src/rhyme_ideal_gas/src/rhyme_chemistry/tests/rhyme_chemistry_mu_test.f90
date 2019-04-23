logical function rhyme_chemistry_mu_test () result (failed)
  use rhyme_chemistry
  use rhyme_assertion

  implicit none

  type ( assertion_t ) :: ch_tester

  type ( chemistry_t ) :: chemi
  type ( log_t ) :: log
  real(kind=8) :: X, Y, f(3)

  ch_tester = .describe. "chemistry mu"

  call chemi%init( log )

  X = .75d0
  Y = .25d0
  f = [.5d0, .25d0, .25d0]

  call ch_tester%expect( chemi%mu(X, Y, f) .toBe. 1.d0 / chemi%one_over_mu(X, Y, f) )

  failed = ch_tester%failed()
end function rhyme_chemistry_mu_test
