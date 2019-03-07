logical function rhyme_chemistry_mu_test () result (failed)
  use rhyme_chemistry

  implicit none

  type ( chemistry_t ) :: chemi
  type ( log_t ) :: log
  real(kind=8) :: X, Y, f(3)

  call chemi%init( log )

  X = .75d0
  Y = .25d0
  f = [.5d0, .25d0, .25d0]

  failed = abs ( chemi%mu(X, Y, f) - 1.d0 / chemi%one_over_mu(X, Y, f) ) > epsilon(0.d0)
end function rhyme_chemistry_mu_test
