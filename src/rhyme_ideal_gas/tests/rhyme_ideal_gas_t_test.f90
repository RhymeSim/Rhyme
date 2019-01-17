logical function rhyme_ideal_gas_t_test () result (failed)
  use rhyme_ideal_gas_factory

  implicit none

  real(kind=8) :: maw ! mean atomic weight

  call ig%init_with ( gas_type )

  maw = 2.34d1

  failed = abs ( ig%T(cons, maw) - ig%T_per_mu(cons) * maw ) > epsilon(0.d0)
end function rhyme_ideal_gas_t_test
