logical function rhyme_ideal_gas_t_per_mu_test () result (failed)
  use rhyme_ideal_gas_factory

  implicit none

  real(kind=8) :: T__mu

  call chemi%init
  call ig%init_with (chemi, gas_type)

  T__mu = T / mu_

  failed = abs( ( ig%T_per_mu(cons) - T__mu ) / T__mu ) > epsilon(0.e0)
end function rhyme_ideal_gas_t_per_mu_test
