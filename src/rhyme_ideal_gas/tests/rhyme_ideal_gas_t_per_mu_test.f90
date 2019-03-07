logical function rhyme_ideal_gas_t_per_mu_test () result (failed)
  use rhyme_ideal_gas_factory

  implicit none

  type ( ideal_gas_t ) :: ig
  real ( kind=8 ) :: T__mu

  call rhyme_ideal_gas_factory_init
  call ig%init_with( chemi, thermo, gas_type, log )

  T__mu = hy%T / hy%mu

  failed = abs( ( ig%T_per_mu( hy%cons ) - T__mu ) / T__mu ) > epsilon(0.e0)
end function rhyme_ideal_gas_t_per_mu_test
