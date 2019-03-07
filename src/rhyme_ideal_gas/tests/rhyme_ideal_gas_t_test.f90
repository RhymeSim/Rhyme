logical function rhyme_ideal_gas_t_test () result (failed)
  use rhyme_ideal_gas_factory

  implicit none

  type ( ideal_gas_t ) :: ig
  real ( kind=8 ) :: maw ! mean atomic weight

  call rhyme_ideal_gas_factory_init
  call ig%init_with( chemi, thermo, gas_type, log )

  maw = 2.34d1

  failed = abs ( ig%T( hy%cons, maw ) - ig%T_per_mu( hy%cons ) * maw ) > epsilon(0.d0)
end function rhyme_ideal_gas_t_test
