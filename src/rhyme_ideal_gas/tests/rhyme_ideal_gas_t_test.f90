logical function rhyme_ideal_gas_t_test () result (failed)
  use rhyme_ideal_gas_factory

  implicit none

  type ( ideal_gas_t ) :: ig
  type ( chemistry_t ) :: chemi
  type ( thermo_base_t ) :: thermo
  real(kind=8) :: maw ! mean atomic weight

  call chemi%init
  call thermo%init
  call ig%init_with( chemi, thermo, gas_type )

  maw = 2.34d1

  failed = abs ( ig%T(cons, maw) - ig%T_per_mu(cons) * maw ) > epsilon(0.d0)
end function rhyme_ideal_gas_t_test
