logical function rhyme_ideal_gas_bulk_modulus_test () result (failed)
  use rhyme_ideal_gas_factory

  implicit none

  call chemi%init

  call ig%init_with(chemi, gas_type)

  failed = abs(ig%B(cons) - ig%gamma * p) > epsilon(0.d0)
end function rhyme_ideal_gas_bulk_modulus_test