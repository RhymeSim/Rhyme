logical function rhyme_ideal_gas_init_with_test () result (failed)
  use rhyme_ideal_gas_factory

  implicit none

  type ( ideal_gas_t ) :: ig
  type ( chemistry_t ) :: chemi
  type ( thermo_base_t ) :: thermo

  call chemi%init
  call thermo%init
  call ig%init_with ( chemi, thermo, igid%diatomic )

  failed = ig%type .ne. igid%diatomic
end function rhyme_ideal_gas_init_with_test
