logical function rhyme_ideal_gas_init_with_test () result (failed)
  use rhyme_ideal_gas_factory

  implicit none

  type ( ideal_gas_t ) :: ig

  call ig%init_with ( chemi, thermo, igid%diatomic, log )

  failed = ig%type .ne. igid%diatomic
end function rhyme_ideal_gas_init_with_test
