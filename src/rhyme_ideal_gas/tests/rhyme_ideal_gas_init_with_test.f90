logical function rhyme_ideal_gas_init_with_test () result (failed)
  use rhyme_ideal_gas_factory

  implicit none

  call ig%init_with ( igid%diatomic )

  failed = ig%type .ne. igid%diatomic
end function rhyme_ideal_gas_init_with_test
