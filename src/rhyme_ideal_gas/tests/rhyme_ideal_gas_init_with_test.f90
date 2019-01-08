logical function rhyme_ideal_gas_init_with_test () result (failed)
  use rhyme_ideal_gas_factory

  implicit none

  call chemi%init
  call ig%init_with ( chemi, igid%diatomic )

  failed = ig%type .ne. igid%diatomic
end function rhyme_ideal_gas_init_with_test
