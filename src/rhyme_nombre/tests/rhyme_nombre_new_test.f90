logical function rhyme_nombre_new_test () result (failed)
  use rhyme_nombre
  use rhyme_assertion

  implicit none

  type ( assertion_t ) :: n_tester

  type ( nombre_t ) :: Hr8, Hr, Hi, kilogram

  n_tester = .describe. "nombre_new"

  call rhyme_nombre_init

  Hr8 = 66.7d0 .u. kilo * meter / sec / (mega * pc)
  Hr = 66.7e0 .u. kilo * meter / sec / (mega * pc)
  Hi = 66 .u. kilo * meter / sec / (mega * pc)
  kilogram = 1.0d0 .u. kg

  call n_tester%expect( Hr8%v .toBe. 66.7d0 )
  call n_tester%expect( Hr%v .toBe. 66.7e0 )
  call n_tester%expect( Hi%v .toBe. 66.0 )
  call n_tester%expect( associated(Hi%u) .toBe. .true. )
  call n_tester%expect( Hi%u%symb .toBe. "m" )
  call n_tester%expect( kilogram%v .toBe. 1.0d0 )

  failed = n_tester%failed()
end function rhyme_nombre_new_test
