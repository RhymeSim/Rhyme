logical function rhyme_nombre_print_test () result (failed)
  use rhyme_nombre
  use rhyme_assertion

  implicit none

  type ( assertion_t ) :: n_tester

  type ( nombre_t ) :: H

  n_tester = .describe. "nombre_print"

  call rhyme_nombre_init

  H = 66.7d0 .u. kilo * meter / second / (mega * parsec)

  call n_tester%expect( trim(H%p()) .toBe. "6.670E+01 [ " // trim( .printchain. H%u ) // " ]" )

  failed = n_tester%failed()
end function rhyme_nombre_print_test
