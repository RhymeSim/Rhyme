logical function rhyme_nombre_print_test () result (failed)
  use rhyme_nombre
  use rhyme_assertion

  implicit none

  type ( assertion_t ) :: n_tester

  type(nombre_t) :: H

  n_tester = .describe. "nombre_print"

  H = 66.7d0 .u. kilo * m / s / (Mega * pc)

  call n_tester%expect( trim(H%p()) .toBe. "0.667E+02 [ " // trim(H%u%p()) // " ]" )

  failed = n_tester%failed()
end function rhyme_nombre_print_test
