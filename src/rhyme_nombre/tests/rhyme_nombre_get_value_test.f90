logical function rhyme_nombre_get_value_test () result ( failed )
  use rhyme_nombre
  use rhyme_assertion

  implicit none

  type ( assertion_t ) :: n_tester
  type ( nombre_t ) :: n

  n_tester = .describe. "nombre_get_value"

  n = 1.23d4 .u. kg * meter / sec**2

  call n_tester%expect( rhyme_nombre_get_value( n ) .toBe. 1.23d4 )

  failed = n_tester%failed()
end function rhyme_nombre_get_value_test
