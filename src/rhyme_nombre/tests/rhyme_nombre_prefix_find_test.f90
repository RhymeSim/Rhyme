logical function rhyme_nombre_prefix_find_test () result (failed)
  use rhyme_nombre_units
  use rhyme_assertion

  implicit none

  type ( assertion_t ) :: n_tester

  type ( nombre_prefix_t ) :: p

  n_tester = .describe. "nombre_prefix_find"

  p = prefix_find("mum")
  call n_tester%expect( p%symb .toBe. "mu" )

  p = prefix_find("pc")
  call n_tester%expect( p%base_10 .toBe. 0 )

  p = prefix_find("Mpc")
  call n_tester%expect( p%symb .toBe. "M" )

  failed = n_tester%failed()
end function rhyme_nombre_prefix_find_test
