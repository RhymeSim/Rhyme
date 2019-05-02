logical function rhyme_nombre_units_close_par_pos_test () result (failed)
  use rhyme_nombre_units
  use rhyme_assertion

  implicit none

  type ( assertion_t ) :: n_tester

  character ( len=256 ) :: str
  character ( len=8 ), dimension ( 32 ) :: arr

  n_tester = .describe. "nombre_units_close_par_pos"

  str = "( kg * ( m / s )^2 )"

  arr = str_unit_tokenizer( str )

  call n_tester%expect( close_par_pos(arr, 1) .toBe. 11 )
  call n_tester%expect( close_par_pos(arr, 4) .toBe. 8 )

  failed = n_tester%failed()
end function rhyme_nombre_units_close_par_pos_test
