logical function rhyme_nombre_parse_close_par_loc_test () result ( failed )
  use rhyme_nombre_parse
  use rhyme_assertion

  implicit none

  type ( assertion_t ) :: tester

  character ( len=8 ), dimension( 64 ) :: arr

  tester = .describe. "nombre_parse_close_par_loc"

  arr = rhyme_nombre_parse_tokenize( '( a * b / ( c^2 * d * ( e / f )^2 ) * g )')

  call tester%expect( rhyme_nombre_parse_close_par_loc( arr, 1 ) .toBe. 23 )
  call tester%expect( rhyme_nombre_parse_close_par_loc( arr, 6 ) .toBe. 20 )
  call tester%expect( rhyme_nombre_parse_close_par_loc( arr, 13 ) .toBe. 17 )

  failed = tester%failed()
end function rhyme_nombre_parse_close_par_loc_test
