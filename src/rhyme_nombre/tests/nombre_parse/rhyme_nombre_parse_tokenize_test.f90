logical function rhyme_nombre_parse_tokenize_test () result ( failed )
  use rhyme_nombre_parse
  use rhyme_assertion

  implicit none

  type ( assertion_t ) :: tester
  character ( len=128 ) :: str
  character ( len=8 ), dimension( 64 ) :: arr
  integer :: i

  tester = .describe. "nombre_parse_tokenize"

  str = '  @saeed * 30m / 12 KM 34 OP ^ 234.567 * ( 12 / Y * GJmT^3 )'
  arr = rhyme_nombre_parse_tokenize( str )

  call tester%expect( arr(1) .toBe. '@saeed' )
  call tester%expect( arr(2) .toBe. '*' )
  call tester%expect( arr(3) .toBe. '30m' )
  call tester%expect( arr(4) .toBe. '/' )
  call tester%expect( arr(5) .toBe. '12KM34OP' )
  call tester%expect( arr(6) .toBe. '^' )
  call tester%expect( arr(7) .toBe. '234.567' )
  call tester%expect( arr(8) .toBe. '*' )
  call tester%expect( arr(9) .toBe. '(' )
  call tester%expect( arr(10) .toBe. '12' )
  call tester%expect( arr(11) .toBe. '/' )
  call tester%expect( arr(12) .toBe. 'Y' )
  call tester%expect( arr(13) .toBe. '*' )
  call tester%expect( arr(14) .toBe. 'GJmT' )
  call tester%expect( arr(15) .toBe. '^' )
  call tester%expect( arr(16) .toBe. '3' )
  call tester%expect( arr(17) .toBe. ')' )

  do i = 18, 64
    call tester%expect( arr(i) .toBe. char(0) )
  end do

  failed = tester%failed()
end function rhyme_nombre_parse_tokenize_test
