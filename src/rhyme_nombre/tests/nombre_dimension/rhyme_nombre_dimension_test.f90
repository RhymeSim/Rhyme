logical function rhyme_nombre_dimension_test () result ( failed )
  use rhyme_nombre_dimension
  use rhyme_assertion

  implicit none

  type ( assertion_t ) :: tester

  tester = .describe. "dimension"

  call tester%expect( &
    dimid%null%powers .toBe. [0d0, 0d0, 0d0, 0d0, 0d0, 0d0, 0d0] &
    .hint. 'null powers' )
  call tester%expect( dimid%null%symb .toBe. '' .hint. 'null symbol' )

  call tester%expect( &
    dimid%mass%powers .toBe. [1d0, 0d0, 0d0, 0d0, 0d0, 0d0, 0d0] &
    .hint. 'mass powers' )
  call tester%expect( dimid%mass%symb .toBe. 'M' .hint. 'mass symbol' )

  call tester%expect( &
    dimid%length%powers .toBe. [0d0, 1d0, 0d0, 0d0, 0d0, 0d0, 0d0] &
    .hint. 'length powers' )
  call tester%expect( dimid%length%symb .toBe. 'L' .hint. 'length symbol' )

  call tester%expect( &
    dimid%time%powers .toBe. [0d0, 0d0, 1d0, 0d0, 0d0, 0d0, 0d0] &
    .hint. 'time powers' )
  call tester%expect( dimid%time%symb .toBe. 'T' .hint. 'time symbol' )

  call tester%expect( &
    dimid%theta%powers .toBe. [0d0, 0d0, 0d0, 1d0, 0d0, 0d0, 0d0] &
    .hint. 'theta powers' )
  call tester%expect( dimid%theta%symb .toBe. 'Theta' .hint. 'theta symbol' )

  call tester%expect( &
    dimid%electric_current%powers .toBe. [0d0, 0d0, 0d0, 0d0, 1d0, 0d0, 0d0] &
    .hint. 'electric_current powers' )
  call tester%expect( dimid%electric_current%symb .toBe. 'I' &
    .hint. 'electric_current symbol' )

  call tester%expect( &
    dimid%amount_of_substance%powers .toBe. [0d0, 0d0, 0d0, 0d0, 0d0, 1d0, 0d0] &
    .hint. 'amount_of_substance powers' )
  call tester%expect( dimid%amount_of_substance%symb .toBe. 'N' &
    .hint. 'amount_of_substance symbol' )

  call tester%expect( &
    dimid%luminocity%powers .toBe. [0d0, 0d0, 0d0, 0d0, 0d0, 0d0, 1d0] &
    .hint. 'luminocity powers' )
  call tester%expect( dimid%luminocity%symb .toBe. 'J' &
    .hint. 'luminocity symbol' )

  failed = tester%failed()
end function rhyme_nombre_dimension_test
