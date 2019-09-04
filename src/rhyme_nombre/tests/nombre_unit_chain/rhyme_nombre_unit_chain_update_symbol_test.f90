logical function rhyme_nombre_unit_chain_update_symbol_test () result ( failed )
  use rhyme_nombre_unit_chain
  use rhyme_assertion

  implicit none

  type ( assertion_t ) :: tester

  type ( nombre_unit_chain_t ), pointer :: chain

  tester = .describe. "nombre_unit_chain_update_symbol"

  chain => kg * meter**2 / sec**2 .as. 'J'

  call tester%expect( chain%symb .toBe. 'J' .hint. 'symbol' )
  call tester%expect( chain%head == kg .toBe. .true. .hint. 'kg' )
  call tester%expect( chain%head%next == meter**2 .toBe. .true. .hint. 'meter' )
  call tester%expect( chain%head%next%next == sec**(-2) .toBe. .true. .hint. 'sec' )

  failed = tester%failed()
end function rhyme_nombre_unit_chain_update_symbol_test
