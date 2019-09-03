logical function rhyme_nombre_unit_update_symbol_test () result ( failed )
  use rhyme_nombre_unit
  use rhyme_assertion

  implicit none

  type ( assertion_t ) :: tester
  type ( nombre_unit_t ), pointer :: updated

  tester = .describe. "unit_update_symbol"

  updated => meter .as. 'updated'

  call tester%expect( updated%prefix%base_10 .toBe. meter%prefix%base_10 .hint. 'prefix' )
  call tester%expect( updated%symb .toBe. 'updated' .hint. 'symbole' )
  call tester%expect( updated%conv .toBe. meter%conv .hint. 'conversion factor' )
  call tester%expect( updated%dim%powers .toBe. meter%dim%powers .hint. 'dimension' )
  call tester%expect( updated%pow .toBe. meter%pow .hint. 'power' )

  failed = tester%failed()
end function rhyme_nombre_unit_update_symbol_test
