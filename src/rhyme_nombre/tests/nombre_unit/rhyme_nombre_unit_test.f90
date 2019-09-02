logical function rhyme_nombre_unit_test () result ( failed )
  use rhyme_nombre_unit
  use rhyme_assertion

  implicit none

  type ( assertion_t ) :: tester

  tester = .describe. "nombre_unit"

  call tester%expect( gram%prefix%base_10 .toBe. 0 .hint. 'gram prefix' )
  call tester%expect( gram%symb .toBe. 'g' .hint. 'gram symbol' )
  call tester%expect( gram%pow .toBe. 1d0 .hint. 'gram pow' )
  call tester%expect( gram%dim%powers .toBe. dimid%mass%powers .hint. 'gram dimension' )

  call tester%expect( kg%prefix%base_10 .toBe. 3 .hint. 'kg prefix' )
  call tester%expect( kg%symb .toBe. 'g' .hint. 'kg symbol' )
  call tester%expect( kg%pow .toBe. 1d0 .hint. 'kg pow' )
  call tester%expect( kg%dim%powers .toBe. dimid%mass%powers .hint. 'kg dimension' )

  call tester%expect( meter%prefix%base_10 .toBe. 0 .hint. 'meter prefix' )
  call tester%expect( meter%symb .toBe. 'm' .hint. 'meter symbol' )
  call tester%expect( meter%pow .toBe. 1d0 .hint. 'meter pow' )
  call tester%expect( meter%dim%powers .toBe. dimid%length%powers .hint. 'meter dimension' )

  call tester%expect( sec%prefix%base_10 .toBe. 0 .hint. 'sec prefix' )
  call tester%expect( sec%symb .toBe. 's' .hint. 'sec symbol' )
  call tester%expect( sec%pow .toBe. 1d0 .hint. 'sec pow' )
  call tester%expect( sec%dim%powers .toBe. dimid%time%powers .hint. 'sec dimension' )

  call tester%expect( kel%prefix%base_10 .toBe. 0 .hint. 'kel prefix' )
  call tester%expect( kel%symb .toBe. 'K' .hint. 'kel symbol' )
  call tester%expect( kel%pow .toBe. 1d0 .hint. 'kel pow' )
  call tester%expect( kel%dim%powers .toBe. dimid%theta%powers .hint. 'kel dimension' )

  call tester%expect( ampere%prefix%base_10 .toBe. 0 .hint. 'ampere prefix' )
  call tester%expect( ampere%symb .toBe. 'A' .hint. 'ampere symbol' )
  call tester%expect( ampere%pow .toBe. 1d0 .hint. 'ampere pow' )
  call tester%expect( ampere%dim%powers .toBe. dimid%electric_current%powers .hint. 'ampere dimension' )

  call tester%expect( mol%prefix%base_10 .toBe. 0 .hint. 'mol prefix' )
  call tester%expect( mol%symb .toBe. 'mol' .hint. 'mol symbol' )
  call tester%expect( mol%pow .toBe. 1d0 .hint. 'mol pow' )
  call tester%expect( mol%dim%powers .toBe. dimid%amount_of_substance%powers .hint. 'mol dimension' )

  failed = tester %failed()
end function rhyme_nombre_unit_test
