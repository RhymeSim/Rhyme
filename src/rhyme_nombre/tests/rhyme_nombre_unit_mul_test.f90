logical function rhyme_nombre_unit_mul_test () result ( failed )
  use rhyme_nombre_unit
  use rhyme_assertion

  implicit none

  type ( assertion_t ) :: tester
  type ( nombre_unit_t ), pointer :: mega_meter, giga_meter, pc

  type ( nombre_unit_t ) :: meter = nombre_unit_t( one, 'm', 1.d0, dimid%length )

  tester = .describe. "rhyme_nombre_unit_mul"

  pc => 3.086d16 * meter
  call tester%expect( pc%prefix%base_10 .toBe. 0 .hint. 'prefix' )
  call tester%expect( pc%symb .toBe. 'm' .hint. 'symbol' )
  call tester%expect( pc%conv .toBe. 3.086d16 .hint. 'conversion factor' )
  call tester%expect( pc%dim%powers .toBe. dimid%length%powers .hint. 'dimension' )
  call tester%expect( pc%pow .toBe. 1d0 .hint. 'power' )

  pc => 3.086e16 * meter
  call tester%expect( pc%conv .toBe. 3.086e16 .hint. 'conversion factor' )

  pc => 3 * meter
  call tester%expect( pc%conv .toBe. 3d0 .hint. 'conversion factor' )

  mega_meter => mega * meter
  call tester%expect( mega_meter%symb .toBe. 'm' .hint. 'Mm symbol' )
  call tester%expect( mega_meter%pow .toBe. 1.d0 .hint. 'Mm power' )
  call tester%expect( mega_meter%prefix%symb .toBe. 'M' .hint. 'Mm prefix' )
  call tester%expect( associated( mega_meter%next ) .toBe. .false. )

  giga_meter => kilo * mega_meter
  call tester%expect( giga_meter%symb .toBe. 'm' .hint. 'Gm symbol' )
  call tester%expect( giga_meter%pow .toBe. 1.d0 .hint. 'Gm power' )
  call tester%expect( giga_meter%prefix%symb .toBe. 'G' .hint. 'Gm prefix' )
  call tester%expect( associated( giga_meter%next ) .toBe. .false. )

  failed = tester%failed()
end function rhyme_nombre_unit_mul_test
