logical function rhyme_nombre_unit_mul_test () result ( failed )
  use rhyme_nombre_unit
  use rhyme_assertion

  implicit none

  type ( assertion_t ) :: tester
  type ( nombre_unit_t ), pointer :: mega_meter, giga_meter

  tester = .describe. "rhyme_nombre_unit_mul"

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
