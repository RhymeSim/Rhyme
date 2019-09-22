logical function rhyme_nombre_base_unit_test () result ( failed )
  use rhyme_nombre_base_unit
  use rhyme_assertion

  implicit none

  type ( assertion_t ) :: tester

  tester = .describe. "nombre_base_unit"

  call tester%expect( gram%prefix%base_10 .toBe. 0 .hint. 'gram prefix' )
  call tester%expect( gram%symb .toBe. 'g' .hint. 'gram symbol' )
  call tester%expect( gram%pow .toBe. 1d0 .hint. 'gram pow' )
  call tester%expect( gram%dim == dimid%mass .toBe. .true. .hint. 'gram dimension' )

  call tester%expect( kilogram%prefix%base_10 .toBe. 3 .hint. 'kg prefix' )
  call tester%expect( kilogram%symb .toBe. 'g' .hint. 'kg symbol' )
  call tester%expect( kilogram%pow .toBe. 1d0 .hint. 'kg pow' )
  call tester%expect( kilogram%dim == dimid%mass .toBe. .true. .hint. 'gram dimension' )

  call tester%expect( meter%prefix%base_10 .toBe. 0 .hint. 'meter prefix' )
  call tester%expect( meter%symb .toBe. 'm' .hint. 'meter symbol' )
  call tester%expect( meter%pow .toBe. 1d0 .hint. 'meter pow' )
  call tester%expect( meter%dim == dimid%length .toBe. .true. .hint. 'gram dimension' )

  call tester%expect( second%prefix%base_10 .toBe. 0 .hint. 'sec prefix' )
  call tester%expect( second%symb .toBe. 's' .hint. 'sec symbol' )
  call tester%expect( second%pow .toBe. 1d0 .hint. 'sec pow' )
  call tester%expect( second%dim == dimid%time .toBe. .true. .hint. 'gram dimension' )

  call tester%expect( kelvin%prefix%base_10 .toBe. 0 .hint. 'kel prefix' )
  call tester%expect( kelvin%symb .toBe. 'K' .hint. 'kel symbol' )
  call tester%expect( kelvin%pow .toBe. 1d0 .hint. 'kel pow' )
  call tester%expect( kelvin%dim == dimid%theta .toBe. .true. .hint. 'gram dimension' )

  call tester%expect( ampere%prefix%base_10 .toBe. 0 .hint. 'ampere prefix' )
  call tester%expect( ampere%symb .toBe. 'A' .hint. 'ampere symbol' )
  call tester%expect( ampere%pow .toBe. 1d0 .hint. 'ampere pow' )
  call tester%expect( ampere%dim == dimid%electric_current .toBe. .true. .hint. 'gram dimension' )

  call tester%expect( mole%prefix%base_10 .toBe. 0 .hint. 'mol prefix' )
  call tester%expect( mole%symb .toBe. 'mol' .hint. 'mol symbol' )
  call tester%expect( mole%pow .toBe. 1d0 .hint. 'mol pow' )
  call tester%expect( mole%dim == dimid%amount_of_substance .toBe. .true. .hint. 'gram dimension' )

  failed = tester %failed()
end function rhyme_nombre_base_unit_test