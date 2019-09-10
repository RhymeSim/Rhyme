logical function rhyme_nombre_derived_unit_clone_test () result ( failed )
  use rhyme_nombre_derived_unit
  use rhyme_assertion

  implicit none

  type ( assertion_t ) :: tester
  type ( nombre_derived_unit_t ), pointer :: c1, clone

  tester = .describe. "nombre_derived_unit_clone"

  c1 => kilogram * meter
  c1%prefix = kilo
  c1%symb = 'c1'
  c1%conv = 1.23d0
  c1%dim = dimid%mass
  c1%pow = 2.34d0

  clone => .clone. c1

  call tester%expect( clone%prefix == kilo .toBe. .true. .hint. 'prefix' )
  call tester%expect( clone%symb .toBe. 'c1' .hint. 'symbol' )
  call tester%expect( clone%conv .toBe. 1.23d0 .hint. 'conversion factor' )
  call tester%expect( clone%dim == dimid%mass .toBe. .true. .hint. 'dimension' )
  call tester%expect( clone%pow .toBe. 2.34d0 .hint. 'power' )

  call tester%expect( clone%head == kilogram .toBe. .true. .hint. 'kg' )
  call tester%expect( associated( clone%head%prev ) .toBe. .false. .hint. 'kg prev' )

  call tester%expect( clone%head%next == meter .toBe. .true. .hint. 'meter' )
  call tester%expect( clone%head%next%prev == kilogram .toBe. .true. .hint. 'meter prev' )
  call tester%expect( associated( clone%head%next%next ) .toBe. .false. .hint. 'meter next' )

  failed = tester%failed()
end function rhyme_nombre_derived_unit_clone_test
