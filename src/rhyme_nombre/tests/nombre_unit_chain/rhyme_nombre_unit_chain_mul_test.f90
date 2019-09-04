logical function rhyme_nombre_unit_chain_mul_test () result ( failed )
  use rhyme_nombre_unit_chain
  use rhyme_assertion

  implicit none

  type ( assertion_t ) :: tester
  type ( nombre_unit_chain_t ), pointer :: c1, c2

  tester = .describe. "nombre_unit_chain_mul"

  c1 => kg * meter
  call tester%expect( c1%head == kg .toBe. .true. .hint. 'c1 head to be kg' )
  call tester%expect( associated( c1%head%prev ) .toBe. .false. .hint. 'c1 head%prev to be null' )
  call tester%expect( c1%head%next == meter .toBe. .true. .hint. 'c1 next to be meter' )
  call tester%expect( c1%head%next%prev == kg .toBe. .true. .hint. 'c1 next%prev to be kg' )
  call tester%expect( associated( c1%head%next%next ) .toBe. .false. .hint. 'c1 next%next to be null' )

  call tester%expect( c1%dim == rhyme_nombre_unit_chain_get_dim( c1 ) .toBe. .true. .hint. 'c1 unit' )

  c2 => c1 * sec
  call tester%expect( c1%head == kg .toBe. .true. .hint. 'c1 head to be kg' )
  call tester%expect( associated( c1%head%prev ) .toBe. .false. .hint. 'c1 head%prev to be null' )
  call tester%expect( c1%head%next == meter .toBe. .true. .hint. 'c1 next to be meter' )
  call tester%expect( c1%head%next%prev == kg .toBe. .true. .hint. 'c1 next%prev to be kg' )
  call tester%expect( associated( c1%head%next%next ) .toBe. .false. .hint. 'c1 next%next to be null' )

  call tester%expect( c2%head == kg .toBe. .true. .hint. 'c2 head to be kg' )
  call tester%expect( associated( c2%head%prev ) .toBe. .false. .hint. 'c2 head%prev to be null' )
  call tester%expect( c2%head%next == meter .toBe. .true. .hint. 'c2 next to be meter' )
  call tester%expect( c2%head%next%prev == kg .toBe. .true. .hint. 'c2 next%prev to be kg' )
  call tester%expect( c2%head%next%next == sec .toBe. .true. .hint. 'c2 next%next to be sec' )
  call tester%expect( c2%head%next%next%prev == meter .toBe. .true. .hint. 'c2 next%next%prev to be meter' )

  c1%pow = 5d-1
  c2 => c1 * kel
  call tester%expect( c2%head%next%next == kel**2 .toBe. .true. .hint. 'c2 next%next to be kel' )
  call tester%expect( c2%head%next%next%pow .toBe. 2d0 .hint. 'c2 next%next%pow to be 2d0' )
  call tester%expect( c2%head%next%next%prev == meter .toBe. .true. .hint. 'c2 next%next%prev to be meter' )
  
  call tester%expect( c2%dim == rhyme_nombre_unit_chain_get_dim( c2 ) .toBe. .true. .hint. 'c2 unit' )

  failed = tester%failed()
end function rhyme_nombre_unit_chain_mul_test
