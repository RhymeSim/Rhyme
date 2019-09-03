logical function rhyme_nombre_unit_chain_mul_test () result ( failed )
  use rhyme_nombre_unit_chain
  use rhyme_assertion

  implicit none

  type ( assertion_t ) :: tester
  type ( nombre_unit_chain_t ), pointer :: c1, c2

  tester = .describe. "nombre_unit_chain_mul"

  c1 => kg * meter
  call tester%expect( c1%head == kg .toBe. .true. .hint. 'head to be kg' )
  call tester%expect( associated( c1%head%prev ) .toBe. .false. .hint. 'head%prev to be null' )
  call tester%expect( c1%head%next == meter .toBe. .true. .hint. 'next to be meter' )
  call tester%expect( c1%head%next%prev == kg .toBe. .true. .hint. 'next%prev to be kg' )
  call tester%expect( associated( c1%head%next%next ) .toBe. .false. .hint. 'next%next to be null' )

  c2 => c1 * sec
  call tester%expect( c1%head == kg .toBe. .true. .hint. 'head to be kg' )
  call tester%expect( associated( c1%head%prev ) .toBe. .false. .hint. 'head%prev to be null' )
  call tester%expect( c1%head%next == meter .toBe. .true. .hint. 'next to be meter' )
  call tester%expect( c1%head%next%prev == kg .toBe. .true. .hint. 'next%prev to be kg' )
  call tester%expect( associated( c1%head%next%next ) .toBe. .false. .hint. 'next%next to be null' )

  call tester%expect( c2%head == kg .toBe. .true. .hint. 'head to be kg' )
  call tester%expect( associated( c2%head%prev ) .toBe. .false. .hint. 'head%prev to be null' )
  call tester%expect( c2%head%next == meter .toBe. .true. .hint. 'next to be meter' )
  call tester%expect( c2%head%next%prev == kg .toBe. .true. .hint. 'next%prev to be kg' )
  call tester%expect( c2%head%next%next == sec .toBe. .true. .hint. 'next%next to be sec' )
  call tester%expect( c2%head%next%next%prev == meter .toBe. .true. .hint. 'next%next%prev to be meter' )

  failed = tester%failed()
end function rhyme_nombre_unit_chain_mul_test
