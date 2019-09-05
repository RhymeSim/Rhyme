logical function rhyme_nombre_unit_chain_mul_test () result ( failed )
  use rhyme_nombre_unit_chain
  use rhyme_assertion

  implicit none

  type ( assertion_t ) :: tester
  type ( nombre_unit_chain_t ), pointer :: c1, c2, c3, c4
  type ( nombre_unit_chain_t ), pointer :: pc, m_sun, yr

  tester = .describe. "nombre_unit_chain_mul"

  c1 => kilogram * meter
  call tester%expect( c1%head == kilogram .toBe. .true. .hint. 'c1 head to be kg' )
  call tester%expect( associated( c1%head%prev ) .toBe. .false. .hint. 'c1 head%prev to be null' )
  call tester%expect( c1%head%next == meter .toBe. .true. .hint. 'c1 next to be meter' )
  call tester%expect( c1%head%next%prev == kilogram .toBe. .true. .hint. 'c1 next%prev to be kg' )
  call tester%expect( associated( c1%head%next%next ) .toBe. .false. .hint. 'c1 next%next to be null' )

  call tester%expect( c1%dim == rhyme_nombre_unit_chain_get_dim( c1 ) .toBe. .true. .hint. 'c1 unit' )

  c2 => c1 * second
  call tester%expect( c1%head == kilogram .toBe. .true. .hint. 'c1 head to be kg' )
  call tester%expect( associated( c1%head%prev ) .toBe. .false. .hint. 'c1 head%prev to be null' )
  call tester%expect( c1%head%next == meter .toBe. .true. .hint. 'c1 next to be meter' )
  call tester%expect( c1%head%next%prev == kilogram .toBe. .true. .hint. 'c1 next%prev to be kg' )
  call tester%expect( associated( c1%head%next%next ) .toBe. .false. .hint. 'c1 next%next to be null' )

  call tester%expect( c2%head == kilogram .toBe. .true. .hint. 'c2 head to be kg' )
  call tester%expect( associated( c2%head%prev ) .toBe. .false. .hint. 'c2 head%prev to be null' )
  call tester%expect( c2%head%next == meter .toBe. .true. .hint. 'c2 next to be meter' )
  call tester%expect( c2%head%next%prev == kilogram .toBe. .true. .hint. 'c2 next%prev to be kg' )
  call tester%expect( c2%head%next%next == second .toBe. .true. .hint. 'c2 next%next to be sec' )
  call tester%expect( c2%head%next%next%prev == meter .toBe. .true. .hint. 'c2 next%next%prev to be meter' )

  c1%pow = 5d-1
  c3 => c1 * kelvin
  call tester%expect( c3%head%next%next == kelvin**2 .toBe. .true. .hint. 'c3 next%next to be kel' )
  call tester%expect( c3%head%next%next%pow .toBe. 2d0 .hint. 'c3 next%next%pow to be 2d0' )
  call tester%expect( c3%head%next%next%prev == meter .toBe. .true. .hint. 'c3 next%next%prev to be meter' )

  call tester%expect( c3%dim == rhyme_nombre_unit_chain_get_dim( c3 ) .toBe. .true. .hint. 'c2 unit' )

  c4 => c1 * c2 * c3
  call tester%expect( c4 == c1 .toBe. .true. .hint. 'c4' )
  call tester%expect( c4%next == c2 .toBe. .true. .hint. 'c4%next' )
  call tester%expect( c4%next%next == c3 .toBe. .true. .hint. 'c4%next%next' )
  call tester%expect( associated( c4%prev ) .toBe. .false. .hint. 'c4%prev' )
  call tester%expect( associated( c4%next%next%next ) .toBe. .false. .hint. 'c4%next%next%next' )

  pc => 3.086d16 * meter
  call tester%expect( pc%conv .toBe. 3.086d16 .hint. 'pc conv' )
  call tester%expect( pc%head == meter .toBe. .true. .hint. 'pc head' )
  call tester%expect( associated( pc%head%next ) .toBe. .false. .hint. 'pc head%next' )

  yr => 3.154e7 * second
  call tester%expect( yr%conv .toBe. 3.154e7 .hint. 'yr conv' )
  call tester%expect( yr%head == second .toBe. .true. .hint. 'yr head' )
  call tester%expect( associated( yr%head%next ) .toBe. .false. .hint. 'yr head%next' )

  m_sun => 3 * kilogram
  call tester%expect( m_sun%conv .toBe. 3d0 .hint. 'm_sun conv' )
  call tester%expect( m_sun%head == kilogram .toBe. .true. .hint. 'm_sun head' )
  call tester%expect( associated( m_sun%head%next ) .toBe. .false. .hint. 'm_sun head%next' )

  failed = tester%failed()
end function rhyme_nombre_unit_chain_mul_test
