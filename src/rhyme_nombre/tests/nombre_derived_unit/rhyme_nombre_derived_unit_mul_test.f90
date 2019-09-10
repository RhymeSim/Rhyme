logical function rhyme_nombre_derived_unit_mul_test () result ( failed )
  use rhyme_nombre_derived_unit_factory
  use rhyme_assertion

  implicit none

  type ( assertion_t ) :: tester

  type ( nombre_derived_unit_t ), pointer :: derived_unit
  type ( nombre_derived_unit_t ), pointer :: ic, rc, r8c, pc
  type ( nombre_derived_unit_t ), pointer :: iu, ru, r8u, uu

  tester = .describe. "nombre_derived_unit_mul"

  derived_unit => nom_du_factory%generate( [ kilogram, meter, second**(-2) ], 'N')

  ic => 123 * derived_unit
  call tester%expect( ic%prefix == null_prefix .toBe. .true. .hint. 'ic conv' )
  call tester%expect( ic%conv .toBe. 1.23d2 .hint. 'ic conv' )
  call tester%expect( ic%head == kilogram .toBe. .true. .hint. 'ic kg' )
  call tester%expect( ic%head%next == meter .toBe. .true. .hint. 'ic m' )
  call tester%expect( ic%head%next%next == second**(-2) .toBe. .true. .hint. 'ic s^-2' )
  call tester%expect( associated( ic%head%next%next%next ) .toBe. .false. .hint. 'ic null' )

  rc => 2.34e5 * derived_unit
  call tester%expect( rc%conv .toBe. 2.34e5 .hint. 'rc conv' )

  r8c => 3.45d2 * derived_unit
  call tester%expect( r8c%conv .toBe. 3.45d2 .hint. 'r8c conv' )

  pc => kilo * derived_unit
  call tester%expect( pc%prefix == kilo .toBe. .true. .hint. 'pc prefix' )
  call tester%expect( pc%conv .toBe. 1d0 .hint. 'pc conv' )

  iu => 1000 * kilogram
  call tester%expect( iu%conv .toBe. 1e3 .hint. 'iu conv' )
  call tester%expect( iu%head == kilogram .toBe. .true. .hint. 'iu head' )
  call tester%expect( associated( iu%head%next ) .toBe. .false. .hint. 'iu head%next' )

  ru => 3.154e7 * second
  call tester%expect( ru%conv .toBe. 3.154e7 .hint. 'ru conv' )
  call tester%expect( ru%head == second .toBe. .true. .hint. 'ru head' )
  call tester%expect( associated( ru%head%next ) .toBe. .false. .hint. 'ru head%next' )

  r8u => 3.086d16 * meter
  call tester%expect( r8u%conv .toBe. 3.086d16 .hint. 'r8u conv' )
  call tester%expect( r8u%head == meter .toBe. .true. .hint. 'r8u head' )
  call tester%expect( associated( r8u%head%next ) .toBe. .false. .hint. 'r8u head%next' )

  uu => kilogram * meter
  call tester%expect( uu%head == kilogram .toBe. .true. .hint. 'uu head to be kg' )
  call tester%expect( associated( uu%head%prev ) .toBe. .false. .hint. 'uu head%prev to be null' )
  call tester%expect( uu%head%next == meter .toBe. .true. .hint. 'uu next to be meter' )
  call tester%expect( uu%head%next%prev == kilogram .toBe. .true. .hint. 'uu next%prev to be kg' )
  call tester%expect( associated( uu%head%next%next ) .toBe. .false. .hint. 'uu next%next to be null' )

  call tester%expect( uu%dim == rhyme_nombre_derived_unit_get_dim( uu ) .toBe. .true. .hint. 'uu unit' )

  failed = tester%failed()
end function rhyme_nombre_derived_unit_mul_test
