logical function rhyme_nombre_derived_unit_div_test () result ( failed )
  use rhyme_nombre_derived_unit
  use rhyme_assertion

  implicit none

  type ( assertion_t ) :: tester
  type ( nombre_derived_unit_t ), pointer :: uu, iu, ru, r8u

  tester = .describe. "nombre_derived_unit_div"

  uu => meter / second
  call tester%expect( uu%head == meter .toBe. .true. .hint. 'uu head' )
  call tester%expect( associated( uu%head%prev ) .toBe. .false. .hint. 'uu head%prev' )
  call tester%expect( uu%head%next == second**(-1) .toBe. .true. .hint. 'uu head%next' )
  call tester%expect( uu%head%next%prev == meter .toBe. .true. .hint. 'uu head%next%prev' )
  call tester%expect( associated( uu%head%next%next ) .toBe. .false. .hint. 'uu head%next%next' )

  call tester%expect( uu%dim == rhyme_nombre_derived_unit_get_dim( uu ) .toBe. .true. .hint. 'uu dim')

  iu => 2 / second
  call tester%expect( iu%conv .toBe. 2d0 .hint. 'iu conv')
  call tester%expect( iu%head == second**(-1) .toBe. .true. .hint. 'iu head')
  call tester%expect( associated( iu%head%next) .toBe. .false. .hint. 'iu head%next to be null')

  call tester%expect( iu%dim == rhyme_nombre_derived_unit_get_dim( iu ) .toBe. .true. .hint. 'iu dim')

  ru => 1.23e0 / second
  call tester%expect( ru%conv .toBe. 1.23e0 .hint. 'ru conv')
  call tester%expect( ru%head == second**(-1) .toBe. .true. .hint. 'ru head')
  call tester%expect( associated( ru%head%next) .toBe. .false. .hint. 'ru head%next to be null')

  call tester%expect( ru%dim == rhyme_nombre_derived_unit_get_dim( ru ) .toBe. .true. .hint. 'ru dim')

  r8u => 2.34d0 / second
  call tester%expect( r8u%conv .toBe. 2.34d0 .hint. 'r8u conv')
  call tester%expect( r8u%head == second**(-1) .toBe. .true. .hint. 'r8u head')
  call tester%expect( associated( r8u%head%next) .toBe. .false. .hint. 'r8u head%next to be null')

  call tester%expect( r8u%dim == rhyme_nombre_derived_unit_get_dim( r8u ) .toBe. .true. .hint. 'r8u dim')

  failed = tester%failed()
end function rhyme_nombre_derived_unit_div_test
