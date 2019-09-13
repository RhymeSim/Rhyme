logical function rhyme_nombre_derived_unit_equality_test () result ( failed )
  use rhyme_nombre_derived_unit_factory
  use rhyme_assertion

  implicit none

  type ( assertion_t ) :: tester

  type ( nombre_derived_unit_t ), pointer :: j, j_cloned, j_modified, n
  type ( nombre_derived_unit_t ), pointer :: du1, du2
  type ( nombre_base_unit_t ), pointer :: old_head

  tester = .describe. "nombre_derived_unit_equality"

  j => nom_du_factory%generate( [ kilogram, meter**2, second**(-2) ], 'J' )
  n => nom_du_factory%generate( [ kilogram, meter, second**(-2) ], 'N')

  call tester%expect( j == j .toBe. .true. .hint. 'j == j' )
  call tester%expect( n == n .toBe. .true. .hint. 'n == n' )
  call tester%expect( j == n .toBe. .false. .hint. 'j == n' )
  call tester%expect( n == j .toBe. .false. .hint. 'n == j' )

  j => nom_du_factory%generate( [ kilogram, meter**2, second**(-2) ], 'J' )
  j_cloned => .clone. j
  call tester%expect( j == j_cloned .toBe. .true. .hint. 'j == j_cloned' )

  j => nom_du_factory%generate( [ kilogram, meter**2, second**(-2) ], 'J' )
  j_modified => .clone. j
  j_modified%prefix = kilo
  call tester%expect( j == j_modified .toBe. .false. .hint. 'j == j_modified prefix' )

  ! NB: Comparison ignores the symbols
  j => nom_du_factory%generate( [ kilogram, meter**2, second**(-2) ], 'J' )
  j_modified => .clone. j
  j_modified%symb = 'modified'
  call tester%expect( j == j_modified .toBe. .true. .hint. 'j == j_modified symb' )

  j => nom_du_factory%generate( [ kilogram, meter**2, second**(-2) ], 'J' )
  j_modified => .clone. j
  j_modified%conv = 12.3d0
  call tester%expect( j == j_modified .toBe. .false. .hint. 'j == j_modified conv' )

  j => nom_du_factory%generate( [ kilogram, meter**2, second**(-2) ], 'J' )
  j_modified => .clone. j
  old_head => j_modified%head
  j_modified%head => .clone. meter
  j_modified%head%next => old_head
  j_modified%head%next%prev => j_modified%head
  call tester%expect( j == j_modified .toBe. .false. .hint. 'j == j_modified dim' )

  j => nom_du_factory%generate( [ kilogram, meter**2, second**(-2) ], 'J' )
  j_modified => .clone. j
  j_modified%pow = 23.4d0
  call tester%expect( j == j_modified .toBe. .false. .hint. 'j == j_modified pow' )

  du1 => nom_du_factory%generate( [ kilogram, meter, second**(-2) ], symb='', pow=1d0)
  du2 => nom_du_factory%generate( [ kilogram**(-1), meter**(-1), second**2 ], symb='', pow=-1d0)
  call tester%expect( du1 == du2 .toBe. .true. .hint. 'du1 == du2' )

  failed = tester%failed()
end function rhyme_nombre_derived_unit_equality_test
