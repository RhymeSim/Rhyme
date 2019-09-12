logical function rhyme_nombre_derived_unit_equality_test () result ( failed )
  use rhyme_nombre_derived_unit_factory
  use rhyme_assertion

  implicit none

  type ( assertion_t ) :: tester

  type ( nombre_derived_unit_t ), pointer :: joule, joule_cloned, joule_modified, newton
  type ( nombre_derived_unit_t ), pointer :: du1, du2
  type ( nombre_base_unit_t ), pointer :: old_head

  tester = .describe. "nombre_derived_unit_equality"

  joule => nom_du_factory%generate( [ kilogram, meter**2, second**(-2) ], 'J' )
  newton => nom_du_factory%generate( [ kilogram, meter, second**(-2) ], 'N')

  call tester%expect( joule == joule .toBe. .true. .hint. 'j == j' )
  call tester%expect( newton == newton .toBe. .true. .hint. 'newton == newton' )
  call tester%expect( joule == newton .toBe. .false. .hint. 'j == newton' )
  call tester%expect( newton == joule .toBe. .false. .hint. 'newton == j' )

  joule => nom_du_factory%generate( [ kilogram, meter**2, second**(-2) ], 'J' )
  joule_cloned => .clone. joule
  call tester%expect( joule == joule_cloned .toBe. .true. .hint. 'j == j_cloned' )

  joule => nom_du_factory%generate( [ kilogram, meter**2, second**(-2) ], 'J' )
  joule_modified => .clone. joule
  joule_modified%prefix = kilo
  call tester%expect( joule == joule_modified .toBe. .false. .hint. 'j == j_modified prefix' )

  ! NB: Comparison ignores the symbols
  joule => nom_du_factory%generate( [ kilogram, meter**2, second**(-2) ], 'J' )
  joule_modified => .clone. joule
  joule_modified%symb = 'modified'
  call tester%expect( joule == joule_modified .toBe. .true. .hint. 'j == j_modified symb' )

  joule => nom_du_factory%generate( [ kilogram, meter**2, second**(-2) ], 'J' )
  joule_modified => .clone. joule
  joule_modified%conv = 12.3d0
  call tester%expect( joule == joule_modified .toBe. .false. .hint. 'j == j_modified conv' )

  joule => nom_du_factory%generate( [ kilogram, meter**2, second**(-2) ], 'J' )
  joule_modified => .clone. joule
  old_head => joule_modified%head
  joule_modified%head => .clone. meter
  joule_modified%head%next => old_head
  joule_modified%head%next%prev => joule_modified%head
  call tester%expect( joule == joule_modified .toBe. .false. .hint. 'j == j_modified dim' )

  joule => nom_du_factory%generate( [ kilogram, meter**2, second**(-2) ], 'J' )
  joule_modified => .clone. joule
  joule_modified%pow = 23.4d0
  call tester%expect( joule == joule_modified .toBe. .false. .hint. 'j == j_modified pow' )

  du1 => nom_du_factory%generate( [ kilogram, meter, second**(-2) ], symb='', pow=1d0)
  du2 => nom_du_factory%generate( [ kilogram**(-1), meter**(-1), second**2 ], symb='', pow=-1d0)
  call tester%expect( du1 == du2 .toBe. .true. .hint. 'du1 == du2' )

  failed = tester%failed()
end function rhyme_nombre_derived_unit_equality_test
