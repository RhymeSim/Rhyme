logical function rhyme_nombre_derived_unit_equality_test () result ( failed )
  use rhyme_nombre_derived_unit_factory
  use rhyme_assertion

  implicit none

  type ( assertion_t ) :: tester

  type ( nombre_derived_unit_t ), pointer :: joule, joule_cloned, joule_modified, newton

  tester = .describe. "nombre_derived_unit_equality"

  joule => nom_du_factory%generate( [ kilogram, meter**2, second**(-2) ], 'J' )
  newton => nom_du_factory%generate( [ kilogram, meter, second**(-2) ], 'N')

  call tester%expect( joule == joule .toBe. .true. .hint. 'j == j' )
  call tester%expect( newton == newton .toBe. .true. .hint. 'newton == newton' )
  call tester%expect( joule == newton .toBe. .false. .hint. 'j == newton' )
  call tester%expect( newton == joule .toBe. .false. .hint. 'newton == j' )

  joule_cloned => .clone. joule
  call tester%expect( joule == joule_cloned .toBe. .true. .hint. 'j == j_cloned' )

  joule_modified => .clone. joule
  joule_modified%prefix = kilo
  call tester%expect( joule == joule_modified .toBe. .false. .hint. 'j == j_modified prefix' )

  joule_modified => .clone. joule
  joule_modified%symb = 'modified'
  call tester%expect( joule == joule_modified .toBe. .false. .hint. 'j == j_modified symb' )

  joule_modified => .clone. joule
  joule_modified%conv = 12.3d0
  call tester%expect( joule == joule_modified .toBe. .false. .hint. 'j == j_modified conv' )

  joule_modified => .clone. joule
  joule_modified%dim = dimid%null
  call tester%expect( joule == joule_modified .toBe. .false. .hint. 'j == j_modified dim' )

  joule_modified => .clone. joule
  joule_modified%pow = 23.4d0
  call tester%expect( joule == joule_modified .toBe. .false. .hint. 'j == j_modified pow' )

  failed = tester%failed()
end function rhyme_nombre_derived_unit_equality_test
