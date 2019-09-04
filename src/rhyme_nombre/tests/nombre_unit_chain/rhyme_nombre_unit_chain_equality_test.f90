logical function rhyme_nombre_unit_chain_equality_test () result ( failed )
  use rhyme_nombre_unit_chain
  use rhyme_assertion

  implicit none

  type ( assertion_t ) :: tester

  type ( nombre_unit_chain_t ), pointer :: joule, joule_cloned, joule_modified, rad

  tester = .describe. "nombre_unit_chain_equality"

  joule => kg * meter**2 / sec**2 .as. 'J'
  rad => meter / meter .as. 'rad'
  call tester%expect( joule == joule .toBe. .true. .hint. 'joule == joule' )
  call tester%expect( rad == rad .toBe. .true. .hint. 'rad == rad' )
  call tester%expect( joule == rad .toBe. .false. .hint. 'joule == rad' )
  call tester%expect( rad == joule .toBe. .false. .hint. 'rad == joule' )

  joule_cloned => rhyme_nombre_unit_chain_clone( joule )
  call tester%expect( joule == joule_cloned .toBe. .true. .hint. 'joule == joule_cloned' )

  joule_modified => rhyme_nombre_unit_chain_clone( joule )
  joule_modified%prefix = kilo
  call tester%expect( joule == joule_modified .toBe. .false. .hint. 'joule == joule_modified prefix' )

  joule_modified => rhyme_nombre_unit_chain_clone( joule )
  joule_modified%symb = 'modified'
  call tester%expect( joule == joule_modified .toBe. .false. .hint. 'joule == joule_modified symb' )

  joule_modified => rhyme_nombre_unit_chain_clone( joule )
  joule_modified%conv = 12.3d0
  call tester%expect( joule == joule_modified .toBe. .false. .hint. 'joule == joule_modified conv' )

  joule_modified => rhyme_nombre_unit_chain_clone( joule )
  joule_modified%dim = dimid%null
  call tester%expect( joule == joule_modified .toBe. .false. .hint. 'joule == joule_modified dim' )

  joule_modified => rhyme_nombre_unit_chain_clone( joule )
  joule_modified%pow = 23.4d0
  call tester%expect( joule == joule_modified .toBe. .false. .hint. 'joule == joule_modified pow' )

  failed = tester%failed()
end function rhyme_nombre_unit_chain_equality_test
