logical function rhyme_nombre_unit_chain_equality_test () result ( failed )
  use rhyme_nombre_unit_chain
  use rhyme_assertion

  implicit none

  type ( assertion_t ) :: tester

  type ( nombre_unit_chain_t ), pointer :: j, j_cloned, j_modified, rad

  tester = .describe. "nombre_unit_chain_equality"

  j => kilogram * meter**2 / second**2 .as. 'J'
  rad => meter / meter .as. 'rad'
  call tester%expect( j == j .toBe. .true. .hint. 'j == j' )
  call tester%expect( rad == rad .toBe. .true. .hint. 'rad == rad' )
  call tester%expect( j == rad .toBe. .false. .hint. 'j == rad' )
  call tester%expect( rad == j .toBe. .false. .hint. 'rad == j' )

  j_cloned => rhyme_nombre_unit_chain_clone( j )
  call tester%expect( j == j_cloned .toBe. .true. .hint. 'j == j_cloned' )

  j_modified => rhyme_nombre_unit_chain_clone( j )
  j_modified%prefix = kilo
  call tester%expect( j == j_modified .toBe. .false. .hint. 'j == j_modified prefix' )

  j_modified => rhyme_nombre_unit_chain_clone( j )
  j_modified%symb = 'modified'
  call tester%expect( j == j_modified .toBe. .false. .hint. 'j == j_modified symb' )

  j_modified => rhyme_nombre_unit_chain_clone( j )
  j_modified%conv = 12.3d0
  call tester%expect( j == j_modified .toBe. .false. .hint. 'j == j_modified conv' )

  j_modified => rhyme_nombre_unit_chain_clone( j )
  j_modified%dim = dimid%null
  call tester%expect( j == j_modified .toBe. .false. .hint. 'j == j_modified dim' )

  j_modified => rhyme_nombre_unit_chain_clone( j )
  j_modified%pow = 23.4d0
  call tester%expect( j == j_modified .toBe. .false. .hint. 'j == j_modified pow' )

  failed = tester%failed()
end function rhyme_nombre_unit_chain_equality_test
