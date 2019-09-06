logical function rhyme_nombre_base_unit_equality_test () result ( failed )
  use rhyme_nombre_base_unit
  use rhyme_assertion

  implicit none

  type ( assertion_t ) :: tester

  tester = .describe. "nombre_base_unit_equality"

  call tester%expect( kilogram == meter .toBe. .false. .hint. 'kg == meter' )
  call tester%expect( kelvin == ampere .toBe. .false. .hint. 'kel == ampere' )
  call tester%expect( second == mole .toBe. .false. .hint. 'sec == mol' )
  call tester%expect( gram == kilogram .toBe. .false. .hint. 'gram == kg' )

  call tester%expect( gram == gram .toBe. .true. .hint. 'gram == gram' )
  call tester%expect( kilogram == kilogram .toBe. .true. .hint. 'kg == kg' )
  call tester%expect( meter == meter .toBe. .true. .hint. 'meter == meter' )
  call tester%expect( second == second .toBe. .true. .hint. 'sec == sec' )
  call tester%expect( kelvin == kelvin .toBe. .true. .hint. 'kel == kel' )
  call tester%expect( ampere == ampere .toBe. .true. .hint. 'ampere == ampere' )
  call tester%expect( mole == mole .toBe. .true. .hint. 'mol == mol' )

  failed = tester%failed()
end function rhyme_nombre_base_unit_equality_test
