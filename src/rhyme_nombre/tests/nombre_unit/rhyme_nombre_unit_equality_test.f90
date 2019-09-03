logical function rhyme_nombre_unit_equality_test () result ( failed )
  use rhyme_nombre_unit
  use rhyme_assertion

  implicit none

  type ( assertion_t ) :: tester

  tester = .describe. "nombre_unit_equality"

  call tester%expect( kg == meter .toBe. .false. .hint. 'kg == meter' )
  call tester%expect( kel == ampere .toBe. .false. .hint. 'kel == ampere' )
  call tester%expect( sec == mol .toBe. .false. .hint. 'sec == mol' )
  call tester%expect( gram == kg .toBe. .false. .hint. 'gram == kg' )

  call tester%expect( gram == gram .toBe. .true. .hint. 'gram == gram' )
  call tester%expect( kg == kg .toBe. .true. .hint. 'kg == kg' )
  call tester%expect( meter == meter .toBe. .true. .hint. 'meter == meter' )
  call tester%expect( sec == sec .toBe. .true. .hint. 'sec == sec' )
  call tester%expect( kel == kel .toBe. .true. .hint. 'kel == kel' )
  call tester%expect( ampere == ampere .toBe. .true. .hint. 'ampere == ampere' )
  call tester%expect( mol == mol .toBe. .true. .hint. 'mol == mol' )

  failed = tester%failed()
end function rhyme_nombre_unit_equality_test
