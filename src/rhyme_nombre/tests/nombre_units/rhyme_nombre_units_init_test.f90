logical function rhyme_nombre_units_init_test () result ( failed )
  use rhyme_nombre_units
  use rhyme_assertion

  implicit none

  type ( assertion_t ) :: tester

  tester = .describe. "units_init"

  call rhyme_nombre_units_init

  call rhyme_nombre_units_init

  call tester%expect( kg%prefix%base_10 .toBe. 3 .hint. 'kg prefix' )
  call tester%expect( kg%symb .toBe. 'g' .hint. 'kg symbol' )

  call tester%expect( m_sun%conv .toBe. 1.9885d33 .hint. 'm_sun conv' )
  call tester%expect( m_sun%symb .toBe. 'Msun' .hint. 'm_sun symbol' )

  call tester%expect( m_h%conv .toBe. 1.6735575d-27 .hint. 'm_H conv' )
  call tester%expect( m_h%symb .toBe. 'm_H' .hint. 'm_H symbol' )

  call tester%expect( amu%conv .toBe. 1.6605d-27 .hint. 'amu conv' )
  call tester%expect( amu%symb .toBe. 'amu' .hint. 'amu symbol' )

  call tester%expect( pc%conv .toBe. 3.086d16 .hint. 'pc conv' )
  call tester%expect( pc%symb .toBe. 'pc' .hint. 'pc symbol' )

  call tester%expect( ly%conv .toBe. 9.461d15 .hint. 'ly conv' )
  call tester%expect( ly%symb .toBe. 'ly' .hint. 'ly symbol' )

  call tester%expect( au%conv .toBe. 1.496d11 .hint. 'AU conv' )
  call tester%expect( au%symb .toBe. 'AU' .hint. 'AU symbol' )

  call tester%expect( yr%conv .toBe. 3.154d7 .hint. 'yr conv' )
  call tester%expect( yr%symb .toBe. 'yr' .hint. 'yr symbol' )

  failed = tester%failed()
end function rhyme_nombre_units_init_test
