logical function rhyme_nombre_units_handle_mul_div_test () result (failed)
  use rhyme_nombre_units
  use rhyme_assertion

  implicit none

  type ( assertion_t ) :: n_tester

  type ( nombre_unit_t ), pointer :: kgK, m_s2, kgm2, Ks3

  n_tester = .describe. "nombre_units_handle_mul_div"

  kgK => kg * Kel
  m_s2 => (meter / sec)**2
  kgm2 => (kg * meter)**2
  Ks3 => (Kel * sec)**3

  call handle_mul_div( kgK, m_s2, "*" )
  kgK => nombre_unit_head( kgK )

  call n_tester%expect( kgK%symb .toBe. "g" )
  call n_tester%expect( kgK%next%symb .toBe. "K" )
  call n_tester%expect( kgK%next%next%symb .toBe. "m" )
  call n_tester%expect( kgK%next%next%next%symb .toBe. "s" )
  call n_tester%expect( associated(kgK%next%next%next%next) .toBe. .false. )

  call handle_mul_div( kgm2, Ks3, "/" )
  kgm2 => nombre_unit_head( kgm2 )

  call n_tester%expect( kgm2%symb .toBe. "g" )
  call n_tester%expect( kgm2%next%symb .toBe. "m" )
  call n_tester%expect( kgm2%next%next%symb .toBe. "K" )
  call n_tester%expect( kgm2%next%next%next%symb .toBe. "s" )
  call n_tester%expect( associated(kgm2%next%next%next%next) .toBe. .false. )
  call n_tester%expect( kgm2%next%next%pow .toBe. (-3.0) )
  call n_tester%expect( kgm2%next%next%next%pow .toBe. (-3.0) )

  failed = n_tester%failed()
end function rhyme_nombre_units_handle_mul_div_test
