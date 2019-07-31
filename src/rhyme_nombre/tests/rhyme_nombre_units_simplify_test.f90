logical function rhyme_nombre_units_simplify_test () result ( failed )
  use rhyme_nombre_units
  use rhyme_assertion

  implicit none

  type ( assertion_t ) :: tester

  type ( nombre_unit_t ), pointer :: u, u_exp, u_simp

  tester = .describe. "nombre_units_simplify"

  u => rhyme_nombre_units_head( kg**2 * sec / kg**5 * kel / (sec**2 * kel**3)**2 )
  u_exp => rhyme_nombre_units_head( kg**(-3) * sec**(-3) * kel**(-5) )

  u_simp = rhyme_nombre_units_simplify( u )

  call tester%expect( u_simp .unitEqualsTo. u_exp .toBe. .true. )

  failed = tester%failed()
end function rhyme_nombre_units_simplify_test
