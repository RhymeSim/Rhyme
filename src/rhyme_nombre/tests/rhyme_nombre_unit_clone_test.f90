logical function rhyme_nombre_unit_clone_test () result ( failed )
  use rhyme_nombre_unit
  use rhyme_assertion

  implicit none

  type ( assertion_t ) :: tester

  type ( nombre_unit_t ) :: kg
  type ( nombre_unit_t ), pointer :: kg_cloned

  tester = .describe. "nombre_unit_clone"

  kg = nombre_unit_t( kilo, "g", 1.d0, dimid%mass )

  kg_cloned => rhyme_nombre_unit_clone( kg )

  call tester%expect( kg_cloned%prefix%symb .toBe. 'k' )
  call tester%expect( kg_cloned%symb .toBe. 'g' )
  call tester%expect( kg_cloned%conv .toBe. 1d0 )
  call tester%expect( kg_cloned%dim%powers .toBe. dimid%mass%powers )
  call tester%expect( kg_cloned%dim%symb .toBe. dimid%mass%symb )
  call tester%expect( kg_cloned%pow .toBe. 1d0 )

  failed = tester%failed()
end function rhyme_nombre_unit_clone_test
