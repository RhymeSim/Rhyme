logical function rhyme_nombre_units_print_dim_test () result ( failed )
  use rhyme_nombre_units
  use rhyme_assertion

  implicit none

  type ( assertion_t ) :: tester

  type ( nombre_unit_t ), pointer :: u
  character ( len=64 ) :: str

  tester = .describe. "nombre_units_print_dim"

  call rhyme_nombre_units_init

  u => rhyme_nombre_units_head( kg * meter**2 / sec**2.001 )
  str = rhyme_nombre_units_print_dim( u )

  call tester%expect( str .toBe. 'M L^2 T^-2.0' )

  failed = tester%failed()
end function rhyme_nombre_units_print_dim_test
