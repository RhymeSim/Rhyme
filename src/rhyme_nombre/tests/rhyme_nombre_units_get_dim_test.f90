logical function rhyme_nombre_units_get_dim_test () result ( failed )
  use rhyme_nombre_units
  use rhyme_assertion

  implicit none

  type ( assertion_t ) :: tester

  type ( nombre_dimension_t ) :: dim
  type ( nombre_unit_t ), pointer :: u

  tester = .describe. "nombre_units_get_dim"

  u => kg * meter / sec**2

  dim = rhyme_nombre_units_get_dim( u )

  call tester%expect( dim%powers .toBe. [ 1d0, 1d0, -2d0, 0d0, 0d0, 0d0, 0d0 ] )

  failed = tester%failed()
end function rhyme_nombre_units_get_dim_test
