logical function rhyme_units_handle_pow_test () result (failed)
  use rhyme_units
  use rhyme_assertion

  implicit none

  type ( assertion_t ) :: n_tester

  type(unit_t), pointer :: u

  n_tester = .describe. "nombre_units_handle_pow"

  u => kg * meter / sec

  call handle_pow(u, 2.0)
  u => unit_head(u)

  call n_tester%expect( u%pow .toBe. 2.d0 )
  call n_tester%expect( u%next%pow .toBe. 2.d0 )
  call n_tester%expect( u%next%next%pow .toBe. (-2.d0) )
  call n_tester%expect( associated(u%next%next%next) .toBe. .false. )

  failed = n_tester%failed()
end function rhyme_units_handle_pow_test
