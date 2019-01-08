logical function rhyme_units_handle_pow_test () result (failed)
  use rhyme_units

  implicit none

  type(unit_t), pointer :: u

  u => kg * m / s

  call handle_pow(u, 2.0)
  u => unit_head(u)

  failed = &
  abs(u%pow - 2.d0) > epsilon(0.d0) &
  .or. abs(u%next%pow - 2.d0) > epsilon(0.d0) &
  .or. abs(u%next%next%pow - (-2.d0)) > epsilon(0.d0) &
  .or. associated(u%next%next%next)
end function rhyme_units_handle_pow_test
