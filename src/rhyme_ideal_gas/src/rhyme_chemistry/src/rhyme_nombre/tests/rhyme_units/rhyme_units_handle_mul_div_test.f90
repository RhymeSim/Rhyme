logical function rhyme_units_handle_mul_div_test () result (failed)
  use rhyme_units

  implicit none

  type(unit_t), pointer :: kgK, m_s2, kgm2, Ks3

  kgK => kg * Kel
  m_s2 => (m / s)**2
  kgm2 => (kg * m)**2
  Ks3 => (Kel * s)**3

  call handle_mul_div(kgK, m_s2, "*")
  kgK => unit_head(kgK)

  failed = &
  kgK%symb .ne. "g" &
  .or. kgK%next%symb .ne. "K" &
  .or. kgK%next%next%symb .ne. "m" &
  .or. kgK%next%next%next%symb .ne. "s" &
  .or. associated(kgK%next%next%next%next)

  if ( failed ) return

  call handle_mul_div(kgm2, Ks3, "/")
  kgm2 => unit_head(kgm2)

  failed = &
  kgm2%symb .ne. "g" &
  .or. kgm2%next%symb .ne. "m" &
  .or. kgm2%next%next%symb .ne. "K" &
  .or. kgm2%next%next%next%symb .ne. "s" &
  .or. associated(kgm2%next%next%next%next) &
  .or. abs ( kgm2%next%next%pow - (-3) ) > epsilon(0.d0) &
  .or. abs ( kgm2%next%next%next%pow - (-3) ) > epsilon(0.d0)

end function rhyme_units_handle_mul_div_test
