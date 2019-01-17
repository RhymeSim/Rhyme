logical function rhyme_nombre_mul_test () result (failed)
  use rhyme_nombre

  implicit none

  type ( nombre_t ) :: H, H2int, H2int_rev, H2real, H2real_rev, H2real8, H2real8_rev
  type ( unit_t ), pointer :: u_H

  u_H => kilo * m / s / (Mega * pc)

  H = 66.7d0 .u. u_H

  H2int = 2 * H
  H2int_rev = H * 2
  H2real = 2.0 * H
  H2real_rev = H * 2.0
  H2real8 = 2.d0 * H
  H2real8_rev = H * 2.d0

  failed = &
    abs ( H2int%v - H2int_rev%v ) > epsilon(0.d0) &
    .or. abs ( H2int%v - (2.d0 * 66.7d0) ) > epsilon(0.d0) &
    .or. abs ( H2real%v - H2real_rev%v ) > epsilon(0.d0) &
    .or. abs ( H2real%v - (2.d0 * 66.7d0) ) > epsilon(0.d0) &
    .or. abs ( H2real8%v - H2real8_rev%v ) > epsilon(0.e0) &
    .or. abs ( H2real8%v - (2.d0 * 66.7d0) ) > epsilon(0.d0)

  if (failed) return

  H = H .to. s**(-1.d0)

  failed = .not. ( H2int%u .unitEqualsTo. u_H )

end function rhyme_nombre_mul_test
