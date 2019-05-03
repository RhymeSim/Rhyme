logical function rhyme_nombre_mul_test () result (failed)
  use rhyme_nombre
  use rhyme_assertion

  implicit none

  type ( assertion_t ) :: n_tester

  type ( nombre_t ) :: H, H2int, H2int_rev, H2real, H2real_rev, H2real8, H2real8_rev
  type ( nombre_unit_t ), pointer :: u_H

  n_tester = .describe. "nombre_mul"

  u_H => kilo * meter / sec / (Mega * pc)

  H = 66.7d0 .u. u_H

  H2int = 2 * H
  H2int_rev = H * 2
  H2real = 2.0 * H
  H2real_rev = H * 2.0
  H2real8 = 2.d0 * H
  H2real8_rev = H * 2.d0

  call n_tester%expect( H2int%v .toBe. H2int_rev%v )
  call n_tester%expect( H2int%v .toBe. (2.d0 * 66.7d0) )
  call n_tester%expect( H2real%v .toBe. H2real_rev%v )
  call n_tester%expect( H2real%v .toBe. (2.d0 * 66.7d0) )
  call n_tester%expect( H2real8%v .toBe. H2real8_rev%v )
  call n_tester%expect( H2real8%v .toBe. (2.d0 * 66.7d0) )

  ! TODO: test multiplying two nombre objects

  H = H .to. sec**(-1.d0)

  call n_tester%expect( H2int%u .unitEqualsTo. u_H .toBe. .true. )

  failed = n_tester%failed()
end function rhyme_nombre_mul_test
