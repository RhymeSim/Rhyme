logical function rhyme_nombre_div_test () result (failed)
  use rhyme_nombre

  implicit none

  type(nombre_t) :: n1, n2, ndivn, ndivr8, r8divn, ndivr, rdivn, ndivi, idivn
  type(unit_t), pointer :: u, r8divn_u, rdivn_u, idivn_u

  n1 = nombre_t(1.23d0, kg * m)
  n2 = nombre_t(2.34d0, s**2)

  ! Nombre / Nombre
  ndivn = n1 / n2
  u => kg * m / s**2

  failed = &
  abs ( ndivn%v - 1.23d0 / 2.34d0 ) > epsilon(0.d0) &
  .or. .not. ( ndivn%u .unitEqualsTo. u )

  if ( failed ) return

  ! Real8 / Nombre
  r8divn = 2.34d0 / n1
  r8divn_u => n1%u**(-1)

  failed = &
  abs ( r8divn%v - 2.34d0 / 1.23d0 ) > epsilon(0.d0) &
  .or. .not. ( r8divn%u .unitEqualsTo. r8divn_u )

  if ( failed ) return

  ! Nombre / Real8
  ndivr8 = n1 / 3.45d0

  failed = &
  abs ( ndivr8%v - 1.23d0 / 3.45d0 ) > epsilon(0.d0) &
  .or. .not. ( ndivr8%u .unitEqualsTo. n1%u )

  if ( failed ) return

  ! Nombre / real
  ndivr = n1 / 3.45e0

  failed = &
  abs ( ndivr%v - 1.23d0 / 3.45e0 ) > epsilon(0.d0) &
  .or. .not. ( ndivr%u .unitEqualsTo. n1%u )

  if ( failed ) return

  ! Real / Nombre
  rdivn = 3.45e0 / n1
  rdivn_u => n1%u**(-1)

  failed = &
  abs ( rdivn%v - 3.45e0 / 1.23d0 ) > epsilon(0.d0)  &
  .or. .not. ( rdivn%u .unitEqualsTo. rdivn_u )

  if ( failed ) return

  ! Nombre / integer
  ndivi = n1 / 3

  failed = &
  abs ( ndivi%v - 1.23d0 / 3 ) > epsilon(0.d0) &
  .or. .not. ( ndivi%u .unitEqualsTo. n1%u )

  if ( failed ) return

  ! Integer / Nombre
  idivn = 3 / n1
  idivn_u => n1%u**(-1)

  failed = &
  abs ( idivn%v - 3 / 1.23d0 ) > epsilon(0.d0)  &
  .or. .not. ( idivn%u .unitEqualsTo. idivn_u )
end function rhyme_nombre_div_test
