logical function rhyme_nombre_div_test () result (failed)
  use rhyme_nombre
  use rhyme_assertion

  implicit none

  type ( assertion_t ) :: n_tester

  type ( nombre_t ) :: n1, n2, ndivn, ndivr8, r8divn, ndivr, rdivn, ndivi, idivn
  type ( nombre_unit_t ), pointer :: u, r8divn_u, rdivn_u, idivn_u

  n_tester = .describe. "rhyme_nombre_div"

  call rhyme_nombre_init

  n1 = nombre_t( 1.23d0, kg * meter )
  n2 = nombre_t( 2.34d0, sec**2 )

  ! Nombre / Nombre
  ndivn = n1 / n2
  u => kg * meter / sec**2

  call n_tester%expect( ndivn%v .toBe. 1.23d0 / 2.34d0 )
  call n_tester%expect( ndivn%u .unitEqualsTo. u .toBe. .true. )

  ! Real8 / Nombre
  r8divn = 2.34d0 / n1
  r8divn_u => n1%u**(-1)

  call n_tester%expect( r8divn%v .toBe. 2.34d0 / 1.23d0 )
  call n_tester%expect( r8divn%u .unitEqualsTo. r8divn_u .toBe. .true. )

  ! Nombre / Real8
  ndivr8 = n1 / 3.45d0

  call n_tester%expect( ndivr8%v .toBe. 1.23d0 / 3.45d0 )
  call n_tester%expect( ndivr8%u .unitEqualsTo. n1%u .toBe. .true. )

  ! Nombre / real
  ndivr = n1 / 3.45e0

  call n_tester%expect( ndivr%v .toBe. 1.23d0 / 3.45e0 )
  call n_tester%expect( ndivr%u .unitEqualsTo. n1%u .toBe. .true. )

  ! Real / Nombre
  rdivn = 3.45e0 / n1
  rdivn_u => n1%u**(-1)

  call n_tester%expect( rdivn%v .toBe. 3.45e0 / 1.23d0 )
  call n_tester%expect( rdivn%u .unitEqualsTo. rdivn_u .toBe. .true. )

  ! Nombre / integer
  ndivi = n1 / 3

  call n_tester%expect( ndivi%v .toBe. 1.23d0 / 3 )
  call n_tester%expect( ndivi%u .unitEqualsTo. n1%u .toBe. .true. )

  ! Integer / Nombre
  idivn = 3 / n1
  idivn_u => n1%u**(-1)

  call n_tester%expect( idivn%v .toBe. 3 / 1.23d0 )
  call n_tester%expect( idivn%u .unitEqualsTo. idivn_u .toBe. .true. )

  failed = n_tester%failed()
end function rhyme_nombre_div_test
