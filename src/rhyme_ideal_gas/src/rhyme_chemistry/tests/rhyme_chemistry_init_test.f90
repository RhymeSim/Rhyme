logical function rhyme_chemistry_init_test () result (failed)
  use rhyme_chemistry
  use rhyme_units_factory
  use rhyme_assertion

  implicit none

  type ( assertion_t ) :: ch_tester

  type ( chemistry_t ) :: chemi
  type ( rhyme_units_t ) :: units
  type ( log_t ) :: log

  type ( nombre_unit_t ), pointer :: M__MOL, M

  ch_tester = .describe. "chemistry init"

  units = units_factory%generate()
  call rhyme_chemistry_init( chemi, units, log )

  M__MOL => units%rho * units%length**3 / mol
  M => units%rho * units%length**3

  call ch_tester%expect( chemi%molar%e%v .toBe. 5.48580d-7 )
  call ch_tester%expect( chemi%molar%e%u .unitEqualsTo. M__MOL .toBe. .true. )
  call ch_tester%expect( chemi%molar%H%v .toBe. 1.00794d-3 )
  call ch_tester%expect( chemi%molar%H%u .unitEqualsTo. M__MOL .toBe. .true. )
  call ch_tester%expect( chemi%molar%He%v .toBe. 4.002602d-3 )
  call ch_tester%expect( chemi%molar%He%u .unitEqualsTo. M__MOL .toBe. .true. )

  call ch_tester%expect( chemi%atomic%e%v .toBe. 9.1093835d-31 )
  call ch_tester%expect( chemi%atomic%e%u .unitEqualsTo. M .toBe. .true. )
  call ch_tester%expect( chemi%atomic%H%v .toBe. 1.6737236d-27 )
  call ch_tester%expect( chemi%atomic%H%u .unitEqualsTo. M .toBe. .true. )
  call ch_tester%expect( chemi%atomic%He%v .toBe. 6.6464764d-27 )
  call ch_tester%expect( chemi%atomic%He%u .unitEqualsTo. M .toBe. .true. )

  call ch_tester%expect( chemi%amu%one%v .toBe. 1.66054d-27 )
  call ch_tester%expect( chemi%amu%one%u .unitEqualsTo. M .toBe. .true. )

  failed = ch_tester%failed()
end function rhyme_chemistry_init_test
