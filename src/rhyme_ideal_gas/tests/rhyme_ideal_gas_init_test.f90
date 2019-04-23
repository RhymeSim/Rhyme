logical function rhyme_ideal_gas_init_test () result (failed)
  use rhyme_ideal_gas_factory
  use rhyme_assertion

  implicit none

  type ( assertion_t ) :: ig_tester

  type ( ideal_gas_t ) :: ig_mon, ig_di, ig_poly
  type ( unit_t ), pointer :: R_unit

  ig_tester = .describe. "ideal_gas init"

  call rhyme_ideal_gas_factory_init

  R_unit => kg * (m / s)**2 / mol / Kel

  call ig_tester%expect( igid%monatomic .toBe. 1 )
  call ig_tester%expect( igid%diatomic .toBe. 2 )
  call ig_tester%expect( igid%polyatomic .toBe. 3 )

  call ig_mon%init_with( chemi, thermo, igid%monatomic, log )

  call ig_tester%expect( ig_mon%initialized .toBe. .true. )

  call ig_tester%expect( ig_mon%R%v .toBe. 8.314d0 )
  call ig_tester%expect( (ig_mon%R%u .unitEqualsTo. R_unit) .toBe. .true. )
  call ig_tester%expect( ig_mon%Cv%v .toBe. 3.d0 / 2.d0 * 8.314d0 )
  call ig_tester%expect( (ig_mon%Cv%u .unitEqualsTo. R_unit) .toBe. .true. )
  call ig_tester%expect( ig_mon%cp%v .toBe. 5.d0 / 2.d0 * 8.314d0 )
  call ig_tester%expect( (ig_mon%cp%u .unitequalsto. r_unit) .toBe. .true. )
  call ig_tester%expect( ig_mon%gamma .toBe. ig_mon%Cp%v / ig_mon%Cv%v  )

  call ig_di%init_with( chemi, thermo, igid%diatomic, log )

  call ig_tester%expect( ig_di%Cv%v .toBe. 5.d0 / 2.d0 * 8.314d0 )
  call ig_tester%expect( (ig_di%Cv%u .unitEqualsTo. R_unit) .toBe. .true. )
  call ig_tester%expect( ig_di%Cp%v .toBe. 7.d0 / 2.d0 * 8.314d0 )
  call ig_tester%expect( (ig_di%Cp%u .unitEqualsTo. R_unit) .toBe. .true. )
  call ig_tester%expect( ig_di%gamma .toBe. ig_di%Cp%v / ig_di%Cv%v )

  call ig_poly%init_with( chemi, thermo, igid%polyatomic, log )

  call ig_tester%expect( ig_poly%Cv%v .toBe. 3.d0 * 8.314d0 )
  call ig_tester%expect( (ig_poly%Cv%u .unitEqualsTo. R_unit) .toBe. .true. )
  call ig_tester%expect( ig_poly%Cp%v .toBe. 4.d0 * 8.314d0 )
  call ig_tester%expect( (ig_poly%Cp%u .unitEqualsTo. R_unit) .toBe. .true. )
  call ig_tester%expect( ig_poly%gamma .toBe. ig_poly%Cp%v / ig_poly%Cv%v )

  call ig_tester%expect( ig_poly%gm1 .toBe. ( ig_poly%gamma - 1.d0 ) )
  call ig_tester%expect( ig_poly%gp1 .toBe. ( ig_poly%gamma + 1.d0 ) )
  call ig_tester%expect( ig_poly%gm1_gp1 .toBe. ( ig_poly%gm1 / ig_poly%gp1 ) )
  call ig_tester%expect( ig_poly%gm1_2g .toBe. ( ig_poly%gm1 / ( 2 * ig_poly%gamma ) ) )
  call ig_tester%expect( ig_poly%gp1_2g .toBe. ( ig_poly%gp1 / ( 2 * ig_poly%gamma ) ) )
  call ig_tester%expect( ig_poly%g_inv .toBe. 1.d0 / ig_poly%gamma )

  failed = ig_tester%failed()
end function rhyme_ideal_gas_init_test
