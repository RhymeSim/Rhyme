logical function rhyme_ideal_gas_init_test () result (failed)
  use rhyme_ideal_gas_factory

  implicit none

  type ( ideal_gas_t ) :: ig_mon, ig_di, ig_poly
  type ( chemistry_t ) :: chemi
  type ( thermo_base_t ) :: thermo
  type ( unit_t ), pointer :: R_unit

  call chemi%init
  call thermo%init

  R_unit => kg * (m / s)**2 / mol / Kel

  failed = &
  igid%monatomic .ne. 1 &
  .or. igid%diatomic .ne. 2 &
  .or. igid%polyatomic .ne. 3
  if ( failed ) return

  call ig_mon%init_with( chemi, thermo, igid%monatomic )

  failed = .not. ig_mon%initialized
  if ( failed ) return

  failed = &
  abs ( ig_mon%R%v - 8.314d0 ) > epsilon(0.d0) &
  .or. .not. (ig_mon%R%u .unitEqualsTo. R_unit) &
  .or. abs ( ig_mon%Cv%v - 3.d0 / 2.d0 * 8.314d0 ) > epsilon(0.d0) &
  .or. .not. (ig_mon%Cv%u .unitEqualsTo. R_unit) &
  .or. abs ( ig_mon%cp%v - 5.d0 / 2.d0 * 8.314d0 ) > epsilon(0.d0) &
  .or. .not. (ig_mon%cp%u .unitequalsto. r_unit) &
  .or. abs ( ig_mon%gamma - ig_mon%Cp%v / ig_mon%Cv%v ) > epsilon(0.d0)
  if ( failed ) return


  call ig_di%init_with( chemi, thermo, igid%diatomic )

  failed = &
  abs ( ig_di%Cv%v - 5.d0 / 2.d0 * 8.314d0 ) > epsilon(0.d0) &
  .or. .not. (ig_di%Cv%u .unitEqualsTo. R_unit) &
  .or. abs ( ig_di%Cp%v - 7.d0 / 2.d0 * 8.314d0 ) > epsilon(0.d0) &
  .or. .not. (ig_di%Cp%u .unitEqualsTo. R_unit) &
  .or. abs ( ig_di%gamma - ig_di%Cp%v / ig_di%Cv%v ) > epsilon(0.d0)
  if ( failed ) return


  call ig_poly%init_with( chemi, thermo, igid%polyatomic )

  failed = &
  abs ( ig_poly%Cv%v - 3.d0 * 8.314d0 ) > epsilon(0.d0) &
  .or. .not. (ig_poly%Cv%u .unitEqualsTo. R_unit) &
  .or. abs ( ig_poly%Cp%v - 4.d0 * 8.314d0 ) > epsilon(0.d0) &
  .or. .not. (ig_poly%Cp%u .unitEqualsTo. R_unit) &
  .or. abs ( ig_poly%gamma - ig_poly%Cp%v / ig_poly%Cv%v ) > epsilon(0.d0)
  if ( failed ) return

  failed = &
  abs( ig_poly%gm1 - ( ig_poly%gamma - 1.d0 ) ) > epsilon(0.d0) &
  .or. abs( ig_poly%gp1 - ( ig_poly%gamma + 1.d0 ) ) > epsilon(0.d0) &
  .or. abs( ig_poly%gm1_gp1 - ( ig_poly%gamma - 1.d0 ) / ( ig_poly%gamma + 1.d0 ) ) > epsilon(0.d0) &
  .or. abs( ig_poly%gm1_2g - ( ig_poly%gamma - 1.d0 ) / ( 2 * ig_poly%gamma ) ) > epsilon(0.d0) &
  .or. abs( ig_poly%gp1_2g - ( ig_poly%gamma + 1.d0 ) / ( 2 * ig_poly%gamma ) ) > epsilon(0.d0) &
  .or. abs( ig_poly%g_inv - 1.d0 / ig_poly%gamma ) > epsilon(0.d0)
end function rhyme_ideal_gas_init_test
