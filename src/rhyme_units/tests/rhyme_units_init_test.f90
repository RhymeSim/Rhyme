logical function rhyme_units_init_test () result ( failed )
  use rhyme_units
  use rhyme_assertion

  implicit none

  type ( assertion_t ) :: u_tester

  type ( nombre_unit_t ), pointer :: u_rho, u_length, u_time, u_pressure, u_temperature
  type ( rhyme_units_t ) :: units

  u_tester = .describe. "rhyme_units_init"

  units%rho_str = "kg * m^-3"
  units%length_str = "m"
  units%time_str = "s"

  u_rho => ( kilo * gram ) * meter**(-3)
  u_length => meter
  u_time => sec
  u_pressure => ( kilo * gram ) * meter**(-3) * meter**2 / sec**2
  u_temperature => kel

  call units%init

  call u_tester%expect( associated( units%rho ) .toBe. .true. )
  call u_tester%expect( associated( units%length ) .toBe. .true. )
  call u_tester%expect( associated( units%time ) .toBe. .true. )
  call u_tester%expect( associated( units%pressure ) .toBe. .true. )
  call u_tester%expect( associated( units%temperature ) .toBe. .true. )

  call u_tester%expect( units%rho .unitEqualsTo. u_rho .toBe. .true. )
  call u_tester%expect( units%length .unitEqualsTo. u_length .toBe. .true. )
  call u_tester%expect( units%time .unitEqualsTo. u_time .toBe. .true. )
  call u_tester%expect( units%pressure .unitEqualsTo. u_pressure .toBe. .true. )
  call u_tester%expect( units%temperature .unitEqualsTo. u_temperature .toBe. .true. )

  failed = u_tester%failed()
end function rhyme_units_init_test
