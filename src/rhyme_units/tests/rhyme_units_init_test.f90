logical function rhyme_units_init_test () result ( failed )
  use rhyme_units
  use rhyme_assertion

  implicit none

  type ( assertion_t ) :: u_tester

  type ( rhyme_units_t ) :: units

  u_tester = .describe. "rhyme_units_init"

  units%rho_str = "kg * m^-3"
  units%length_str = "m"
  units%time_str = "s"

  call units%init

  call u_tester%expect( associated( units%rho ) .toBe. .true. )
  call u_tester%expect( associated( units%length ) .toBe. .true. )
  call u_tester%expect( associated( units%time ) .toBe. .true. )
  call u_tester%expect( associated( units%pressure ) .toBe. .true. )
  call u_tester%expect( associated( units%temperature ) .toBe. .true. )

  call u_tester%expect( units%rho%p() .toBe. units%rho_str )

  failed = u_tester%failed()
end function rhyme_units_init_test
