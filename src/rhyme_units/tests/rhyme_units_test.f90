logical function rhyme_units_test () result ( failed )
  use rhyme_units
  use rhyme_assertion

  implicit none

  type ( assertion_t ) :: u_tester

  type ( rhyme_units_t ) :: units

  u_tester = .describe. "rhyme_units"

  call u_tester%expect( associated( units%rho ) .toBe. .false. )
  call u_tester%expect( associated( units%length ) .toBe. .false. )
  call u_tester%expect( associated( units%time ) .toBe. .false. )
  call u_tester%expect( associated( units%pressure ) .toBe. .false. )
  call u_tester%expect( associated( units%temperature ) .toBe. .false. )

  failed = u_tester%failed()
end function rhyme_units_test
