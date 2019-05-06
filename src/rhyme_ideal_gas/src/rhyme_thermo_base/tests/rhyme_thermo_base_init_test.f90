logical function rhyme_thermo_base_init_test () result ( failed )
  use rhyme_thermo_base
  use rhyme_units_factory
  use rhyme_assertion

  implicit none

  type ( assertion_t ) :: ch_tester

  type ( thermo_base_t ) :: thermo, thermo_cgs
  type ( rhyme_units_t ) :: units
  type ( log_t ) :: log

  type ( nombre_unit_t ), pointer :: u_Kb

  units = units_factory%generate()

  call ch_tester%expect( thermo%initialized .toBe. .false. )

  call rhyme_thermo_base_init( thermo, units, log )
  u_Kb => units%rho * units%length**5 / ( units%time**2 * kel )

  call ch_tester%expect( thermo%initialized .toBe. .true. )
  call ch_tester%expect( thermo%kB%v .toBe. 1.38064852d-23 )
  call ch_tester%expect( (thermo%kB%u .unitEqualsTo. u_Kb) .toBe. .true. )


  units_factory%rho_str = 'g / cm^3'
  units_factory%length_str = 'cm'
  units_factory%time_str = 's'
  units = units_factory%generate()

  call rhyme_thermo_base_init( thermo_cgs, units, log )
  u_Kb => units%rho * units%length**5 / ( units%time**2 * kel )

  call ch_tester%expect( thermo_cgs%initialized .toBe. .true. )
  call ch_tester%expect( thermo_cgs%kB%v .toBe. 1.38064852d-16 .within. 15 )
  call ch_tester%expect( thermo_cgs%kB%u .unitEqualsTo. u_Kb .toBe. .true. )

  failed = ch_tester%failed()
end function rhyme_thermo_base_init_test
