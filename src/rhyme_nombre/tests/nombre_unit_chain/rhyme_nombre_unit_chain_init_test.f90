logical function rhyme_nombre_unit_chain_init_test () result ( failed )
  use rhyme_nombre_unit_chain
  use rhyme_assertion

  implicit none

  type ( assertion_t ) :: tester

  tester = .describe. "nombre_unit_chain_init"

  call rhyme_nombre_unit_chain_init

  call tester%expect( solar_mass%conv .toBe. 1.9885d30 .hint. 'Msun conv' )
  call tester%expect( solar_mass%symb .toBe. 'Msun' .hint. 'Msun symb' )
  call tester%expect( solar_mass%head == kilogram .toBe. .true. .hint. 'Msun head' )
  call tester%expect( associated( solar_mass%head%next ) .toBe. .false. .hint. 'Msun head%next' )

  call tester%expect( hydrogen_mass%conv .toBe. 1.6735575d-24 .hint. 'm_H conv' )
  call tester%expect( hydrogen_mass%symb .toBe. 'm_H' .hint. 'm_H symb' )
  call tester%expect( hydrogen_mass%head == kilogram .toBe. .true. .hint. 'm_H head' )
  call tester%expect( associated( hydrogen_mass%head%next ) .toBe. .false. .hint. 'm_H head%next' )

  call tester%expect( atomic_mass_unit%conv .toBe. 1.6605d-24 .hint. 'amu conv' )
  call tester%expect( atomic_mass_unit%symb .toBe. 'amu' .hint. 'amu symb' )
  call tester%expect( atomic_mass_unit%head == kilogram .toBe. .true. .hint. 'amu head' )
  call tester%expect( associated( atomic_mass_unit%head%next ) .toBe. .false. .hint. 'amu head%next' )

  call tester%expect( parsec%conv .toBe. 3.086d16 .hint. 'pc conv' )
  call tester%expect( parsec%symb .toBe. 'pc' .hint. 'pc symb' )
  call tester%expect( parsec%head == meter .toBe. .true. .hint. 'pc head' )
  call tester%expect( associated( parsec%head%next ) .toBe. .false. .hint. 'pc head%next' )

  call tester%expect( light_year%conv .toBe. 9.461d15 .hint. 'ly conv' )
  call tester%expect( light_year%symb .toBe. 'ly' .hint. 'ly symb' )
  call tester%expect( light_year%head == meter .toBe. .true. .hint. 'ly head' )
  call tester%expect( associated( light_year%head%next ) .toBe. .false. .hint. 'ly head%next' )

  call tester%expect( astronomical_unit%conv .toBe. 1.496d11 .hint. 'AU conv' )
  call tester%expect( astronomical_unit%symb .toBe. 'AU' .hint. 'AU symb' )
  call tester%expect( astronomical_unit%head == meter .toBe. .true. .hint. 'AU head' )
  call tester%expect( associated( astronomical_unit%head%next ) .toBe. .false. .hint. 'AU head%next' )

  call tester%expect( year%conv .toBe. 3.154d7 .hint. 'yr conv' )
  call tester%expect( year%symb .toBe. 'yr' .hint. 'yr symb' )
  call tester%expect( year%head == second .toBe. .true. .hint. 'yr head' )
  call tester%expect( associated( year%head%next ) .toBe. .false. .hint. 'yr head%next' )

  call tester%expect( joule%conv .toBe. 1d0 .hint. 'J conv' )
  call tester%expect( joule%symb .toBe. 'J' .hint. 'J symb' )
  call tester%expect( joule%head == kilogram .toBe. .true. .hint. 'J head' )
  call tester%expect( joule%head%next == meter**2 .toBe. .true. .hint. 'J head' )
  call tester%expect( joule%head%next%next == second**(-2) .toBe. .true. .hint. 'J head' )
  call tester%expect( associated( joule%head%next%next%next ) .toBe. .false. .hint. 'J head%...%next' )

  call tester%expect( electron_volt%conv .toBe. 1.602176634d-19 .hint. 'eV conv' )
  call tester%expect( electron_volt%symb .toBe. 'eV' .hint. 'eV symb' )
  call tester%expect( electron_volt%head == kilogram .toBe. .true. .hint. 'eV head' )
  call tester%expect( electron_volt%head%next == meter**2 .toBe. .true. .hint. 'eV head' )
  call tester%expect( electron_volt%head%next%next == second**(-2) .toBe. .true. .hint. 'eV head' )
  call tester%expect( associated( electron_volt%head%next%next%next ) .toBe. .false. .hint. 'eV head%...%next' )

  call tester%expect( watt%conv .toBe. 1d0 .hint. 'W conv' )
  call tester%expect( watt%symb .toBe. 'W' .hint. 'W symb' )
  call tester%expect( watt%head == kilogram .toBe. .true. .hint. 'W head' )
  call tester%expect( watt%head%next == meter**2 .toBe. .true. .hint. 'W head' )
  call tester%expect( watt%head%next%next == second**(-3) .toBe. .true. .hint. 'W head' )
  call tester%expect( associated( watt%head%next%next%next ) .toBe. .false. .hint. 'W head%...%next' )

  call tester%expect( pascal%conv .toBe. 1d0 .hint. 'Pa conv' )
  call tester%expect( pascal%symb .toBe. 'Pa' .hint. 'Pa symb' )
  call tester%expect( pascal%head == kilogram .toBe. .true. .hint. 'Pa head' )
  call tester%expect( pascal%head%next == meter**(-1) .toBe. .true. .hint. 'Pa head' )
  call tester%expect( pascal%head%next%next == second**(-2) .toBe. .true. .hint. 'Pa head' )
  call tester%expect( associated( pascal%head%next%next%next ) .toBe. .false. .hint. 'Pa head%...%next' )

  call tester%expect( hertz%conv .toBe. 1d0 .hint. 'Hz conv' )
  call tester%expect( hertz%symb .toBe. 'Hz' .hint. 'Hz symb' )
  call tester%expect( hertz%head == second**(-1) .toBe. .true. .hint. 'Hz head' )
  call tester%expect( associated( hertz%head%next ) .toBe. .false. .hint. 'Hz head%next' )

  call tester%expect( radian%conv .toBe. 1d0 .hint. 'rad conv' )
  call tester%expect( radian%symb .toBe. 'rad' .hint. 'rad symb' )
  call tester%expect( radian%head == meter .toBe. .true. .hint. 'rad head' )
  call tester%expect( radian%head%next == meter**(-1) .toBe. .true. .hint. 'rad head' )
  call tester%expect( associated( radian%head%next%next ) .toBe. .false. .hint. 'rad head%next%next' )

  call tester%expect( stradian%conv .toBe. 1d0 .hint. 'sr conv' )
  call tester%expect( stradian%symb .toBe. 'sr' .hint. 'sr symb' )
  call tester%expect( stradian%head == meter**2 .toBe. .true. .hint. 'sr head' )
  call tester%expect( stradian%head%next == meter**(-2) .toBe. .true. .hint. 'sr head' )
  call tester%expect( associated( stradian%head%next%next ) .toBe. .false. .hint. 'sr head%next%next' )

  call tester%expect( newton%conv .toBe. 1d0 .hint. 'N conv' )
  call tester%expect( newton%symb .toBe. 'N' .hint. 'N symb' )
  call tester%expect( newton%head == kilogram .toBe. .true. .hint. 'N head' )
  call tester%expect( newton%head%next == meter .toBe. .true. .hint. 'N head' )
  call tester%expect( newton%head%next%next == second**(-2) .toBe. .true. .hint. 'N head' )
  call tester%expect( associated( newton%head%next%next%next ) .toBe. .false. .hint. 'N head%...%next' )

  failed = tester%failed()
end function rhyme_nombre_unit_chain_init_test
