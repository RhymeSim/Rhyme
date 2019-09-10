logical function rhyme_nombre_derived_unit_chain_div_test () result ( failed )
  use rhyme_nombre_derived_unit_chain_factory
  use rhyme_nombre_derived_unit_factory
  use rhyme_assertion

  implicit none

  type ( assertion_t ) :: tester

  type ( nombre_derived_unit_t ), pointer :: du, duu
  type ( nombre_derived_unit_t ), pointer :: duc, ducu, uduc, uuduc, duc2, ducduc2

  tester = .describe. "nombre_derived_unit_chain_div"

  call rhyme_nombre_derived_unit_chain_init

  du => nom_du_factory%generate( [ kilogram, meter, second**(-2) ], symb='N', pow=1.23d0 )
  duu => du / meter**2
  call tester%expect( duu == du .toBe. .true. )
  call tester%expect( duu%next%head == meter**(-2) .toBe. .true. )
  call tester%expect( associated( duu%next%next ) .toBe. .false. )


  du => nom_du_factory%generate( [ kilogram, meter, second**(-2) ], symb='', pow=1.23d0 )
  duu => du / meter**2
  call tester%expect( duu%head == kilogram .toBe. .true. )
  call tester%expect( duu%head%next == meter .toBe. .true. )
  call tester%expect( duu%head%next%next == second**(-2) .toBe. .true. )
  call tester%expect( duu%head%next%next%next == meter**(-2 * 1.23d0) .toBe. .true. )


  duc => nom_duc_factory%generate_chain( [ joule, parsec, radian**(-1) ] )
  ducu => duc / ( kilo * meter )
  call tester%expect( ducu == joule .toBe. .true. )
  call tester%expect( ducu%next == parsec .toBe. .true. )
  call tester%expect( ducu%next%next == radian**(-1) .toBe. .true. )
  call tester%expect( ducu%next%next%next%pow .toBe. 1d0 )
  call tester%expect( ducu%next%next%next%head == ( kilo * meter )**(-1) .toBe. .true. )
  call tester%expect( associated( ducu%next%next%next%head%next ) .toBe. .false. )
  call tester%expect( associated( ducu%next%next%next%next ) .toBe. .false. )


  duc => nom_duc_factory%generate_chain( [ joule, &
    nom_du_factory%generate( [ kilogram, meter**(-1) ], symb='', pow=1.23d0 ) ] )
  ducu => duc / kelvin
  call tester%expect( ducu == joule .toBe. .true. )
  call tester%expect( ducu%next%pow .toBe. 1.23d0 )
  call tester%expect( ducu%next%head == kilogram .toBe. .true. )
  call tester%expect( ducu%next%head%next == meter**(-1) .toBe. .true. )
  call tester%expect( ducu%next%head%next%next == kelvin**(-1 * 1.23d0) .toBe. .true. )
  call tester%expect( associated( ducu%next%head%next%next%next ) .toBe. .false. )
  call tester%expect( associated( ducu%next%next ) .toBe. .false. )


  duc => nom_duc_factory%generate_chain( [ year, watt, hertz ] )
  uduc => kilogram / duc
  call tester%expect( uduc%prefix == null_prefix .toBe. .true. )
  call tester%expect( uduc%symb .toBe. '' )
  call tester%expect( uduc%conv .toBe. 1d0 )
  call tester%expect( uduc%dim == dimid%mass .toBe. .true. )
  call tester%expect( uduc%pow .toBe. 1d0 )
  call tester%expect( uduc%head == kilogram .toBe. .true. )
  call tester%expect( associated( uduc%head%next ) .toBe. .false. )
  call tester%expect( uduc%next == year**(-1) .toBe. .true. )
  call tester%expect( uduc%next%next == watt**(-1) .toBe. .true. )
  call tester%expect( uduc%next%next%next == hertz**(-1) .toBe. .true. )
  call tester%expect( associated( uduc%next%next%next%next ) .toBe. .false. )


  duc => nom_duc_factory%generate_chain( [ &
    nom_du_factory%generate( [ kilogram, meter**(-1) ], symb='', pow=1.23d0 ), &
    year, watt ] )
  uuduc => meter / duc
  call tester%expect( uuduc%prefix == null_prefix .toBe. .true. )
  call tester%expect( uuduc%symb .toBe. '' )
  call tester%expect( uuduc%conv .toBe. 1d0 )
  call tester%expect( uuduc%pow .toBe. 1.23d0 )
  call tester%expect( uuduc%head == meter**(1 / 1.23d0) .toBe. .true. )
  call tester%expect( uuduc%head%next == kilogram**(-1) .toBe. .true. )
  call tester%expect( uuduc%head%next%next == meter .toBe. .true. )
  call tester%expect( associated( uuduc%head%next%next%next ) .toBe. .false. )

  duc => nom_duc_factory%generate_chain( [ solar_mass, parsec, year**(-2) ] )
  duc2 => nom_duc_factory%generate_chain( [ hertz, radian**(-1) ] )
  ducduc2 => duc / duc2
  call tester%expect( ducduc2 == solar_mass .toBe. .true. )
  call tester%expect( ducduc2%next == parsec .toBe. .true. )
  call tester%expect( ducduc2%next%next == year**(-2) .toBe. .true. )
  call tester%expect( ducduc2%next%next%next == hertz**(-1) .toBe. .true. )
  call tester%expect( ducduc2%next%next%next%next == radian .toBe. .true. )
  call tester%expect( associated( ducduc2%next%next%next%next%next ) .toBe. .false. )

  failed = tester%failed()
end function rhyme_nombre_derived_unit_chain_div_test
