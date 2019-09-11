logical function rhyme_nombre_derived_unit_chain_mul_test () result ( failed )
  use rhyme_nombre_derived_unit_chain_factory
  use rhyme_nombre_derived_unit_factory
  use rhyme_assertion

  implicit none

  type ( assertion_t ) :: tester

  type ( nombre_derived_unit_t ), pointer :: duc, duc2, ducduc
  type ( nombre_derived_unit_t ), pointer :: ducu, ducuu, uduc, uuduc

  tester = .describe. "nombre_derived_unit_chain_mul"

  call rhyme_nombre_derived_unit_chain_init

  duc => atomic_mass_unit * astronomical_unit * stradian
  call tester%expect( associated( duc%prev ) .toBe. .false. .hint. 'duc' )
  call tester%expect( duc == atomic_mass_unit .toBe. .true. )
  call tester%expect( duc%next == astronomical_unit .toBe. .true. )
  call tester%expect( duc%next%prev == atomic_mass_unit .toBe. .true. )
  call tester%expect( duc%next%next == stradian .toBe. .true. )
  call tester%expect( duc%next%next%prev == astronomical_unit .toBe. .true. )
  call tester%expect( associated( duc%next%next%next ) .toBe. .false. )

  duc => atomic_mass_unit * astronomical_unit * stradian
  ducu => duc * second
  call tester%expect( ducu == atomic_mass_unit .toBe. .true. .hint. 'ducu w/ symb' )
  call tester%expect( ducu%next == astronomical_unit .toBe. .true. )
  call tester%expect( ducu%next%next == stradian .toBe. .true. )
  call tester%expect( ducu%next%next%next%head == second .toBe. .true. )

  duc => atomic_mass_unit * nom_du_factory%generate( [ kilogram, second**(-1) ], symb='', pow=1.23d0 )
  ducu => duc * meter
  call tester%expect( ducu == atomic_mass_unit .toBe. .true. .hint. 'ducu w/o symb' )
  call tester%expect( ducu%next%head == kilogram .toBe. .true. )
  call tester%expect( ducu%next%head%next == second**(-1) .toBe. .true. )
  call tester%expect( ducu%next%head%next%next == meter**(1d0 / 1.23d0) .toBe. .true. )
  call tester%expect( associated( ducu%next%head%next%next%next ) .toBe. .false. )

  duc => atomic_mass_unit * astronomical_unit * stradian
  ducu => duc * second
  ducuu => ducu * kilogram**2
  call tester%expect( ducuu == atomic_mass_unit .toBe. .true. .hint. 'ducuu w/ symb' )
  call tester%expect( ducuu%next == astronomical_unit .toBe. .true. )
  call tester%expect( ducuu%next%next == stradian .toBe. .true. )
  call tester%expect( ducuu%next%next%next%head == second .toBe. .true. )
  call tester%expect( ducuu%next%next%next%head%next == kilogram**2 .toBe. .true. )

  duc => atomic_mass_unit * astronomical_unit * stradian
  uduc => kelvin * duc
  call tester%expect( uduc%head == kelvin .toBe. .true. .hint. 'uduc w/ symb' )
  call tester%expect( uduc%next == atomic_mass_unit .toBe. .true. )
  call tester%expect( uduc%next%next == astronomical_unit .toBe. .true. )
  call tester%expect( uduc%next%next%next == stradian .toBe. .true. )
  call tester%expect( associated( uduc%next%next%next%next ) .toBe. .false. )

  duc => nom_du_factory%generate( [ kilogram, second**(-1) ], symb='', pow=1.23d0 ) * atomic_mass_unit
  uduc => kelvin * duc
  call tester%expect( uduc%head == kelvin**(1d0 / 1.23d0) .toBe. .true. .hint. 'uduc w/o symb' )
  call tester%expect( uduc%head%next == kilogram .toBe. .true. )
  call tester%expect( uduc%head%next%next == second**(-1) .toBe. .true. )
  call tester%expect( associated( uduc%head%next%next%next ) .toBe. .false. )
  call tester%expect( uduc%next == atomic_mass_unit .toBe. .true. )
  call tester%expect( associated( uduc%next%next ) .toBe. .false. )

  duc => atomic_mass_unit * astronomical_unit * stradian
  uduc => kelvin * duc
  uuduc => meter**2 * uduc
  call tester%expect( uuduc%head == meter**2 .toBe. .true. .hint. 'uuduc w/ symb' )
  call tester%expect( uuduc%head%next == kelvin .toBe. .true. )
  call tester%expect( uuduc%next == atomic_mass_unit .toBe. .true. )
  call tester%expect( uuduc%next%next == astronomical_unit .toBe. .true. )
  call tester%expect( uuduc%next%next%next == stradian .toBe. .true. )
  call tester%expect( associated( uduc%next%next%next%next ) .toBe. .false. )

  duc => atomic_mass_unit * astronomical_unit * stradian
  duc2 => newton * parsec
  ducduc => duc * duc2
  call tester%expect( ducduc == atomic_mass_unit .toBe. .true. .hint. 'ducduc' )
  call tester%expect( ducduc%next == astronomical_unit .toBe. .true. )
  call tester%expect( ducduc%next%next == stradian .toBe. .true. )
  call tester%expect( ducduc%next%next%next == newton .toBe. .true. )
  call tester%expect( ducduc%next%next%next%next == parsec .toBe. .true. )
  call tester%expect( associated( ducduc%next%next%next%next%next ) .toBe. .false. )

  duc => nom_du_factory%generate( [ kilogram, meter ], symb='', pow=1.23d0 )
  duc2 => nom_du_factory%generate( [ second**(-2) ], symb='', pow=2.34d0 )
  ducduc => duc * duc2
  call tester%expect( ducduc%head == kilogram .toBe. .true. .hint. 'ducduc w/o symb' )
  call tester%expect( ducduc%head%next == meter .toBe. .true. )
  call tester%expect( ducduc%head%next%next == second**(-2 * 2.34d0 / 1.23d0) .toBe. .true. )
  call tester%expect( associated( ducduc%head%next%next%next ) .toBe. .false. )

  failed = tester%failed()
end function rhyme_nombre_derived_unit_chain_mul_test
