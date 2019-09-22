logical function rhyme_nombre_unit_clone_test () result ( failed )
  use rhyme_nombre_unit_factory
  use rhyme_assertion

  implicit none

  type ( assertion_t ) :: tester

  type ( nombre_unit_t ), pointer :: chain, clone

  tester = .describe. "nombre_unit_clone"

  call rhyme_nombre_derived_unit_init

  chain => nom_duc_factory%generate_chain( [ solar_mass, parsec, year, joule, watt ] )
  clone => .clonechain. chain

  call tester%expect( associated( clone%prev ) .toBe. .false. )
  call tester%expect( clone == solar_mass .toBe. .true. )
  call tester%expect( clone%next == parsec .toBe. .true. )
  call tester%expect( clone%next%prev == solar_mass .toBe. .true. )
  call tester%expect( clone%next%next == year .toBe. .true. )
  call tester%expect( clone%next%next%prev == parsec .toBe. .true. )
  call tester%expect( clone%next%next%next == joule .toBe. .true. )
  call tester%expect( clone%next%next%next%prev == year .toBe. .true. )
  call tester%expect( clone%next%next%next%next == watt .toBe. .true. )
  call tester%expect( clone%next%next%next%next%prev == joule .toBe. .true. )
  call tester%expect( associated( clone%next%next%next%next%next ) .toBe. .false. )

  failed = tester%failed()
end function rhyme_nombre_unit_clone_test
