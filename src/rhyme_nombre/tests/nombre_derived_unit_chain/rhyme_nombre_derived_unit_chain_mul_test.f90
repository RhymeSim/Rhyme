logical function rhyme_nombre_derived_unit_chain_mul_test () result ( failed )
  use rhyme_nombre_derived_unit_chain_factory
  use rhyme_assertion

  implicit none

  type ( assertion_t ) :: tester

  type ( nombre_derived_unit_t ), pointer :: du, duu, duuu
  type ( nombre_derived_unit_t ), pointer :: udu, uudu, uuudu

  tester = .describe. "nombre_derived_unit_chain_mul"

  call nom_duc_factory%init

  du => nom_duc_factory%chain_members(1)%ptr &
    * nom_duc_factory%chain_members(2)%ptr &
    * nom_duc_factory%chain_members(3)%ptr

  call tester%expect( associated( du%prev ) .toBe. .false. )
  call tester%expect( du == nom_duc_factory%chain_members(1)%ptr .toBe. .true. )
  call tester%expect( du%next == nom_duc_factory%chain_members(2)%ptr .toBe. .true. )
  call tester%expect( du%next%prev == nom_duc_factory%chain_members(1)%ptr .toBe. .true. )
  call tester%expect( du%next%next == nom_duc_factory%chain_members(3)%ptr .toBe. .true. )
  call tester%expect( du%next%next%prev == nom_duc_factory%chain_members(2)%ptr .toBe. .true. )
  call tester%expect( associated( du%next%next%next ) .toBe. .false. )

  duu => du * second
  call tester%expect( duu == nom_duc_factory%chain_members(1)%ptr .toBe. .true. )
  call tester%expect( duu%next == nom_duc_factory%chain_members(2)%ptr .toBe. .true. )
  call tester%expect( duu%next%next == nom_duc_factory%chain_members(3)%ptr .toBe. .true. )
  call tester%expect( duu%next%next%next%head == second .toBe. .true. )

  duuu => duu * kilogram**2
  call tester%expect( duuu == nom_duc_factory%chain_members(1)%ptr .toBe. .true. )
  call tester%expect( duuu%next == nom_duc_factory%chain_members(2)%ptr .toBe. .true. )
  call tester%expect( duuu%next%next == nom_duc_factory%chain_members(3)%ptr .toBe. .true. )
  call tester%expect( duuu%next%next%next%head == second .toBe. .true. )
  call tester%expect( duuu%next%next%next%head%next == kilogram**2 .toBe. .true. )

  udu => kelvin * du

  call tester%expect( udu%head == kelvin .toBe. .true. )
  call tester%expect( udu%next == nom_duc_factory%chain_members(1)%ptr .toBe. .true. )
  call tester%expect( udu%next%next == nom_duc_factory%chain_members(2)%ptr .toBe. .true. )
  call tester%expect( udu%next%next%next == nom_duc_factory%chain_members(3)%ptr .toBe. .true. )
  ! call tester%expect( c2%head%next%next == kelvin**2 .toBe. .true. .hint. 'c2 next%next to be kel' )
  ! call tester%expect( c2%head%next%next%pow .toBe. 2d0 .hint. 'c2 next%next%pow to be 2d0' )
  ! call tester%expect( c2%head%next%next%prev == meter .toBe. .true. .hint. 'c2 next%next%prev to be meter' )
  !
  ! call tester%expect( c2%dim == rhyme_nombre_derived_unit_get_dim( c2 ) .toBe. .true. .hint. 'c2 unit' )

  failed = tester%failed()
end function rhyme_nombre_derived_unit_chain_mul_test
