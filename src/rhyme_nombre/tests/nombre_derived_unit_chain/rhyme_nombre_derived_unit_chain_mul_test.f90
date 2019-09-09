logical function rhyme_nombre_derived_unit_chain_mul_test () result ( failed )
  use rhyme_nombre_derived_unit_chain_factory
  use rhyme_assertion

  implicit none

  type ( assertion_t ) :: tester

  type ( nombre_derived_unit_t ), pointer :: chain

  tester = .describe. "nombre_derived_unit_chain_mul"

  call nom_duc_factory%init

  chain => nom_duc_factory%chain_members(1)%ptr &
    * nom_duc_factory%chain_members(2)%ptr &
    * nom_duc_factory%chain_members(3)%ptr &
    * nom_duc_factory%chain_members(4)%ptr


  call tester%expect( associated( chain%prev ) .toBe. .false. )
  call tester%expect( chain == nom_duc_factory%chain_members(1)%ptr .toBe. .true. )
  call tester%expect( chain%next == nom_duc_factory%chain_members(2)%ptr .toBe. .true. )
  call tester%expect( chain%next%prev == nom_duc_factory%chain_members(1)%ptr .toBe. .true. )
  call tester%expect( chain%next%next == nom_duc_factory%chain_members(3)%ptr .toBe. .true. )
  call tester%expect( chain%next%next%prev == nom_duc_factory%chain_members(2)%ptr .toBe. .true. )
  call tester%expect( chain%next%next%next == nom_duc_factory%chain_members(4)%ptr .toBe. .true. )
  call tester%expect( chain%next%next%next%prev == nom_duc_factory%chain_members(3)%ptr .toBe. .true. )
  call tester%expect( associated( chain%next%next%next%next ) .toBe. .false. )

  failed = tester%failed()
end function rhyme_nombre_derived_unit_chain_mul_test
