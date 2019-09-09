logical function rhyme_nombre_derived_unit_chain_clone_test () result ( failed )
  use rhyme_nombre_derived_unit_chain_factory
  use rhyme_assertion

  implicit none

  type ( assertion_t ) :: tester

  type ( nombre_derived_unit_t ), pointer :: chain, clone

  tester = .describe. "nombre_derived_unit_chain_clone"

  chain => nom_duc_factory%generate_chain()
  clone => rhyme_nombre_derived_unit_chain_clone( chain )

  call tester%expect( associated( clone%prev ) .toBe. .false. )
  call tester%expect( clone == nom_duc_factory%chain_members(1)%ptr .toBe. .true. )
  call tester%expect( clone%next == nom_duc_factory%chain_members(2)%ptr .toBe. .true. )
  call tester%expect( clone%next%prev == nom_duc_factory%chain_members(1)%ptr .toBe. .true. )
  call tester%expect( clone%next%next == nom_duc_factory%chain_members(3)%ptr .toBe. .true. )
  call tester%expect( clone%next%next%prev == nom_duc_factory%chain_members(2)%ptr .toBe. .true. )
  call tester%expect( clone%next%next%next == nom_duc_factory%chain_members(4)%ptr .toBe. .true. )
  call tester%expect( clone%next%next%next%prev == nom_duc_factory%chain_members(3)%ptr .toBe. .true. )
  call tester%expect( clone%next%next%next%next == nom_duc_factory%chain_members(5)%ptr .toBe. .true. )
  call tester%expect( clone%next%next%next%next%prev == nom_duc_factory%chain_members(4)%ptr .toBe. .true. )
  call tester%expect( associated( clone%next%next%next%next%next ) .toBe. .false. )

  failed = tester%failed()
end function rhyme_nombre_derived_unit_chain_clone_test
