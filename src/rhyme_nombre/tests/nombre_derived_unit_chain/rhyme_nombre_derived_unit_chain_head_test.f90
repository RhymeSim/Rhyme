logical function rhyme_nombre_derived_unit_chain_head_test () result ( failed )
  use rhyme_nombre_derived_unit_chain_factory
  use rhyme_assertion

  implicit none

  type ( assertion_t ) :: tester
  type ( nombre_derived_unit_t ), pointer :: chain, head

  tester = .describe. "nombre_derived_unit_chain_head"

  chain => nom_duc_factory%generate_chain()

  head => rhyme_nombre_derived_unit_chain_head( chain )
  call tester%expect( head == nom_duc_factory%chain_members(1)%ptr .toBe. .true. )

  head => rhyme_nombre_derived_unit_chain_head( chain%next )
  call tester%expect( head == nom_duc_factory%chain_members(1)%ptr .toBe. .true. )

  head => rhyme_nombre_derived_unit_chain_head( chain%next%next )
  call tester%expect( head == nom_duc_factory%chain_members(1)%ptr .toBe. .true. )

  head => rhyme_nombre_derived_unit_chain_head( chain%next%next%next )
  call tester%expect( head == nom_duc_factory%chain_members(1)%ptr .toBe. .true. )

  head => rhyme_nombre_derived_unit_chain_head( chain%next%next%next%next )
  call tester%expect( head == nom_duc_factory%chain_members(1)%ptr .toBe. .true. )

  failed = tester%failed()
end function rhyme_nombre_derived_unit_chain_head_test
