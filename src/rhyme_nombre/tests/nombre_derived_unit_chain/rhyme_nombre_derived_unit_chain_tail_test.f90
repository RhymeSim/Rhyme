logical function rhyme_nombre_derived_unit_chain_tail_test () result ( failed )
  use rhyme_nombre_derived_unit_chain_factory
  use rhyme_assertion

  implicit none

  type ( assertion_t ) :: tester
  type ( nombre_derived_unit_t ), pointer :: chain, tail

  tester = .describe. "nombre_derived_unit_chain_tail"

  chain => nom_duc_factory%generate_chain()

  tail => rhyme_nombre_derived_unit_chain_tail( chain )
  call tester%expect( tail == nom_duc_factory%chain_members(5)%ptr .toBe. .true. )

  tail => rhyme_nombre_derived_unit_chain_tail( chain%next )
  call tester%expect( tail == nom_duc_factory%chain_members(5)%ptr .toBe. .true. )

  tail => rhyme_nombre_derived_unit_chain_tail( chain%next%next )
  call tester%expect( tail == nom_duc_factory%chain_members(5)%ptr .toBe. .true. )

  tail => rhyme_nombre_derived_unit_chain_tail( chain%next%next%next )
  call tester%expect( tail == nom_duc_factory%chain_members(5)%ptr .toBe. .true. )

  tail => rhyme_nombre_derived_unit_chain_tail( chain%next%next%next%next )
  call tester%expect( tail == nom_duc_factory%chain_members(5)%ptr .toBe. .true. )

  failed = tester%failed()
end function rhyme_nombre_derived_unit_chain_tail_test
