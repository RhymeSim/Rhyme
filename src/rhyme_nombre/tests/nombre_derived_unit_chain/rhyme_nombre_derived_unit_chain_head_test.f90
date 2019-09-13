logical function rhyme_nombre_derived_unit_chain_head_test () result ( failed )
  use rhyme_nombre_derived_unit_chain_factory
  use rhyme_assertion

  implicit none

  type ( assertion_t ) :: tester
  type ( nombre_derived_unit_t ), pointer :: chain, head

  tester = .describe. "nombre_derived_unit_chain_head"

  call rhyme_nombre_derived_unit_init

  chain => nom_duc_factory%generate_chain( [ pascal, hertz, radian, newton ] )

  head => .head. chain
  call tester%expect( head == pascal .toBe. .true. )

  head => .head. chain%next
  call tester%expect( head == pascal .toBe. .true. )

  head => .head. chain%next%next
  call tester%expect( head == pascal .toBe. .true. )

  head => .head. chain%next%next%next
  call tester%expect( head == pascal .toBe. .true. )

  failed = tester%failed()
end function rhyme_nombre_derived_unit_chain_head_test
