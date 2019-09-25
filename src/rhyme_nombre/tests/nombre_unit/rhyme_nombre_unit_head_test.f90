logical function rhyme_nombre_unit_head_test () result ( failed )
  use rhyme_nombre_unit_factory
  use rhyme_assertion

  implicit none

  type ( assertion_t ) :: tester
  type ( nombre_unit_t ), pointer :: chain, head

  tester = .describe. "nombre_unit_head"

  call rhyme_nombre_derived_unit_init

  chain => nom_u_factory%generate_chain( [ pascal, hertz, radian, newton ] )

  head => .head. chain
  call tester%expect( head == pascal .toBe. .true. )

  head => .head. chain%next
  call tester%expect( head == pascal .toBe. .true. )

  head => .head. chain%next%next
  call tester%expect( head == pascal .toBe. .true. )

  head => .head. chain%next%next%next
  call tester%expect( head == pascal .toBe. .true. )

  failed = tester%failed()
end function rhyme_nombre_unit_head_test
