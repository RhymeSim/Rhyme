logical function rhyme_nombre_unit_chain_tail_test () result ( failed )
  use rhyme_nombre_unit_chain
  use rhyme_assertion

  implicit none

  type ( assertion_t ) :: tester
  type ( nombre_unit_chain_t ), pointer :: chain
  type ( nombre_unit_t ), pointer :: tail_ptr

  tester = .describe. "nombre_unit_chain_tail"

  chain => meter * kg

  tail_ptr => rhyme_nombre_unit_chain_tail( chain )

  call tester%expect( tail_ptr == kg .toBe. .true. )

  failed = tester%failed()
end function rhyme_nombre_unit_chain_tail_test
