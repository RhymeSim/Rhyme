logical function rhyme_nombre_unit_chain_assignment_test () result ( failed )
  use rhyme_nombre_unit_chain
  use rhyme_assertion

  implicit none

  type ( assertion_t ) :: tester

  tester = .describe. ""

  ! call tester%expect(  )

  failed = tester%failed()
end function rhyme_nombre_unit_chain_assignment_test
