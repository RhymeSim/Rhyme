logical function rhyme_nombre_unit_chain_tail_test () result ( failed )
  use rhyme_nombre_unit_chain
  use rhyme_assertion

  implicit none

  type ( assertion_t ) :: tester
  type ( nombre_unit_chain_t ), pointer :: c1, c2, c3
  type ( nombre_unit_chain_t ), pointer :: tail

  tester = .describe. "nombre_unit_chain_tail"

  ! c1 => sec**(-1) .as. 'Hz'
  ! c2 => meter / meter .as. 'rad'
  ! c3 => kg * meter / sec**2 .as. 'N'
  !
  ! c1%next => c2
  ! c2%next => c3
  ! c3%prev => c2
  ! c2%prev => c1
  !
  ! tail => rhyme_nombre_unit_chain_tail( c1 )
  ! call tester%expect( tail == kg .toBe. .true. )

  ! chain => meter * kg
  !
  ! tail_ptr => rhyme_nombre_unit_chain_tail( chain )
  !

  failed = tester%failed()
end function rhyme_nombre_unit_chain_tail_test
