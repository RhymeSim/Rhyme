logical function rhyme_nombre_unit_chain_head_test () result ( failed )
  use rhyme_nombre_unit_chain
  use rhyme_assertion

  implicit none

  type ( assertion_t ) :: tester
  type ( nombre_unit_chain_t ), pointer :: c1, c2, c3
  type ( nombre_unit_chain_t ), pointer :: head

  tester = .describe. "nombre_unit_chain_head"

  c1 => 1 / sec .as. 'Hz'
  c2 => meter / meter .as. 'rad'
  c3 => kg * meter / sec**2 .as. 'N'

  c1%next => c2
  c2%next => c3
  c3%prev => c2
  c2%prev => c1

  head => rhyme_nombre_unit_chain_head( c1 )
  call tester%expect( head == c1 .toBe. .true. )

  head => rhyme_nombre_unit_chain_head( c2 )
  call tester%expect( head == c1 .toBe. .true. )

  head => rhyme_nombre_unit_chain_head( c3 )
  call tester%expect( head == c1 .toBe. .true. )

  failed = tester%failed()
end function rhyme_nombre_unit_chain_head_test
