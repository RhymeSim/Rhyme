logical function rhyme_chemistry_init_test () result (failed)
  use rhyme_chemistry
  use rhyme_assertion

  implicit none

  type ( assertion_t ) :: ch_tester

  type ( chemistry_t ) :: chemi
  type ( log_t ) :: log
  type ( unit_t ), pointer :: kg__mol

  ch_tester = .describe. "chemistry init"

  call chemi%init( log )

  kg__mol => kg / mol

  call ch_tester%expect( chemi%molar%e%v .toBe. 5.48580d-7 )
  call ch_tester%expect( chemi%molar%e%u%p() .toBe. kg__mol%p() )
  call ch_tester%expect( chemi%atomic%H%v .toBe. 6.6464764d-27 )
  call ch_tester%expect( chemi%atomic%H%u%p() .toBe. kg%p() )
  call ch_tester%expect( chemi%amu%one%v .toBe. 1.66054d-27 )
  call ch_tester%expect( chemi%amu%one%u%p() .toBe. kg%p() )

  failed = ch_tester%failed()
end function rhyme_chemistry_init_test
