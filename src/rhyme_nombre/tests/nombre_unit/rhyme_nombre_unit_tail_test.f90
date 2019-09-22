logical function rhyme_nombre_unit_tail_test () result ( failed )
  use rhyme_nombre_unit_factory
  use rhyme_assertion

  implicit none

  type ( assertion_t ) :: tester
  type ( nombre_unit_t ), pointer :: chain, tail

  tester = .describe. "nombre_unit_tail"

  call rhyme_nombre_derived_unit_init

  chain => nom_duc_factory%generate_chain( [ hydrogen_mass, light_year, electron_volt ] )

  tail => .tail. chain
  call tester%expect( tail == electron_volt .toBe. .true. )

  tail => .tail. chain%next
  call tester%expect( tail == electron_volt .toBe. .true. )

  tail => .tail. chain%next%next
  call tester%expect( tail == electron_volt .toBe. .true. )

  failed = tester%failed()
end function rhyme_nombre_unit_tail_test
