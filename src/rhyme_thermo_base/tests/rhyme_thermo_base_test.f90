logical function rhyme_thermo_base_test () result ( failed )
  use rhyme_thermo_base_factory
  use rhyme_assertion

  implicit none

  type ( assertion_t ) :: th_tester

  th_tester = .describe. "rhyme_thermo_base"

  call th_tester%expect( thid%monatomic .toBe. 1 )
  call th_tester%expect( thid%diatomic .toBe. 2 )
  call th_tester%expect( thid%polyatomic .toBe. 3 )

  failed = th_tester%failed()
end function rhyme_thermo_base_test
