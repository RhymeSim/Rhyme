logical function rhyme_xyz_test () result ( failed )
  use rhyme_xyz_factory
  use rhyme_assertion

  implicit none

  type ( assertion_t ) :: tester

  type ( xyz_t ) :: xxx

  tester = .describe. "xyz"

  call xyz_factory%init('Case1')
  xxx = xyz_factory%generate()

  call tester%expect(xxxid%idx .toBe. 0 .hint. 'Placeholder test')

  failed = tester%failed()

  call xyz_factory%final
end function rhyme_xyz_test
