logical function rhyme_xyz_equality_test() result (failed)
  use rhyme_xyz_factory
  use rhyme_assertion

  implicit none

  type(assertion_t) :: tester

  type(xyz_t) :: xxx(2)

  tester = .describe. "xyz_equality"

  call xyz_factory%init('Case1')
  xxx(1) = xyz_factory%generate()

  call xyz_factory%init('Case2')
  xxx(2) = xyz_factory%generate()

  call tester%expect(.false. .toBe. .true. .hint. 'Placeholder test')

  failed = tester%failed()

  call xyz_factory%final
end function rhyme_xyz_equality_test
