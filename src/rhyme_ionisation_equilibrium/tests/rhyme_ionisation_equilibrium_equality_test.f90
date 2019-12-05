logical function rhyme_ionisation_equilibrium_equality_test() result (failed)
  use rhyme_ionisation_equilibrium_factory
  use rhyme_assertion

  implicit none

  type(assertion_t) :: tester

  type(ionisation_equilibrium_t) :: ie(2)

  tester = .describe. "ionisation_equilibrium_equality"

  call ie_factory%init('CaseA')
  ie(1) = ie_factory%generate()

  call ie_factory%init('CaseB')
  ie(2) = ie_factory%generate()

  call tester%expect(ie(1) == ie(1) .toBe. .true.)
  call tester%expect(ie(2) == ie(1) .toBe. .false.)
  call tester%expect(ie(2) == ie(2) .toBe. .true.)
  call tester%expect(ie(1) == ie(2) .toBe. .false.)

  failed = tester%failed()

  call ie_factory%final
end function rhyme_ionisation_equilibrium_equality_test
