module rhyme_ionisation_equilibrium_assertion
  use rhyme_ionisation_equilibrium
  use rhyme_assertion

  implicit none

  interface operator(.toBe.)
    module procedure rhyme_ionisation_equilibrium_assertion_tobe
  end interface operator(.toBe.)

contains

  module function rhyme_ionisation_equilibrium_assertion_tobe(ie1, ie2) result(test)
    implicit none

    type (ionisation_equilibrium_t), intent(in) :: ie1, ie2
    type (test_t) :: test

    test%op = 'to_be'

    write(test%val, *) ie1
    write(test%exp, *) ie2

    test%is_passed = ie1 == ie2
  end function rhyme_ionisation_equilibrium_assertion_tobe
end module rhyme_ionisation_equilibrium_assertion
