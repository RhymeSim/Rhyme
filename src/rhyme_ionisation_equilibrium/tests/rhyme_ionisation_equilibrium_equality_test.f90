logical function rhyme_ionisation_equilibrium_equality_test() result(failed)
   use rhyme_ionisation_equilibrium_factory
   use rhyme_assertion

   implicit none

   type(assertion_t) :: tester

   type(ionisation_equilibrium_t) :: ie(2)

   tester = .describe."ionisation_equilibrium_equality"

   ie(1) = ionisation_equilibrium_factory_generate('CaseA')
   ie(2) = ionisation_equilibrium_factory_generate('CaseB')

   call tester%expect(ie(1) == ie(1) .toBe..true.)
   call tester%expect(ie(2) == ie(1) .toBe..false.)
   call tester%expect(ie(2) == ie(2) .toBe..true.)
   call tester%expect(ie(1) == ie(2) .toBe..false.)

   failed = tester%failed()
end function rhyme_ionisation_equilibrium_equality_test
