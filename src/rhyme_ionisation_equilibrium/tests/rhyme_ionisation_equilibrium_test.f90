logical function rhyme_ionisation_equilibrium_test() result(failed)
   use rhyme_ionisation_equilibrium_factory
   use rhyme_assertion

   implicit none

   type(assertion_t) :: tester

   type(ionisation_equilibrium_t) :: ie

   tester = .describe."ionisation_equilibrium"

   ie = ionisation_equilibrium_factory_generate('default')

   call tester%expect(ieid%unset.toBe.-1.hint.'unset id')
   call tester%expect(ieid%case_a.toBe.1.hint.'Case A id')
   call tester%expect(ieid%case_b.toBe.2.hint.'Case B id')

   call tester%expect(ie%case.toBe.ieid%unset.hint.'default case')
   call tester%expect(ie%uvb.toBe..false..hint.'default uvb flag')
   call tester%expect(ie%collisional.toBe..false..hint.'default collisional flag')
   call tester%expect(ie%photo.toBe..false..hint.'default photoionisation flag')

   failed = tester%failed()
end function rhyme_ionisation_equilibrium_test
