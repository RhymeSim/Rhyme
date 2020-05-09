logical function rhyme_ionisation_equilibrium_test() result(failed)
   use rhyme_ionisation_equilibrium_factory
   use rhyme_assertion

   implicit none

   type(assertion_t) :: tester

   type(ionisation_equilibrium_t) :: ie

   integer :: i

   tester = .describe."ionisation_equilibrium"

   ie = ionisation_equilibrium_factory_generate('default')

   call tester%expect(ieid%unset.toBe.-1.hint.'unset id')
   call tester%expect(ieid%case_a.toBe.1.hint.'Case A id')
   call tester%expect(ieid%case_b.toBe.2.hint.'Case B id')

   call tester%expect(ie%cases.toBe.ieid%unset.hint.'default cases')
   call tester%expect(ie%uvb.toBe..false..hint.'default uvb flag')
   call tester%expect(ie%collisional.toBe..false..hint.'default collisional flag')
   call tester%expect(ie%photo.toBe..false..hint.'default photoionisation flag')

   do i = 1, NSPE
      call tester%expect(associated(ie%RI(i)%run) .toBe..false..hint.'RI')
      call tester%expect(associated(ie%CI(i)%run) .toBe..false..hint.'CI')
      call tester%expect(associated(ie%CIE(i)%run) .toBe..false..hint.'CIE')
   end do

   failed = tester%failed()
end function rhyme_ionisation_equilibrium_test
