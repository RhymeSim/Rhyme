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

   call tester%expect(ie%cases.toBe.ieid%unset.hint.'cases')

   call tester%expect(ie%redhsift.toBe.-1d0.hint.'redhsift')

   call tester%expect(ie%uvb.toBe..false..hint.'uvb flag')
   call tester%expect(ie%uvb_self_shielding.toBe..false..hint.'uvb self-shielding')
   call tester%expect(ie%uvb_ssh.toBe.huge(0d0) .hint.'uvb self-shielding values')
   call tester%expect(ie%gamma_uvb.toBe.-1e0.hint.'uvb gamma')
   call tester%expect(ie%uvb_photoheating.toBe.-1e0.hint.'uvb photoheating')

   call tester%expect(ie%collisional.toBe..false..hint.'default collisional flag')
   call tester%expect(ie%photo.toBe..false..hint.'default photoionisation flag')

   do i = 1, NSPE
      call tester%expect(associated(ie%RI(i)%run) .toBe..false..hint.'RI')
      call tester%expect(associated(ie%CI(i)%run) .toBe..false..hint.'CI')
      call tester%expect(associated(ie%CIE(i)%run) .toBe..false..hint.'CIE')
      call tester%expect(associated(ie%IE(i)%run) .toBe..false..hint.'IE')
   end do

   call tester%expect(ie%table_sizes.toBe.ieid%unset.hint.'table sizes')

   call tester%expect(associated(ie%table_temp_range(1)%u) .toBe..false..hint.'table temp range 1 unit')
   call tester%expect(associated(ie%table_temp_range(2)%u) .toBe..false..hint.'table temp range 2 unit')

   call tester%expect(associated(ie%table_density_range(1)%u) .toBe..false..hint.'table density range 1 unit')
   call tester%expect(associated(ie%table_density_range(2)%u) .toBe..false..hint.'table density range 2 unit')

   call tester%expect(allocated(ie%table) .toBe..false..hint.'table')

   failed = tester%failed()
end function rhyme_ionisation_equilibrium_test
