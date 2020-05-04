logical function rhyme_mh_workspace_init_test() result(failed)
   use rhyme_mh_workspace_factory
   use rhyme_samr_factory
   use rhyme_logger_factory
   use rhyme_assertion

   implicit none

   type(assertion_t) :: tester

   type(mh_workspace_t) :: mhws
   type(samr_t) :: samr
   type(logger_t) :: logger
   integer :: l

   samr = samr_factory%generate()
   logger = logger_factory_generate('default')

   call rhyme_mh_workspace_init(mhws, samr, logger)

   call tester%expect(mhws%nlevels.toBe.samr%nlevels)

   do l = 0, mhws%nlevels - 1
      call tester%expect(mhws%levels(l)%max_nboxes.toBe.samr%levels(l)%max_nboxes)
      call tester%expect(allocated(mhws%levels(l)%boxes) .toBe..true.)
      call tester%expect(size(mhws%levels(l)%boxes) .toBe.samr%levels(l)%max_nboxes)
   end do

   failed = tester%failed()
end function rhyme_mh_workspace_init_test
