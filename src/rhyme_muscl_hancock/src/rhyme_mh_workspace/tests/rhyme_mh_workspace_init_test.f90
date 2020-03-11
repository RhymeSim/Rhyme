logical function rhyme_mh_workspace_init_test() result(failed)
   use rhyme_mh_workspace
   use rhyme_samr_factory
   use rhyme_logger_factory
   use rhyme_assertion

   implicit none

   type(assertion_t) :: mhws_tester

   type(mh_workspace_t) :: mhws
   type(samr_t) :: samr
   type(logger_t) :: logger
   integer :: l

   logger = log_factory%generate()
   samr = samr_factory%generate()

   call rhyme_mh_workspace_init(mhws, samr, logger)

   call mhws_tester%expect(mhws%nlevels.toBe.samr%nlevels)

   do l = 0, mhws%nlevels - 1
      call mhws_tester%expect(mhws%levels(l)%max_nboxes.toBe.samr%levels(l)%max_nboxes)
      call mhws_tester%expect(allocated(mhws%levels(l)%boxes) .toBe..true.)
      call mhws_tester%expect(size(mhws%levels(l)%boxes) .toBe.samr%levels(l)%max_nboxes)
   end do

   failed = mhws_tester%failed()
end function rhyme_mh_workspace_init_test
