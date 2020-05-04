logical function rhyme_irs_guess_p_star_test() result(failed)
   use rhyme_irs_factory
   use rhyme_logger_factory
   use rhyme_assertion

   implicit none

   type(assertion_t) :: tester

   type(irs_t) :: irs
   type(logger_t) :: logger

   type(rp_side_t) :: l, r
   real(kind=8) :: p_star(5)
   real(kind=8), dimension(1 + NDIM + 1 + 1) :: rnd1, rnd2
   integer :: i, axis

   tester = .describe.'irs_guess_p_star'

   irs = irs_factory_generate('default')
   logger = logger_factory_generate('default')

   call rhyme_irs_init(irs, logger)

   do i = 1, 10
      call random_number(rnd1)
      call random_number(rnd2)

      l%rho = rnd1(1)
      l%v = rnd1(2:2 + NDIM - 1)
      l%p = rnd1(2 + NDIM)
      l%cs = rnd1(2 + NDIM + 1)

      r%rho = rnd2(1)
      r%v = rnd2(2:2 + NDIM - 1)
      r%p = rnd2(2 + NDIM)
      r%cs = rnd2(2 + NDIM + 1)

      do axis = 1, NDIM
         p_star = rhyme_irs_guess_p_star(l, r, axis, irs%w_vacuum(cid%p))
         call tester%expect(all((p_star - irs%w_vacuum(cid%p) + nearest(0d0, 1d0)) > 0d0) .toBe..true.)
      end do
   end do

   failed = tester%failed()
end function rhyme_irs_guess_p_star_test
