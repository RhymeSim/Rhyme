logical function rhyme_stabilizer_shifting_test() result(failed)
   use rhyme_stabilizer_factory
   use rhyme_samr_factory
   use rhyme_logger_factory
   use rhyme_assertion

   implicit none

   type(assertion_t) :: tester

   type(stabilizer_t) :: st
   type(samr_t) :: samr, samr_orig
   type(logger_t) :: logger

#if NDIM == 3
   integer :: i, j, k, uid, dims(NDIM)
#endif

   tester = .describe."stabilizer_shifting"

   st = stabilizer_factory_generate('linear-rho^2')
   samr = samr_factory%generate(physical=.true.)
   samr_orig = samr_factory%generate(physical=.true.)
   logger = logger_factory_generate('default')

   call rhyme_stabilizer_init(st, samr, logger)

#if NDIM == 3
   dims = samr%levels(0)%boxes(1)%dims

   do uid = 1, NCMP
      do k = 1, dims(3)
      do j = 1, dims(2)
      do i = 1, dims(1)
         samr_orig%levels(0)%boxes(1)%cells(i, j, k, uid) = &
            samr%levels(0)%boxes(1)%cells(i, j, k, uid)
      end do
      end do
      end do
   end do

   call rhyme_stabilizer_shifting(st, samr, [-1, -1, 0])

   do k = 1, dims(3) - 1
   do j = 1, dims(2) - 1
   do i = 1, dims(1) - 1
      call tester%expect( &
         samr%levels(0)%boxes(1)%cells(i, j, k, :) .toBe. &
         samr_orig%levels(0)%boxes(1)%cells(i + 1, j + 1, k, :))
   end do
   end do
   end do

   do k = 1, dims(3)
      call tester%expect( &
         samr%levels(0)%boxes(1)%cells(dims(1), dims(2), k, :) .toBe. &
         samr_orig%levels(0)%boxes(1)%cells(dims(1), dims(2), k, :))
      call tester%expect( &
         samr%levels(0)%boxes(1)%cells(dims(1), dims(2), k, :) .toBe. &
         samr%levels(0)%boxes(1)%cells(dims(1) - 1, dims(2) - 1, k, :))
   end do
#endif

   failed = tester%failed()
end function rhyme_stabilizer_shifting_test
