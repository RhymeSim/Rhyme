logical function rhyme_samr_weighted_average_of_distances_test() result(failed)
   use rhyme_samr_factory
   use rhyme_assertion

   implicit none

#if NDIM == 1
#define JCOLON
#define KCOLON
#define JRANGE
#define KRANGE
#define JDX
#define KDX
#define LOOP_J
#define LOOP_K
#define LOOP_J_END
#define LOOP_K_END
#define JHALF
#define KHALF
#endif
#if NDIM == 2
#define JCOLON , :
#define KCOLON
#define JRANGE , 1:dims(2)
#define KRANGE
#define JDX , j
#define KDX
#define LOOP_J do j = 1, dims(2)
#define LOOP_K
#define LOOP_J_END end do
#define LOOP_K_END
#define JHALF , dims(2) / 2
#define KHALF
#endif
#if NDIM == 3
#define JCOLON , :
#define KCOLON , :
#define JRANGE , 1:dims(2)
#define KRANGE , 1:dims(3)
#define JDX , j
#define KDX , k
#define LOOP_J do j = 1, dims(2)
#define LOOP_K do k = 1, dims(3)
#define LOOP_J_END end do
#define LOOP_K_END end do
#define JHALF , dims(2) / 2
#define KHALF, dims(3) / 2
#endif

   type(assertion_t) :: tester

   type(samr_t) :: samr
   real(kind=8) :: center(NDIM)
   real(kind=8), allocatable :: coords(:JCOLON KCOLON, :), weights(:JCOLON KCOLON)

   integer :: dims(NDIM), i JDX KDX

   tester = .describe."samr_weighted_average_of_distances"

   samr = samr_factory%generate(physical=.true.)

   dims = samr%levels(0)%boxes(1)%dims

   allocate (coords(1:dims(1) JRANGE KRANGE, NDIM))
   allocate (weights(1:dims(1) JRANGE KRANGE))

   LOOP_K
   LOOP_J
   do i = 1, dims(1)
      coords(i JDX KDX, :) = [i JDX KDX]
   end do
   LOOP_J_END
   LOOP_K_END

   center = rhyme_samr_weighted_average_of_distances(samr, cid%rho, 2d0, coords, weights)

   call tester%expect(.notToBeNaN.center.hint.'center')

   samr%levels(0)%boxes(1)%cells(:JCOLON KCOLON, cid%rho) = 0d0
   center = rhyme_samr_weighted_average_of_distances(samr, cid%rho, 2d0, coords, weights)

   call tester%expect(center.toBe.coords(dims(1)/2 JHALF KHALF, :) .hint.'zero average density')

   deallocate (coords, weights)

   failed = tester%failed()
end function rhyme_samr_weighted_average_of_distances_test
