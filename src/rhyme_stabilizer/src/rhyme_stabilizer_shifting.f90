submodule(rhyme_stabilizer) shifting_smod
contains
   module subroutine rhyme_stabilizer_shifting(st, samr, shift)
      type(stabilizer_t), intent(in) :: st
      type(samr_t), intent(inout) :: samr
      integer, intent(in) :: shift(NDIM)

#if NDIM == 1
#define JDX
#define KDX
#define SH_JDX
#define SH_KDX
#define LOOP_J
#define LOOP_K
#define LOOP_J_END
#define LOOP_K_END
#endif
#if NDIM == 2
#define JDX , j
#define KDX
#define SH_JDX , j - shift(2)
#define SH_KDX
#define LOOP_J do j = 1, dims(2)
#define LOOP_K
#define LOOP_J_END end do
#define LOOP_K_END
#endif
#if NDIM == 3
#define JDX , j
#define KDX , k
#define SH_JDX , j - shift(2)
#define SH_KDX , k - shift(3)
#define LOOP_J do j = 1, dims(2)
#define LOOP_K do k = 1, dims(3)
#define LOOP_J_END end do
#define LOOP_K_END end do
#endif

      integer :: uid, i JDX KDX, dims(NDIM)

      dims = samr%levels(0)%boxes(1)%dims

      do uid = 1, NCMP
         LOOP_K
         LOOP_J
         do i = 1, dims(1)
            samr%levels(0)%boxes(1)%cells(i JDX KDX, uid) = &
               samr%levels(0)%boxes(1)%cells(i - shift(1) SH_JDX SH_KDX, uid)
         end do
         LOOP_J_END
         LOOP_K_END
      end do

#if NDIM > 1
      ! TODO
#endif

#if NDIM > 2
      if (shift(1) == -1 .and. shift(2) == -1) then
         do k = 1, dims(3)
            samr%levels(0)%boxes(1)%cells(dims(1), dims(2), k, :) = &
               samr%levels(0)%boxes(1)%cells(dims(1) - 1, dims(2) - 1, k, :)
         end do
      end if

      if (shift(1) == -1 .and. shift(3) == -1) then
         do j = 1, dims(3)
            samr%levels(0)%boxes(1)%cells(dims(1), j, dims(3), :) = &
               samr%levels(0)%boxes(1)%cells(dims(1) - 1, j, dims(3) - 1, :)
         end do
      end if

      if (shift(2) == -1 .and. shift(3) == -1) then
         do i = 1, dims(3)
            samr%levels(0)%boxes(1)%cells(i, dims(2), dims(3), :) = &
               samr%levels(0)%boxes(1)%cells(i, dims(2) - 1, dims(3) - 1, :)
         end do
      end if
#endif

   end subroutine rhyme_stabilizer_shifting
end submodule shifting_smod
