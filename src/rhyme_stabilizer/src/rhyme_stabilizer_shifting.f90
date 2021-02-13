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

      if (shift(1) == -1) then
         do uid = 1, NCMP
            LOOP_K
            LOOP_J
            samr%levels(0)%boxes(1)%cells(dims(1) JDX KDX, uid) = &
               samr%levels(0)%boxes(1)%cells(dims(1) - 1 JDX KDX, uid)
            LOOP_J_END
            LOOP_K_END
         end do
      end if

      if (shift(1) == 1) then
         do uid = 1, NCMP
            LOOP_K
            LOOP_J
            samr%levels(0)%boxes(1)%cells(1 JDX KDX, uid) = &
               samr%levels(0)%boxes(1)%cells(2 JDX KDX, uid)
            LOOP_J_END
            LOOP_K_END
         end do
      end if

#if NDIM > 1
      if (shift(2) == -1) then
         do uid = 1, NCMP
            LOOP_K
            do i = 1, dims(1)
               samr%levels(0)%boxes(1)%cells(i, dims(2) KDX, uid) = &
                  samr%levels(0)%boxes(1)%cells(i, dims(2) - 1 KDX, uid)
            end do
            LOOP_K_END
         end do
      end if

      if (shift(2) == 1) then
         do uid = 1, NCMP
            LOOP_K
            do i = 1, dims(1)
               samr%levels(0)%boxes(1)%cells(i, 1 KDX, uid) = &
                  samr%levels(0)%boxes(1)%cells(i, 2 KDX, uid)
            end do
            LOOP_K_END
         end do
      end if

      if (shift(1) == -1 .and. shift(2) == -1) then
         do uid = 1, NCMP
            LOOP_K
            samr%levels(0)%boxes(1)%cells(dims(1), dims(2) KDX, uid) = &
               samr%levels(0)%boxes(1)%cells(dims(1) - 1, dims(2) - 1 KDX, uid)
            LOOP_K_END
         end do
      end if

      if (shift(1) == -1 .and. shift(2) == 1) then
         do uid = 1, NCMP
            LOOP_K
            samr%levels(0)%boxes(1)%cells(dims(1), 1 KDX, uid) = &
               samr%levels(0)%boxes(1)%cells(dims(1) - 1, 2 KDX, uid)
            LOOP_K_END
         end do
      end if

      if (shift(1) == 1 .and. shift(2) == -1) then
         do uid = 1, NCMP
            LOOP_K
            samr%levels(0)%boxes(1)%cells(1, dims(2) KDX, uid) = &
               samr%levels(0)%boxes(1)%cells(2, dims(2) - 1 KDX, uid)
            LOOP_K_END
         end do
      end if

      if (shift(1) == 1 .and. shift(2) == 1) then
         do uid = 1, NCMP
            LOOP_K
            samr%levels(0)%boxes(1)%cells(1, 1 KDX, uid) = &
               samr%levels(0)%boxes(1)%cells(2, 2 KDX, uid)
            LOOP_K_END
         end do
      end if
#endif

#if NDIM > 2
      if (shift(3) == -1) then
         do uid = 1, NCMP
         do j = 1, dims(2)
         do i = 1, dims(1)
            samr%levels(0)%boxes(1)%cells(i, j, dims(3), uid) = &
               samr%levels(0)%boxes(1)%cells(i, j, dims(3) - 1, uid)
         end do
         end do
         end do
      end if

      if (shift(3) == 1) then
         do uid = 1, NCMP
         do j = 1, dims(2)
         do i = 1, dims(1)
            samr%levels(0)%boxes(1)%cells(i, j, 1, uid) = &
               samr%levels(0)%boxes(1)%cells(i, j, 2, uid)
         end do
         end do
         end do
      end if

      if (shift(1) == -1 .and. shift(3) == -1) then
         do uid = 1, NCMP
            do j = 1, dims(3)
               samr%levels(0)%boxes(1)%cells(dims(1), j, dims(3), uid) = &
                  samr%levels(0)%boxes(1)%cells(dims(1) - 1, j, dims(3) - 1, uid)
            end do
         end do
      end if

      if (shift(1) == -1 .and. shift(3) == 1) then
         do uid = 1, NCMP
            do j = 1, dims(3)
               samr%levels(0)%boxes(1)%cells(dims(1), j, 1, uid) = &
                  samr%levels(0)%boxes(1)%cells(dims(1) - 1, j, 2, uid)
            end do
         end do
      end if

      if (shift(1) == 1 .and. shift(3) == -1) then
         do uid = 1, NCMP
            do j = 1, dims(3)
               samr%levels(0)%boxes(1)%cells(1, j, dims(3), uid) = &
                  samr%levels(0)%boxes(1)%cells(2, j, dims(3) - 1, uid)
            end do
         end do
      end if

      if (shift(1) == 1 .and. shift(3) == 1) then
         do uid = 1, NCMP
            do j = 1, dims(3)
               samr%levels(0)%boxes(1)%cells(1, j, 1, uid) = &
                  samr%levels(0)%boxes(1)%cells(2, j, 2, uid)
            end do
         end do
      end if

      if (shift(2) == -1 .and. shift(3) == -1) then
         do uid = 1, NCMP
            do i = 1, dims(3)
               samr%levels(0)%boxes(1)%cells(i, dims(2), dims(3), uid) = &
                  samr%levels(0)%boxes(1)%cells(i, dims(2) - 1, dims(3) - 1, uid)
            end do
         end do
      end if

      if (shift(2) == -1 .and. shift(3) == 1) then
         do uid = 1, NCMP
            do i = 1, dims(3)
               samr%levels(0)%boxes(1)%cells(i, dims(2), 1, uid) = &
                  samr%levels(0)%boxes(1)%cells(i, dims(2) - 1, 2, uid)
            end do
         end do
      end if

      if (shift(2) == 1 .and. shift(3) == -1) then
         do uid = 1, NCMP
            do i = 1, dims(3)
               samr%levels(0)%boxes(1)%cells(i, 1, dims(3), uid) = &
                  samr%levels(0)%boxes(1)%cells(i, 2, dims(3) - 1, uid)
            end do
         end do
      end if

      if (shift(2) == 1 .and. shift(3) == 1) then
         do uid = 1, NCMP
            do i = 1, dims(3)
               samr%levels(0)%boxes(1)%cells(i, 1, 1, uid) = &
                  samr%levels(0)%boxes(1)%cells(i, 2, 2, uid)
            end do
         end do
      end if
#endif

   end subroutine rhyme_stabilizer_shifting
end submodule shifting_smod
