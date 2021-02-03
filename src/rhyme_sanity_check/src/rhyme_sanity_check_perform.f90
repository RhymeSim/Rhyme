submodule(rhyme_sanity_check) perform_smod
   use, intrinsic :: IEEE_ARITHMETIC

contains
   module subroutine rhyme_sanity_check_perform(sc, samr, logger)
      implicit none

      type(sanity_check_t), intent(inout) :: sc
      type(samr_t), intent(in) :: samr
      type(logger_t), intent(inout) :: logger

      integer :: dims(NDIM), pos(NDIM)
      real(kind=8) :: min_val

#if NDIM==1
#define JRANGE
#define KRANGE
#define IDPOS pos(1)
#endif
#if NDIM==2
#define JRANGE , 1:dims(2)
#define KRANGE
#define IDPOS pos(1), pos(2)
#endif
#if NDIM==3
#define JRANGE , 1:dims(2)
#define KRANGE , 1:dims(3)
#define IDPOS pos(1), pos(2), pos(3)
#endif

      dims = samr%levels(0)%boxes(1)%dims

      ! General checks
      if (any(IEEE_IS_NAN(samr%levels(0)%boxes(1)%cells))) then
         call logger%warn('NaN found in cells!')
      end if

      if (any(samr%levels(0)%boxes(1)%cells(1:dims(1) JRANGE KRANGE, cid%rho) < 0d0)) then
         pos = minloc(samr%levels(0)%boxes(1)%cells(1:dims(1) JRANGE KRANGE, cid%rho))
         min_val = samr%levels(0)%boxes(1)%cells(IDPOS, cid%rho)
         call logger%warn('Negative density found!', min_val, '@', pos)
      end if

      if (any(samr%levels(0)%boxes(1)%cells(1:dims(1) JRANGE KRANGE, cid%e_tot) < 0d0)) then
         pos = minloc(samr%levels(0)%boxes(1)%cells(1:dims(1) JRANGE KRANGE, cid%e_tot))
         min_val = samr%levels(0)%boxes(1)%cells(IDPOS, cid%e_tot)
         call logger%warn('Negative e_tot found!', min_val, '@', pos)
      end if

      ! Specific checks
      call rhyme_sanity_check_fill(sc, samr)
      call rhyme_sanity_check_print(sc, logger)

   end subroutine rhyme_sanity_check_perform
end submodule perform_smod
