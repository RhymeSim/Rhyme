submodule(rhyme_report) plot_pseudocolors
contains
module subroutine rhyme_report_plot_pseudocolors(report, samr, logger)
   implicit none

   class(report_t), intent(in) :: report
   type(samr_t), intent(in) :: samr
   type(logger_t), intent(inout) :: logger

   type(report_pseudocolor_t), pointer :: pntr

   integer :: dims(NDIM)
   real(kind=8) :: box_lengths(NDIM)

   if (.not. associated(report%pseudocolors)) return

   dims = samr%levels(0)%boxes(1)%dims
   box_lengths = samr%box_lengths

#if NDIM > 1

#if NDIM == 2
#define IDXX 1:dims(1)
#define IDXY 1:dims(1)
#define IDXZ 1:dims(1)
#define JDXX , 1:dims(2)
#define JDXY , 1:dims(2)
#define JDXZ , 1:dims(2)
#define KDXX
#define KDXY
#define KDXZ
#define RESX dims(1)
#define RESY dims(2)
#define RESZ 1
#define BLX box_lengths(1)
#define BLY box_lengths(2)
#define BLZ 1d0
#elif NDIM ==3
#define IDXX dims(1)/2
#define IDXY 1:dims(1)
#define IDXZ 1:dims(1)
#define JDXX , 1:dims(2)
#define JDXY , dims(2)/2
#define JDXZ , 1:dims(2)
#define KDXX , 1:dims(3)
#define KDXY , 1:dims(3)
#define KDXZ , dims(3)/2
#define RESX dims(1)
#define RESY dims(2)
#define RESZ dims(3)
#define BLX box_lengths(1)
#define BLY box_lengths(2)
#define BLZ box_lengths(3)
#endif

   pntr => report%pseudocolors

   do while (associated(pntr))
      call logger%log('pseudoplot '//trim(repid%labels(pntr%type)))
      select case (logger%projection_axis)
      case (lgid%x)
         call logger%plot( &
            samr%levels(0)%boxes(1)%cells(IDXX JDXX KDXX, pntr%type), &
            [0d0, BLY], [0d0, BLZ], labels=['X', 'Y'], &
            colorscheme=colorschemes(logger%colormap), &
            auto_setup=.true., resolution=[RESY, RESZ])
      case (lgid%y)
         call logger%plot( &
            samr%levels(0)%boxes(1)%cells(IDXY JDXY KDXY, pntr%type), &
            [0d0, BLX], [0d0, BLZ], labels=['X', 'Y'], &
            colorscheme=colorschemes(logger%colormap), &
            auto_setup=.true., resolution=[RESX, RESZ])
      case (lgid%z)
         call logger%plot( &
            samr%levels(0)%boxes(1)%cells(IDXZ JDXZ KDXZ, pntr%type), &
            [0d0, BLX], [0d0, BLY], labels=['X', 'Y'], &
            colorscheme=colorschemes(logger%colormap), &
            auto_setup=.true., resolution=[RESX, RESY])
      case default
         call logger%err('Unknonw axis!', '', '', [logger%projection_axis])
      end select

      pntr => pntr%next
   end do
#endif
end subroutine rhyme_report_plot_pseudocolors
end submodule plot_pseudocolors
