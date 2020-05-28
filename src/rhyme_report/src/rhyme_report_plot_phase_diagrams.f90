submodule(rhyme_report) plot_phase_diagrams
contains
module subroutine rhyme_report_plot_phase_diagrams(report, samr, logger)
   implicit none

   class(report_t), intent(in) :: report
   type(samr_t), intent(in) :: samr
   type(logger_t), intent(inout) :: logger

   type(report_2d_histogram_t), pointer :: pntr

   integer :: dims(3)
   real(kind=8) :: box_lengths(3)
   real(kind=8), allocatable :: v(:, :)
   character(len=32) :: l(2)

   if (.not. associated(report%phase_diagrams)) return

   dims = samr%levels(0)%boxes(1)%dims
   box_lengths = samr%box_lengths

#if NDIM == 1
#define IDX 1:dims(1)
#define IDY
#define IDZ
#elif NDIM == 2
#define IDX 1:dims(1)
#define IDY , 1:dims(2)
#define IDZ
#elif NDIM ==3
#define IDX 1:dims(1)
#define IDY , 1:dims(2)
#define IDZ , 1:dims(3)
#endif

   pntr => report%phase_diagrams

   allocate (v(2, product(dims)))

   do while (associated(pntr))
      select case (pntr%type)
      case (repid%rho_temp)
         call logger%log('Phase diagram:', 'rho', 'vs.', ['T'])
         v(1, :) = pack(samr%levels(0)%boxes(1)%cells(IDX IDY IDZ, cid%rho), .true.)
         v(2, :) = pack(samr%levels(0)%boxes(1)%cells(IDX IDY IDZ, cid%temp), .true.)
         l = ['Temp', 'Rho ']
      case (repid%p_temp)
         call logger%err('p-T diagram is not implemented yet!')
         return
      case default
         return
      end select

      call logger%histogram( &
         v(1, :), v(2, :), nbins=[72, 72], bin_scales=[plid%log, plid%log], &
         cs_scale=plid%log, axes_scales=[plid%log, plid%log], &
         labels=l, resolution=[72, 72], normalized=.true.)

      pntr => pntr%next
   end do
end subroutine rhyme_report_plot_phase_diagrams
end submodule plot_phase_diagrams
