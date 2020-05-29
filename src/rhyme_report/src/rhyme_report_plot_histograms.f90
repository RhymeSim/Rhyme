submodule(rhyme_report) plot_histograms_smod
contains
module subroutine rhyme_report_plot_histograms(report, samr, logger)
   implicit none

   class(report_t), intent(in) :: report
   type(samr_t), intent(in) :: samr
   type(logger_t), intent(inout) :: logger

   type(report_histogram_t), pointer :: pntr
   real(kind=8), allocatable :: v(:)
   character(len=32) :: l(2)
   integer :: dims(NDIM)
   type(samr_box_t) :: box

   if (.not. associated(report%histograms)) return

   box = samr%levels(0)%boxes(1)
   dims = box%dims

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

   pntr => report%histograms
   allocate (v(product(dims)))

   do while (associated(pntr))
      call logger%log('histogram '//repid%labels(pntr%type))

      select case (pntr%type)
      case (repid%v2)
         v = pack( &
             sum(box%cells(IDX IDY IDZ, cid%rho_u:cid%rho_u + NDIM - 1)**2) &
             /box%cells(IDX IDY IDZ, cid%rho)**2, .true.)
         l = ['v^2      ', 'Frequency']
         call logger%histogram( &
            v, bin_scale=plid%linear, &
            axes_scales=[plid%linear, plid%linear], &
            labels=l, normalized=.true.)
      case (repid%abs_v)
         v = pack( &
             sqrt(sum(box%cells(IDX IDY IDZ, cid%rho_u:cid%rho_u + NDIM - 1)**2) &
                  /box%cells(IDX IDY IDZ, cid%rho)**2), .true.)
         l = ['|v|      ', 'Frequency']
         call logger%histogram( &
            v, bin_scale=plid%linear, &
            axes_scales=[plid%linear, plid%linear], &
            labels=l, normalized=.true.)
#if NDIM > 2
      case (repid%w)
         v = pack( &
             box%cells(IDX IDY IDZ, cid%rho_w)/box%cells(IDX IDY IDZ, cid%rho), .true.)
         l = ['w        ', 'Frequency']
         call logger%histogram( &
            v, bin_scale=plid%linear, &
            axes_scales=[plid%linear, plid%linear], &
            labels=l, normalized=.true.)
#endif
#if NDIM > 1
      case (repid%v)
         v = pack( &
             box%cells(IDX IDY IDZ, cid%rho_v)/box%cells(IDX IDY IDZ, cid%rho), .true.)
         l = ['v        ', 'Frequency']
         call logger%histogram( &
            v, bin_scale=plid%linear, &
            axes_scales=[plid%linear, plid%linear], &
            labels=l, normalized=.true.)
#endif
      case (repid%u)
         v = pack( &
             box%cells(IDX IDY IDZ, cid%rho_u)/box%cells(IDX IDY IDZ, cid%rho), .true.)
         l = ['u        ', 'Frequency']
         call logger%histogram( &
            v, bin_scale=plid%linear, &
            axes_scales=[plid%linear, plid%linear], &
            labels=l, normalized=.true.)
      case (repid%rho)
         v = pack(box%cells(IDX IDY IDZ, cid%rho), .true.)
         l = ['rho      ', 'Frequency']
         call logger%histogram( &
            v, bin_scale=plid%log, &
            axes_scales=[plid%log, plid%linear], &
            labels=l, normalized=.true.)
      case (repid%rho_u)
         v = pack(box%cells(IDX IDY IDZ, cid%rho_u), .true.)
         l = ['rho_u    ', 'Frequency']
         call logger%histogram( &
            v, bin_scale=plid%linear, &
            axes_scales=[plid%linear, plid%linear], &
            labels=l, normalized=.true.)
#if NDIM > 1
      case (repid%rho_v)
         v = pack(box%cells(IDX IDY IDZ, cid%rho_v), .true.)
         l = ['rho_v    ', 'Frequency']
         call logger%histogram( &
            v, bin_scale=plid%linear, &
            axes_scales=[plid%linear, plid%linear], &
            labels=l, normalized=.true.)
#endif
#if NDIM > 2
      case (repid%rho_w)
         v = pack(box%cells(IDX IDY IDZ, cid%rho_w), .true.)
         l = ['rho_w    ', 'Frequency']
         call logger%histogram( &
            v, bin_scale=plid%linear, &
            axes_scales=[plid%linear, plid%linear], &
            labels=l, normalized=.true.)
#endif
      case (repid%e_tot)
         v = pack(box%cells(IDX IDY IDZ, cid%e_tot), .true.)
         l = ['e_tot    ', 'Frequency']
         call logger%histogram( &
            v, bin_scale=plid%log, &
            axes_scales=[plid%log, plid%linear], &
            labels=l, normalized=.true.)
      case (repid%temp)
         v = pack(box%cells(IDX IDY IDZ, cid%temp), .true.)
         l = ['temp     ', 'Frequency']
         call logger%histogram( &
            v, bin_scale=plid%log, &
            axes_scales=[plid%log, plid%linear], &
            labels=l, normalized=.true.)
      case (repid%ntr_frac_0)
         v = pack(box%cells(IDX IDY IDZ, cid%ntr_frac_0), .true.)
         l = ['ntr_frac_0', 'Frequency ']
         call logger%histogram( &
            v, bin_scale=plid%log, &
            axes_scales=[plid%log, plid%linear], &
            labels=l, normalized=.true.)
#if NSPE > 1
      case (repid%ntr_frac_1)
         v = pack(box%cells(IDX IDY IDZ, cid%ntr_frac_1), .true.)
         l = ['ntr_frac_1', 'Frequency ']
         call logger%histogram( &
            v, bin_scale=plid%log, &
            axes_scales=[plid%log, plid%linear], &
            labels=l, normalized=.true.)
#endif
#if NSPE > 2
      case (repid%ntr_frac_2)
         v = pack(box%cells(IDX IDY IDZ, cid%ntr_frac_2), .true.)
         l = ['ntr_frac_2', 'Frequency ']
         call logger%histogram( &
            v, bin_scale=plid%log, &
            axes_scales=[plid%log, plid%linear], &
            labels=l, normalized=.true.)
#endif
      end select

      pntr => pntr%next
   end do

   deallocate (v)
end subroutine rhyme_report_plot_histograms
end submodule plot_histograms_smod
