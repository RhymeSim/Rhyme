module rhyme_report
   use rhyme_units
   use rhyme_samr
   use rhyme_logger

   implicit none

   type, private :: indices_t
      integer :: p_temp = -7
      integer :: rho_temp = -6
      integer :: v2 = -5
      integer :: abs_v = -4
      integer :: w = -3
      integer :: v = -2
      integer :: u = -1
      integer :: unset = 0
      integer :: rho = cid%rho
      integer :: rho_u = cid%rho_u
#if NDIM > 1
      integer :: rho_v = cid%rho_v
#endif
#if NDIM > 2
      integer :: rho_w = cid%rho_w
#endif
      integer :: e_tot = cid%e_tot
      integer :: temp = cid%temp
      integer :: ntr_frac_0 = cid%ntr_frac_0
#if NSPE > 1
      integer :: ntr_frac_1 = cid%ntr_frac_1
#endif
#if NSPE > 1
      integer :: ntr_frac_2 = cid%ntr_frac_2
#endif

      character(len=16) :: labels(-7:1 + NDIM + 1 + 1 + NSPE) = [ &
                           'p vs temp       ' &
                           , 'rho vs temp     ' &
                           , 'v2              ' &
                           , '|v|             ' &
                           , 'w               ' &
                           , 'v               ' &
                           , 'u               ' &
                           , 'unset           ' &
                           , 'rho             ' &
                           , 'rho_u           ' &
#if NDIM > 1
                           , 'rho_v           ' &
#endif
#if NDIM > 2
                           , 'rho_w           ' &
#endif
                           , 'e_tot           ' &
                           , 'temp            ' &
                           , 'ntr_frac_0      ' &
#if NSPE > 1
                           , 'ntr_frac_1      ' &
#endif
#if NSPE > 2
                           , 'ntr_frac_2      ' &
#endif
                           ]
   end type indices_t

   type(indices_t), parameter :: repid = indices_t()

   type, private :: report_pseudocolor_t
      integer :: type = repid%unset
      type(report_pseudocolor_t), pointer :: next => null()
   end type report_pseudocolor_t

   type, private :: report_2d_histogram_t
      integer :: type = repid%unset
      type(report_2d_histogram_t), pointer :: next => null()
   end type report_2d_histogram_t

   type, private :: report_histogram_t
      integer :: type = repid%unset
      type(report_histogram_t), pointer :: next => null()
   end type report_histogram_t

   type report_t
      integer :: every = repid%unset
      type(report_pseudocolor_t), pointer :: pseudocolors => null()
      type(report_2d_histogram_t), pointer :: phase_diagrams => null()
      type(report_histogram_t), pointer :: histograms => null()
   contains
      procedure :: rhyme_report_write_formatted
      generic :: write (formatted) => rhyme_report_write_formatted
      procedure :: new_psudocolor => rhyme_report_new_pseudocolor
      procedure :: new_phase_diagram => rhyme_report_new_2d_histogram
      procedure :: new_histogram => rhyme_report_new_histogram
      procedure :: plot_pseudocolors => rhyme_report_plot_pseudocolors
      procedure :: plot_phase_diagrams => rhyme_report_plot_phase_diagrams
      procedure :: plot_histograms => rhyme_report_plot_histograms
      procedure :: publish => rhyme_report_publish
   end type report_t

   interface
      module subroutine rhyme_report_init(rep, logger)
         type(report_t), intent(inout) :: rep
         type(logger_t), intent(inout) :: logger
      end subroutine rhyme_report_init

      module subroutine rhyme_report_new_pseudocolor(report, type)
         class(report_t), intent(inout) :: report
         integer, intent(in) :: type
      end subroutine rhyme_report_new_pseudocolor

      module subroutine rhyme_report_new_2d_histogram(report, type)
         class(report_t), intent(inout) :: report
         integer, intent(in) :: type
      end subroutine rhyme_report_new_2d_histogram

      module subroutine rhyme_report_new_histogram(report, type)
         class(report_t), intent(inout) :: report
         integer, intent(in) :: type
      end subroutine rhyme_report_new_histogram

      module subroutine rhyme_report_plot_pseudocolors(report, samr, logger)
         class(report_t), intent(in) :: report
         type(samr_t), intent(in) :: samr
         type(logger_t), intent(inout) :: logger
      end subroutine rhyme_report_plot_pseudocolors

      module subroutine rhyme_report_plot_phase_diagrams(report, samr, logger)
         class(report_t), intent(in) :: report
         type(samr_t), intent(in) :: samr
         type(logger_t), intent(inout) :: logger
      end subroutine rhyme_report_plot_phase_diagrams

      module subroutine rhyme_report_plot_histograms(report, samr, logger)
         class(report_t), intent(in) :: report
         type(samr_t), intent(in) :: samr
         type(logger_t), intent(inout) :: logger
      end subroutine rhyme_report_plot_histograms

      module subroutine rhyme_report_publish(report, samr, logger)
         class(report_t), intent(in) :: report
         type(samr_t), intent(in) :: samr
         type(logger_t), intent(inout) :: logger
      end subroutine rhyme_report_publish
   end interface

contains
   subroutine rhyme_report_write_formatted( &
      this, unit, iotype, v_list, iostat, iomsg)
      implicit none

      class(report_t), intent(in) :: this
      integer, intent(in) :: unit
      character(len=*), intent(in) :: iotype
      integer, intent(in) :: v_list(:)
      integer, intent(out) :: iostat
      character(len=*), intent(inout) :: iomsg

      write (unit, fmt='(A,A,A,A,A,I0,A)', iostat=iostat, iomsg=iomsg) &
         '<report_t', &
         ' iotype="', trim(iotype), '"', &
         ' v_list=', size(v_list), &
         '>'
   end subroutine rhyme_report_write_formatted
end module rhyme_report
