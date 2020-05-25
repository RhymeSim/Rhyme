module rhyme_logger
   use, intrinsic :: iso_fortran_env, only: stdin => input_unit, stdout => output_unit, stderr => error_unit
   use rhyme_plotter
   use rhyme_string

   implicit none

   type, private :: logger_indices_t
      integer :: unset = -1
      integer :: x = 1, y = 2, z = 3
   end type logger_indices_t

   type(logger_indices_t), parameter :: lgid = logger_indices_t()

   type, private :: logger_const_t
      integer :: closed = -10
      character(len=45) :: time_fmt = "(A,I4,A,I0.2,A,I0.2,A,I0.2,A,I0.2,A,I0.2,A)"
      character(len=7) :: default_logfile = "log.txt"
      character(len=7) :: default_errfile = "err.txt"
   end type logger_const_t

   type(logger_const_t), parameter :: logger_const = logger_const_t()

   type logger_t
      logical :: initialized = .false.
      character(len=1024) :: logfile = logger_const%default_logfile
      character(len=1024) :: errfile = logger_const%default_errfile
      character(len=1024), dimension(10) :: colored_logo = ''
      character(len=1024), dimension(10) :: logo = ''
      character(len=32) :: sections(32) = ''
      integer :: section_starts_at(32, 8) = 0
      integer :: secid = 0
      integer :: logfile_unit = logger_const%closed
      integer :: errfile_unit = logger_const%closed
      integer :: t(8)
      logical :: unicode_plotting = .false.
      integer :: projection_axis = lgid%unset
      integer :: colormap = lgid%unset
   contains
      procedure :: init => rhyme_logger_init
      procedure :: begin_section => rhyme_logger_begin_section
      procedure :: end_section => rhyme_logger_end_section

      procedure :: log => rhyme_logger_log
      procedure :: warn => rhyme_logger_warn
      procedure :: err => rhyme_logger_err

      procedure :: plot_image => rhyme_logger_plot_image
      generic :: plot => plot_image

      procedure :: plot_histogram => rhyme_logger_plot_histogram
      procedure :: plot_2d_histogram => rhyme_logger_plot_2d_histogram
      generic :: histogram => plot_histogram, plot_2d_histogram

      procedure :: update_time => rhyme_logger_update_time
      procedure :: time => rhyme_logger_time
      procedure :: time_and_section => rhyme_logger_time_and_section
      procedure :: tas => rhyme_logger_time_and_section

      procedure :: set_logo => rhyme_logger_set_logo
      procedure :: set_colored_logo => rhyme_logger_set_colored_logo

      procedure :: open_logfile => rhyme_logger_open_logfile
      procedure :: close_logfile => rhyme_logger_close_logfile
      procedure :: open_errfile => rhyme_logger_open_errfile
      procedure :: close_errfile => rhyme_logger_close_errfile

   end type logger_t

   interface
      module subroutine rhyme_logger_log(logger, message, key, ope, val)
         class(logger_t), intent(inout) :: logger
         character(len=*), intent(in) :: message
         class(*), intent(in), optional :: key
         character(len=*), intent(in), optional :: ope
         class(*), intent(in), optional :: val(:)
      end subroutine rhyme_logger_log

      module subroutine rhyme_logger_warn(logger, message, key, ope, val)
         class(logger_t), intent(inout) :: logger
         character(len=*), intent(in) :: message
         class(*), intent(in), optional :: key
         character(len=*), intent(in), optional :: ope
         class(*), intent(in), optional :: val(:)
      end subroutine rhyme_logger_warn

      module subroutine rhyme_logger_err(logger, message, key, ope, val)
         class(logger_t), intent(inout) :: logger
         character(len=*), intent(in) :: message
         class(*), intent(in), optional :: key
         character(len=*), intent(in), optional :: ope
         class(*), intent(in), optional :: val(:)
      end subroutine rhyme_logger_err

      module subroutine rhyme_logger_plot_image( &
         logger, values, xrange, yrange, labels, cs_range, cs_scale, colorscheme, &
         axes_scales, auto_setup)
         class(logger_t), intent(inout) :: logger
         real(kind=8), intent(in) :: values(:, :)
         real(kind=8), intent(in) :: xrange(2), yrange(2)
         character(len=*), intent(in), optional :: labels(2)
         real(kind=8), intent(in), optional :: cs_range(2)
         integer, intent(in), optional :: cs_scale
         type(colorscheme_t), intent(in), optional :: colorscheme
         integer, intent(in), optional :: axes_scales(2)
         logical, intent(in), optional :: auto_setup
      end subroutine rhyme_logger_plot_image

      module subroutine rhyme_logger_plot_histogram( &
         logger, values, nbins, bin_scale, domain, normalized, labels, axes_scales)
         class(logger_t), intent(inout) :: logger
         real(kind=8), intent(in) :: values(:)
         integer, intent(in), optional :: nbins, bin_scale
         real(kind=8), intent(in), optional :: domain(2)
         logical, intent(in), optional :: normalized
         character(len=*), intent(in), optional :: labels(2)
         integer, intent(in), optional :: axes_scales(2)
      end subroutine rhyme_logger_plot_histogram

      module subroutine rhyme_logger_plot_2d_histogram( &
         logger, xvalues, yvalues, nbins, bin_scales, xdomain, ydomain, &
         normalized, labels, cs_range, cs_scale, colorscheme, axes_scales)
         class(logger_t), intent(inout) :: logger
         real(kind=8), intent(in) :: xvalues(:), yvalues(:)
         integer, intent(in), optional :: nbins(2), bin_scales(2)
         real(kind=8), intent(in), optional :: xdomain(2), ydomain(2)
         logical, intent(in), optional :: normalized
         character(len=*), intent(in), optional :: labels(2)
         real(kind=8), intent(in), optional :: cs_range(2)
         integer, intent(in), optional :: cs_scale
         type(colorscheme_t), intent(in), optional :: colorscheme
         integer, intent(in), optional :: axes_scales(2)
      end subroutine rhyme_logger_plot_2d_histogram

      module subroutine rhyme_logger_update_time(logger)
         class(logger_t), intent(inout) :: logger
      end subroutine rhyme_logger_update_time

      module function rhyme_logger_time(logger, color) result(time_str)
         class(logger_t), intent(inout) :: logger
         character(len=*), intent(in), optional :: color
         character(len=64) :: time_str
      end function rhyme_logger_time

      module function rhyme_logger_time_and_section(logger, color) result(tas_str)
         class(logger_t), intent(inout) :: logger
         character(len=*), intent(in), optional :: color
         character(len=126) :: tas_str
      end function rhyme_logger_time_and_section

      module subroutine rhyme_logger_open_logfile(logger)
         class(logger_t), intent(inout) :: logger
      end subroutine rhyme_logger_open_logfile

      module subroutine rhyme_logger_close_logfile(logger)
         class(logger_t), intent(inout) :: logger
      end subroutine rhyme_logger_close_logfile

      module subroutine rhyme_logger_open_errfile(logger)
         class(logger_t), intent(inout) :: logger
      end subroutine rhyme_logger_open_errfile

      module subroutine rhyme_logger_close_errfile(logger)
         class(logger_t), intent(inout) :: logger
      end subroutine rhyme_logger_close_errfile

      module subroutine rhyme_logger_set_colored_logo(logger)
         class(logger_t), intent(inout) :: logger
      end subroutine rhyme_logger_set_colored_logo

      module subroutine rhyme_logger_set_logo(logger)
         class(logger_t), intent(inout) :: logger
      end subroutine rhyme_logger_set_logo

      module subroutine rhyme_logger_begin_section(logger, section)
         class(logger_t), intent(inout) :: logger
         class(*), intent(in) :: section
      end subroutine rhyme_logger_begin_section

      module subroutine rhyme_logger_end_section(logger, print_duration)
         class(logger_t), intent(inout) :: logger
         logical, intent(in), optional :: print_duration
      end subroutine rhyme_logger_end_section
   end interface

contains

   subroutine rhyme_logger_init(logger, str)
      implicit none

      class(logger_t), intent(inout) :: logger
      character(len=*), intent(in) :: str

      integer :: i

      if (logger%initialized) return

      call rhyme_color_init

      call logger%update_time
      call logger%set_logo
      call logger%set_colored_logo

      write (logger%logfile, fmt="(I4,A,I0.2,A,I0.2,A,I0.2,A,I0.2,A,I0.2,A,A)") &
         logger%t(1), '-', logger%t(2), '-', logger%t(3), '-', logger%t(5), '-', &
         logger%t(6), '-', logger%t(7), '-'//trim(.filename.str), '.log.txt'

      write (logger%errfile, fmt="(I4,A,I0.2,A,I0.2,A,I0.2,A,I0.2,A,I0.2,A,A)") &
         logger%t(1), '-', logger%t(2), '-', logger%t(3), '-', logger%t(5), '-', &
         logger%t(6), '-', logger%t(7), '-'//trim(.filename.str), '.err.txt'

      ! Logo
      call logger%open_logfile
      call logger%open_errfile

      do i = 1, size(logger%logo)
         write (stdout, *) trim(logger%colored_logo(i))
         write (logger%logfile_unit, *) trim(logger%logo(i))
         write (logger%errfile_unit, *) trim(logger%logo(i))
      end do

      call logger%close_logfile
      call logger%close_errfile

      logger%secid = 0

      logger%initialized = .true.
   end subroutine rhyme_logger_init

   function concat_components(msg, key, op, val, color) result(str)
      implicit none

      character(len=*), intent(in) :: msg, key, op, val
      character(len=*), intent(in), optional :: color
      character(len=2048) :: str

      character(len=16) :: clr

      if (present(color)) then
         clr = color
      else
         clr = ''
      end if

      str = trim(adjustl(msg))//' ' &
            //trim(adjustl(key))//' ' &
            //trim(adjustl(clr)) &
            //trim(adjustl(op))//tc%nc//' ' &
            //trim(adjustl(val))

      str = trim(adjustl(str))
   end function concat_components
end module rhyme_logger
