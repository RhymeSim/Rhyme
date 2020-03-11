module rhyme_logger
   use, intrinsic :: iso_fortran_env, only: stdin => input_unit, stdout => output_unit, stderr => error_unit
   use rhyme_plotter
   use rhyme_string

   implicit none

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
   contains
      procedure :: init => rhyme_logger_init
      procedure :: begin_section => rhyme_logger_begin_section
      procedure :: end_section => rhyme_logger_end_section

      procedure :: log => rhyme_logger_log
      procedure :: warn => rhyme_logger_warn
      procedure :: err => rhyme_logger_err

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
      module subroutine rhyme_logger_log(this, message, key, ope, val)
         class(logger_t), intent(inout) :: this
         character(len=*), intent(in) :: message
         class(*), intent(in), optional :: key
         character(len=*), intent(in), optional :: ope
         class(*), intent(in), optional :: val(:)
      end subroutine rhyme_logger_log

      module subroutine rhyme_logger_warn(this, message, key, ope, val)
         class(logger_t), intent(inout) :: this
         character(len=*), intent(in) :: message
         class(*), intent(in), optional :: key
         character(len=*), intent(in), optional :: ope
         class(*), intent(in), optional :: val(:)
      end subroutine rhyme_logger_warn

      module subroutine rhyme_logger_err(this, message, key, ope, val)
         class(logger_t), intent(inout) :: this
         character(len=*), intent(in) :: message
         class(*), intent(in), optional :: key
         character(len=*), intent(in), optional :: ope
         class(*), intent(in), optional :: val(:)
      end subroutine rhyme_logger_err

      module subroutine rhyme_logger_update_time(this)
         class(logger_t), intent(inout) :: this
      end subroutine rhyme_logger_update_time

      module function rhyme_logger_time(this, color) result(time_str)
         class(logger_t), intent(inout) :: this
         character(len=*), intent(in), optional :: color
         character(len=64) :: time_str
      end function rhyme_logger_time

      module function rhyme_logger_time_and_section(this, color) result(tas_str)
         class(logger_t), intent(inout) :: this
         character(len=*), intent(in), optional :: color
         character(len=126) :: tas_str
      end function rhyme_logger_time_and_section

      module subroutine rhyme_logger_open_logfile(this)
         class(logger_t), intent(inout) :: this
      end subroutine rhyme_logger_open_logfile

      module subroutine rhyme_logger_close_logfile(this)
         class(logger_t), intent(inout) :: this
      end subroutine rhyme_logger_close_logfile

      module subroutine rhyme_logger_open_errfile(this)
         class(logger_t), intent(inout) :: this
      end subroutine rhyme_logger_open_errfile

      module subroutine rhyme_logger_close_errfile(this)
         class(logger_t), intent(inout) :: this
      end subroutine rhyme_logger_close_errfile

      module subroutine rhyme_logger_set_colored_logo(this)
         class(logger_t), intent(inout) :: this
      end subroutine rhyme_logger_set_colored_logo

      module subroutine rhyme_logger_set_logo(this)
         class(logger_t), intent(inout) :: this
      end subroutine rhyme_logger_set_logo

      module subroutine rhyme_logger_begin_section(this, section)
         class(logger_t), intent(inout) :: this
         class(*), intent(in) :: section
      end subroutine rhyme_logger_begin_section

      module subroutine rhyme_logger_end_section(this, print_duration)
         class(logger_t), intent(inout) :: this
         logical, intent(in), optional :: print_duration
      end subroutine rhyme_logger_end_section
   end interface

contains

   subroutine rhyme_logger_init(this, str)
      implicit none

      class(logger_t), intent(inout) :: this
      character(len=*), intent(in) :: str

      integer :: i

      if (this%initialized) return

      call this%update_time
      call this%set_logo
      call this%set_colored_logo

      write (this%logfile, fmt="(I4,A,I0.2,A,I0.2,A,I0.2,A,I0.2,A,I0.2,A,A)") &
         this%t(1), '-', this%t(2), '-', this%t(3), '-', this%t(5), '-', &
         this%t(6), '-', this%t(7), '-'//trim(.filename.str), '.log.txt'

      write (this%errfile, fmt="(I4,A,I0.2,A,I0.2,A,I0.2,A,I0.2,A,I0.2,A,A)") &
         this%t(1), '-', this%t(2), '-', this%t(3), '-', this%t(5), '-', &
         this%t(6), '-', this%t(7), '-'//trim(.filename.str), '.err.txt'

      ! Logo
      call this%open_logfile
      call this%open_errfile

      do i = 1, size(this%logo)
         write (stdout, *) trim(this%colored_logo(i))
         write (this%logfile_unit, *) trim(this%logo(i))
         write (this%errfile_unit, *) trim(this%logo(i))
      end do

      call this%close_logfile
      call this%close_errfile

      this%secid = 0

      this%initialized = .true.
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
