module rhyme_logger_util
  use, intrinsic :: iso_fortran_env, only: stdin=>input_unit, stdout=>output_unit, stderr=>error_unit

  implicit none


  type logger_util_const_t
    integer :: closed = -10
    character ( len=45 ) :: time_fmt = "(A,I4,A,I0.2,A,I0.2,A,I0.2,A,I0.2,A,I0.2,A)"
    character ( len=4 ) :: int_fmt = "(I0)"
    character ( len=7 ) :: real_fmt = "(E16.5)"
    character ( len=7 ) :: real8_fmt = "(E16.7)"
    character ( len=7 ) :: default_logfile = "log.txt"
    character ( len=7 ) :: default_errfile = "err.txt"
  end type logger_util_const_t

  type ( logger_util_const_t ), parameter :: logger_util_const = logger_util_const_t ()


  type terminal_colors_t
    character ( len=12 ) :: rd = achar(27)//"[0;1;31;91m" ! Red
    character ( len=12 ) :: gn = achar(27)//"[0;1;32;92m" ! Green
    character ( len=12 ) :: yl = achar(27)//"[0;1;33;93m" ! Yellow
    character ( len=12 ) :: ig = achar(27)//"[0;1;34;94m" ! Indigo
    character ( len=12 ) :: vt = achar(27)//"[0;1;35;95m" ! Violet
    character ( len=12 ) :: bl = achar(27)//"[0;1;36;96m" ! Blue
    character ( len=4 ) :: nc = achar(27)//"[0m" ! Reset
  end type terminal_colors_t

  type ( terminal_colors_t ), parameter :: tc = terminal_colors_t ()


  type logger_util_t
    logical :: initialized = .false.
    character ( len=1024 ) :: logfile = logger_util_const%default_logfile
    character ( len=1024 ) :: errfile = logger_util_const%default_errfile
    character ( len=1024 ), dimension(10) :: colored_logo = ""
    character ( len=1024 ), dimension(10) :: logo = ""
    character ( len=64 ) :: sec = ""
    integer :: logfile_unit = logger_util_const%closed
    integer :: errfile_unit = logger_util_const%closed
    integer :: t(8)
  contains
    procedure :: start => rhyme_logger_util_start
    procedure :: set_section => rhyme_logger_util_set_section

    procedure :: log => rhyme_logger_util_log
    procedure :: warn => rhyme_logger_util_warn
    procedure :: err => rhyme_logger_util_err

    procedure :: done => rhyme_logger_util_done

    procedure :: update_time => rhyme_logger_util_update_time
    procedure :: time => rhyme_logger_util_time
    procedure :: time_and_section => rhyme_logger_util_time_and_section
    procedure :: tas => rhyme_logger_util_time_and_section

    procedure :: set_logo => rhyme_logger_util_set_logo
    procedure :: set_colored_logo => rhyme_logger_util_set_colored_logo

    procedure :: open_logfile => rhyme_logger_util_open_logfile
    procedure :: close_logfile => rhyme_logger_util_close_logfile
    procedure :: open_errfile => rhyme_logger_util_open_errfile
    procedure :: close_errfile => rhyme_logger_util_close_errfile
  end type logger_util_t

  interface
    module subroutine rhyme_logger_util_log ( this, message, key, operator, value )
      class ( logger_util_t ), intent ( inout ) :: this
      character ( len=* ), intent ( in ) :: message
      class (*), intent ( in ), optional :: key
      character ( len=* ), intent ( in ), optional :: operator
      class (*), intent ( in ), optional :: value(:)
    end subroutine rhyme_logger_util_log

    module subroutine rhyme_logger_util_warn ( this, message, key, operator, value )
      class ( logger_util_t ), intent ( inout ) :: this
      character ( len=* ), intent ( in ) :: message
      class (*), intent ( in ), optional :: key
      character ( len=* ), intent ( in ), optional :: operator
      class (*), intent ( in ), optional :: value(:)
    end subroutine rhyme_logger_util_warn

    module subroutine rhyme_logger_util_err ( this, message, key, operator, value )
      class ( logger_util_t ), intent ( inout ) :: this
      character ( len=* ), intent ( in ) :: message
      class (*), intent ( in ), optional :: key
      character ( len=* ), intent ( in ), optional :: operator
      class (*), intent ( in ), optional :: value(:)
    end subroutine rhyme_logger_util_err

    module subroutine rhyme_logger_util_done ( this, msg )
      class ( logger_util_t ), intent ( inout ) :: this
      character ( len=* ), intent ( in ) :: msg
    end subroutine rhyme_logger_util_done

    module subroutine rhyme_logger_util_update_time ( this )
      class ( logger_util_t ), intent ( inout ) :: this
    end subroutine rhyme_logger_util_update_time

    module function rhyme_logger_util_time ( this, color ) result ( time_str )
      class ( logger_util_t ), intent ( inout ) :: this
      character ( len=* ), intent ( in ), optional :: color
      character ( len=64 ) :: time_str
    end function rhyme_logger_util_time

    module function rhyme_logger_util_time_and_section ( this, color ) result ( tas_str )
      class ( logger_util_t ), intent ( inout ) :: this
      character ( len=* ), intent ( in ), optional :: color
      character ( len=126 ) :: tas_str
    end function rhyme_logger_util_time_and_section

    module subroutine rhyme_logger_util_open_logfile ( this )
      class ( logger_util_t ), intent ( inout ) :: this
    end subroutine rhyme_logger_util_open_logfile

    module subroutine rhyme_logger_util_close_logfile ( this )
      class ( logger_util_t ), intent ( inout ) :: this
    end subroutine rhyme_logger_util_close_logfile

    module subroutine rhyme_logger_util_open_errfile ( this )
      class ( logger_util_t ), intent ( inout ) :: this
    end subroutine rhyme_logger_util_open_errfile

    module subroutine rhyme_logger_util_close_errfile ( this )
      class ( logger_util_t ), intent ( inout ) :: this
    end subroutine rhyme_logger_util_close_errfile

    module subroutine rhyme_logger_util_set_colored_logo ( this )
      class ( logger_util_t ), intent ( inout ) :: this
    end subroutine rhyme_logger_util_set_colored_logo

    module subroutine rhyme_logger_util_set_logo ( this )
      class ( logger_util_t ), intent ( inout ) :: this
    end subroutine rhyme_logger_util_set_logo
  end interface

contains

  subroutine rhyme_logger_util_start ( this )
    implicit none

    class ( logger_util_t ), intent ( inout ) :: this

    integer :: i

    if ( this%initialized ) return

    call this%update_time
    call this%set_logo
    call this%set_colored_logo

    write ( this%logfile, fmt="(I4,A,I0.2,A,I0.2,A,I0.2,A,I0.2,A,I0.2,A)" ) &
      this%t(1),'-',this%t(2),'-',this%t(3),'-', this%t(5),'-',this%t(6),'-',this%t(7),'.log.txt'

    write ( this%errfile, fmt="(I4,A,I0.2,A,I0.2,A,I0.2,A,I0.2,A,I0.2,A)" ) &
      this%t(1),'-',this%t(2),'-',this%t(3),'-', this%t(5),'-',this%t(6),'-',this%t(7),'.err.txt'

    ! Logo
    call this%open_logfile
    call this%open_errfile

    do i = 1, size( this%logo )
      write ( stdout,* ) trim( this%colored_logo(i) )
      write ( this%logfile_unit,* ) trim( this%logo(i) )
      write ( this%errfile_unit,* ) trim( this%logo(i) )
    end do

    call this%close_logfile
    call this%close_errfile


    ! Start point
    call this%set_section ( 'ツ' )
    call this%log ( 'Start!' )

    this%initialized = .true.
  end subroutine rhyme_logger_util_start


  subroutine rhyme_logger_util_set_section ( this, section )
    implicit none

    class ( logger_util_t ), intent ( inout ) :: this
    character ( len=* ), intent ( in ) :: section

    call this%open_logfile
    write ( stdout,* ) ""
    write ( this%logfile_unit,* ) ""
    call this%close_logfile

    this%sec = trim( section )

    call this%update_time
  end subroutine rhyme_logger_util_set_section


  function to_string ( input ) result ( str )
    implicit none

    class (*), intent ( in ) :: input
    character ( len=128 ) :: str

    str = ''

    select type ( inp => input )
    type is ( integer )
      write ( str, logger_util_const%int_fmt ) inp
    type is ( real( kind=4 ) )
      write ( str, logger_util_const%real_fmt ) inp
    type is ( real( kind=8 ) )
      write ( str, logger_util_const%real8_fmt ) inp
    type is ( character (*) )
      str = trim( inp )
    end select

    str = adjustl( str )
  end function to_string


  function array_to_string ( input ) result ( str )
    implicit none

    class (*), intent ( in ) :: input(:)
    character ( len=128 ) :: str

    integer :: i, length
    character ( len=64 ) :: inp_str

    length = size( input )
    str = ''

    select type ( inp => input )
    type is ( integer )
      do i = 1, length
        write ( inp_str, logger_util_const%int_fmt ) inp(i)
        str = trim( str )//'  '//trim(adjustl( inp_str ) )
      end do
    type is ( real( kind=4 ) )
      do i = 1, length
        write ( inp_str, logger_util_const%real_fmt ) inp(i)
        str = trim( str )//'  '//trim(adjustl( inp_str ) )
      end do
    type is ( real( kind=8 ) )
      do i = 1, length
        write ( inp_str, logger_util_const%real8_fmt ) inp(i)
        str = trim( str )//'  '//trim(adjustl( inp_str ) )
      end do
    type is ( character (*) )
      do i = 1, length
        str = trim( str )//'  '//trim(adjustl( inp(i) ) )
      end do
    end select

    if ( length > 1 ) then
      str = '[ '//trim(adjustl( str ) )//' ]'
    else
      str = adjustl( str )
    end if
  end function array_to_string


  function concat_components ( msg, key, op, val, color ) result ( str )
    implicit none

    character ( len=* ), intent ( in ) :: msg, key, op, val
    character ( len=* ), intent ( in ), optional :: color
    character ( len=128 ) :: str

    character ( len=16 ) :: clr

    if ( present( color ) ) then
      clr = color
    else
      clr = ''
    end if

    str = trim(adjustl( msg ))//' ' &
      //trim( adjustl( key ) )//' ' &
      //trim( adjustl( clr ) )//trim( adjustl( op ) )//tc%nc//' ' &
      //trim( adjustl( val ) )

    str = trim( adjustl( str ) )
  end function concat_components
end module rhyme_logger_util
