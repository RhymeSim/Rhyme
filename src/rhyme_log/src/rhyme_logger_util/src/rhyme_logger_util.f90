module rhyme_logger_util
  use, intrinsic :: iso_fortran_env, only: stdin=>input_unit, stdout=>output_unit, stderr=>error_unit

  implicit none


  type logger_util_const_t
    integer :: closed = -10
    character ( len=45 ) :: time_fmt = "(A,I4,A,I0.2,A,I0.2,A,I0.2,A,I0.2,A,I0.2,A)"
    character ( len=6 ) :: int_fmt = "(I512)"
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
    character ( len=1024 ), dimension(10) :: vivid_logo = ""
    character ( len=1024 ), dimension(10) :: logo = ""
    character ( len=64 ) :: sec = ""
    integer :: logfile_unit = logger_util_const%closed
    integer :: errfile_unit = logger_util_const%closed
    integer :: t(8)
  contains
    procedure :: init => rhyme_logger_util_init
    procedure :: set_section => rhyme_logger_util_set_section
    procedure :: write => rhyme_logger_util_write
    procedure :: write_kw => rhyme_logger_util_write_kw
    procedure :: write_kw1d => rhyme_logger_util_write_kw1d
    procedure :: update_time => rhyme_logger_util_update_time
    procedure :: time => rhyme_logger_util_time
    procedure :: open_logfile => rhyme_logger_util_open_logfile
    procedure :: close_logfile => rhyme_logger_util_close_logfile
    procedure :: open_errfile => rhyme_logger_util_open_errfile
    procedure :: close_errfile => rhyme_logger_util_close_errfile
    procedure :: set_logo => rhyme_logger_util_set_logo
    procedure :: set_vivid_logo => rhyme_logger_util_set_vivid_logo
  end type logger_util_t

contains
  subroutine rhyme_logger_util_init ( this )
    implicit none

    class ( logger_util_t ), intent ( inout ) :: this

    integer :: i

    if ( this%initialized ) return

    call this%set_logo
    call this%set_vivid_logo

    if ( len_trim(this%logfile) < 1 ) this%logfile = logger_util_const%default_logfile
    if ( len_trim(this%errfile) < 1 ) this%errfile = logger_util_const%default_errfile

    ! Logo
    call this%open_logfile
    call this%open_errfile

    do i = 1, size( this%logo )
      write ( stdout,* ) trim( this%vivid_logo(i) )
      write ( this%logfile_unit,* ) trim( this%logo(i) )
      write ( this%errfile_unit,* ) trim( this%logo(i) )
    end do

    call this%close_logfile
    call this%close_errfile


    ! Start point
    call this%set_section ( 'init' )
    call this%write ( 'Start! ツ' )

    this%initialized = .true.
  end subroutine rhyme_logger_util_init


  subroutine rhyme_logger_util_set_section ( this, section )
    implicit none

    class ( logger_util_t ), intent ( inout ) :: this
    character ( len=* ), intent ( in ) :: section

    call this%open_logfile
    write ( stdout,* ) ""
    write ( this%logfile_unit,* ) ""
    call this%close_logfile

    this%sec = trim(section)
  end subroutine rhyme_logger_util_set_section


  subroutine rhyme_logger_util_write ( this, info )
    implicit none

    class ( logger_util_t ), intent ( inout ) :: this
    class (*), intent ( in ) :: info

    character ( len=512 ) :: i

    select type ( inf => info )
    type is ( character(*) )
      i = inf
    type is ( integer )
      write ( i, logger_util_const%int_fmt ) inf
    type is ( real( kind=4 ) )
      write ( i, logger_util_const%real_fmt ) inf
    type is ( real( kind=8 ) )
      write ( i, logger_util_const%real8_fmt ) inf
    end select

    i = adjustl( i )

    call this%open_logfile

    write ( stdout,* ) this%time(color=.true.)//' '//trim(this%sec)//': '//trim(i)
    write ( this%logfile_unit,* ) trim(this%time(color=.false.))//' '//trim(this%sec)//': '//trim(i)

    call this%close_logfile
  end subroutine rhyme_logger_util_write


  subroutine rhyme_logger_util_write_kw ( this, key, value )
    implicit none

    class ( logger_util_t ), intent ( inout ) :: this
    class (*), intent ( in ) :: key, value

    character ( len=512 ) :: k, v


    select type ( ke => key )
    type is ( character (*) )
      k = ke
    type is ( integer )
      write ( k, logger_util_const%int_fmt ) ke
    type is ( real( kind=4 ) )
      write ( k, logger_util_const%real_fmt ) ke
    type is ( real( kind=8 ) )
      write ( k, logger_util_const%real8_fmt ) ke
    end select

    select type ( val => value )
    type is ( character (*) )
      v = val
    type is ( integer )
      write ( v, logger_util_const%int_fmt ) val
    type is ( real( kind=4 ) )
      write ( v, logger_util_const%real_fmt ) val
    type is ( real( kind=8 ) )
      write ( v, logger_util_const%real8_fmt ) val
    end select

    k = adjustl( k )
    v = adjustl( v )

    call this%open_logfile

    write ( stdout,* ) this%time(color=.true.)//' '//trim(this%sec)//': '//trim(k)//tc%ig//' => '//tc%nc//trim(v)
    write ( this%logfile_unit,* ) trim(this%time(color=.false.))//' '//trim(this%sec)//': '//trim(k)//' => '//trim(v)

    call this%close_logfile
  end subroutine rhyme_logger_util_write_kw


  subroutine rhyme_logger_util_write_kw1d ( this, key, value )
    implicit none

    class ( logger_util_t ), intent ( inout ) :: this
    class (*), intent ( in ) :: key
    class (*), dimension(:), intent ( in ) :: value

    character ( len=512 ) :: k = "", v = ""
    integer :: i

    select type ( ke => key )
    type is ( character (*) )
      k = ke
    type is ( integer )
      write ( k, logger_util_const%int_fmt ) ke
    type is ( real( kind=4 ) )
      write ( k, logger_util_const%real_fmt ) ke
    type is ( real( kind=8 ) )
      write ( k, logger_util_const%real8_fmt ) ke
    end select
    k = adjustl( k )

    v = '[ '
    select type ( val => value )
    type is ( character (*) )
      do i = 1, size( val )
        write ( v, '(A,A,A,A)' ) trim(v), ' ', val(i), ' '
      end do
    type is ( integer )
      do i = 1, size( val )
        write ( v, '(A,A,I0,A)' ) trim(v), ' ', val(i), ' '
      end do
    type is ( real( kind=4 ) )
      do i = 1, size( val )
        write ( v, '(A,A,E16.5,A)' ) trim(v), ' ', val(i), ' '
      end do
    type is ( real( kind=8 ) )
      do i = 1, size( val )
        write ( v, '(A,A,E16.7,A)' ) trim(v), ' ', val(i), ' '
      end do
    end select
    v = trim(v)//' ]'

    call this%open_logfile

    write ( stdout,* ) this%time(color=.true.)//' '//trim(this%sec)//': '//trim(k)//tc%ig//' => '//tc%nc//trim(v)
    write ( this%logfile_unit,* ) trim(this%time(color=.false.))//' '//trim(this%sec)//': '//trim(k)//' => '//trim(v)

    call this%close_logfile
  end subroutine rhyme_logger_util_write_kw1d


  subroutine rhyme_logger_util_update_time ( this )
    implicit none

    class ( logger_util_t ), intent ( inout ) :: this

    call date_and_time ( values=this%t )
  end subroutine rhyme_logger_util_update_time


  character ( len=41 ) function rhyme_logger_util_time ( this, color ) result ( time_str )
    implicit none

    class ( logger_util_t ), intent ( inout ) :: this
    logical, intent ( in ) :: color

    call this%update_time

    write ( time_str, fmt=logger_util_const%time_fmt ) &
      '[ ',this%t(1),'-',this%t(2),'-',this%t(3),' | ', &
      this%t(5),':',this%t(6),':',this%t(7),' ]'

    if ( color ) time_str = tc%gn//trim(time_str)//tc%nc
  end function rhyme_logger_util_time



  subroutine rhyme_logger_util_open_logfile ( this)
    implicit none

    class ( logger_util_t ), intent ( inout ) :: this

    logical :: opened = .false.

    inquire ( file=this%logfile, number=this%logfile_unit, opened=opened )

    if ( .not. opened ) then
      open ( newunit=this%logfile_unit, file=this%logfile, encoding='utf-8', position='append' )
      inquire ( file=this%logfile, number=this%logfile_unit )
    end if
  end subroutine rhyme_logger_util_open_logfile


  subroutine rhyme_logger_util_close_logfile ( this)
    implicit none

    class ( logger_util_t ), intent ( inout ) :: this

    logical :: opened = .false.

    inquire ( file=this%logfile, number=this%logfile_unit, opened=opened )

    if ( opened ) close ( this%logfile_unit )
  end subroutine rhyme_logger_util_close_logfile


  subroutine rhyme_logger_util_open_errfile ( this)
    implicit none

    class ( logger_util_t ), intent ( inout ) :: this

    logical :: opened = .false.

    inquire ( file=this%errfile, number=this%errfile_unit, opened=opened )

    if ( .not. opened ) then
      open ( newunit=this%errfile_unit, file=this%errfile, encoding='utf-8', position='append' )
      inquire ( file=this%errfile, number=this%errfile_unit )
    end if
  end subroutine rhyme_logger_util_open_errfile


  subroutine rhyme_logger_util_close_errfile ( this)
    implicit none

    class ( logger_util_t ), intent ( inout ) :: this

    logical :: opened = .false.

    inquire ( file=this%errfile, number=this%errfile_unit, opened=opened )

    if ( opened ) close ( this%errfile_unit )
  end subroutine rhyme_logger_util_close_errfile


  subroutine rhyme_logger_util_set_vivid_logo ( this )
    implicit none

    class ( logger_util_t ), intent ( inout ) :: this

    this%vivid_logo(1) = tc%nc
    this%vivid_logo(2) = tc%rd//"▄"//tc%yl//"▄▄"//tc%gn//"▄▄"//tc%bl//"▄"//tc%vt//             "    ▄"//tc%rd//"▄"//tc%nc
    this%vivid_logo(3) = tc%yl//"█"//tc%gn//"█▀"//tc%bl//"▀▀"//tc%ig//"▀█"//tc%vt//"█"//tc%rd//  "  █"//tc%yl//"█"//tc%nc
    this%vivid_logo(4) = tc%gn//"█"//tc%bl//"█"//tc%vt//           "    █"//tc%rd//"█"//tc%yl//  "  █"//tc%gn//"█▄"//tc%bl//"██"//tc%ig//"██"//tc%vt//"▄"//tc%rd//"  ▀"//tc%yl//"██"//tc%bl//           "  ██"//tc%ig//"█"//tc%vt//"  █"//tc%rd//"██"//tc%yl//"█▄"//tc%gn//"██"//tc%bl//"▄"//tc%vt//           "   ▄█"//tc%rd//"██"//tc%yl//"█▄"//tc%nc
    this%vivid_logo(5) = tc%bl//"█"//tc%ig//"██"//tc%vt//"██"//tc%rd//"██"//tc%gn//             "   █"//tc%bl//"█▀"//tc%vt//           "   █"//tc%rd//"█"//tc%gn//           "   ██"//tc%bl//"▄"//tc%ig//" ██"//tc%rd//           "   █"//tc%yl//"█"//tc%gn//" ██"//tc%bl//" █"//tc%ig//"█"//tc%vt//"  █"//tc%rd//"█▄"//tc%yl//"▄▄"//tc%gn//"▄█"//tc%bl//"█"//tc%nc
    this%vivid_logo(6) = tc%ig//"█"//tc%vt//"█"//tc%rd//"  ▀"//tc%yl//"██"//tc%gn//"▄"//tc%bl//  "  █"//tc%ig//"█"//tc%rd//           "    █"//tc%yl//"█"//tc%bl//           "    █"//tc%ig//"██"//tc%vt//"█▀"//tc%yl//           "   █"//tc%gn//"█"//tc%bl//" ██"//tc%ig//" █"//tc%vt//"█"//tc%rd//"  █"//tc%yl//"█▀"//tc%gn//"▀▀"//tc%bl//"▀▀"//tc%ig//"▀"//tc%nc
    this%vivid_logo(7) = tc%vt//"█"//tc%rd//"█"//tc%gn//           "    █"//tc%bl//"█"//tc%ig//  "  █"//tc%vt//"█"//tc%yl//           "    █"//tc%gn//"█"//tc%vt//                      "     ██"//tc%rd//"█"//tc%gn//           "    █"//tc%bl//"█"//tc%ig//" ██"//tc%vt//" █"//tc%rd//"█"//tc%yl//"  ▀"//tc%gn//"██"//tc%bl//"▄▄"//tc%ig//"▄▄"//tc%vt//"█"//tc%nc
    this%vivid_logo(8) = tc%rd//"▀"//tc%yl//"▀"//tc%bl//           "    ▀"//tc%ig//"▀▀"//tc%vt//  " ▀"//tc%rd//"▀"//tc%gn//           "    ▀"//tc%bl//"▀"//tc%rd//                      "     ██"//tc%bl//                      "     ▀"//tc%ig//"▀"//tc%vt//" ▀▀"//tc%rd//" ▀"//tc%yl//"▀"//tc%bl//           "    ▀"//tc%ig//"▀▀"//tc%vt//"▀▀"//tc%nc
    this%vivid_logo(9) = tc%rd//                                                                                                                          "                      ██"//tc%yl//"█"//tc%nc//"  © Saeed Sarpas, 2019"//tc%nc
    this%vivid_logo(10) = tc%nc
  end subroutine rhyme_logger_util_set_vivid_logo


  subroutine rhyme_logger_util_set_logo ( this )
    implicit none

    class ( logger_util_t ), intent ( inout ) :: this

    this%logo(1) = ""
    this%logo(2) = "▄▄▄▄▄▄    ▄▄"
    this%logo(3) = "██▀▀▀▀██  ██"
    this%logo(4) = "██    ██  ██▄████▄  ▀██  ███  ████▄██▄   ▄████▄"
    this%logo(5) = "███████   ██▀   ██   ██▄ ██   ██ ██ ██  ██▄▄▄▄██"
    this%logo(6) = "██  ▀██▄  ██    ██    ████▀   ██ ██ ██  ██▀▀▀▀▀▀"
    this%logo(7) = "██    ██  ██    ██     ███    ██ ██ ██  ▀██▄▄▄▄█"
    this%logo(8) = "▀▀    ▀▀▀ ▀▀    ▀▀     ██     ▀▀ ▀▀ ▀▀    ▀▀▀▀▀"
    this%logo(9) = "                      ▀▀▀  © Saeed Sarpas, 2019"
    this%logo(10) = ""
  end subroutine rhyme_logger_util_set_logo

end module rhyme_logger_util
