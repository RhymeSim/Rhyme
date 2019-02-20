module rhyme_log
  use, intrinsic :: iso_fortran_env, only: stdin=>input_unit, stdout=>output_unit, stderr=>error_unit

  implicit none


  type logid_t
    integer :: closed = -10
    character ( len=36 ) :: time_fmt = "(A,I4,A,I0.2,A,I0.2,A,I0.2,A,I0.2,A)"
  end type logid_t

  type ( logid_t ), parameter :: logid = logid_t ()


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


  type log_t
    character ( len=1024 ) :: logfile = "./log.txt"
    character ( len=1024 ) :: errfile = "./err.txt"
    integer :: logfile_unit = logid%closed
    integer :: errfile_unit = logid%closed
    character ( len=1024 ), dimension(8) :: vivid_logo
    character ( len=1024 ), dimension(8) :: logo
    integer :: t(8)
  contains
    procedure :: init => rhyme_log_init
    procedure :: update_time => rhyme_log_update_time
    procedure :: current_time => rhyme_log_current_time
    procedure :: open_logfile => rhyme_log_open_logfile
    procedure :: close_logfile => rhyme_log_close_logfile
    procedure :: open_errfile => rhyme_log_open_errfile
    procedure :: close_errfile => rhyme_log_close_errfile
    procedure :: set_logo => rhyme_log_set_logo
    procedure :: set_vivid_logo => rhyme_log_set_vivid_logo
  end type log_t

contains

  subroutine rhyme_log_update_time ( this )
    implicit none

    class ( log_t ), intent ( inout ) :: this

    call date_and_time ( values=this%t )
  end subroutine rhyme_log_update_time


  character ( len=22 ) function rhyme_log_current_time ( this ) result ( time_str )
    implicit none

    class ( log_t ), intent ( inout ) :: this

    call this%update_time

    write ( time_str, fmt=logid%time_fmt ) &
    '[ ',this%t(1),'-',this%t(2),'-',this%t(3),' | ',this%t(5),':',this%t(6),' ]'
  end function rhyme_log_current_time


  subroutine rhyme_log_init ( this )
    implicit none

    class ( log_t ), intent ( inout ) :: this

    integer :: i

    call this%set_logo
    call this%set_vivid_logo


    do i = 1, size( this%vivid_logo )
    end do

    call this%open_logfile
    call this%open_errfile

    do i = 1, size( this%logo )
      write ( stdout,* ) trim( this%vivid_logo(i) )
      write ( this%logfile_unit,* ) trim( this%logo(i) )
      write ( this%errfile_unit,* ) trim( this%logo(i) )
    end do

    call this%close_logfile
    call this%close_errfile
  end subroutine rhyme_log_init


  subroutine rhyme_log_open_logfile ( this)
    implicit none

    class ( log_t ), intent ( inout ) :: this

    logical :: opened = .false.

    inquire ( file=this%logfile, number=this%logfile_unit, opened=opened )

    if ( .not. opened ) then
      open ( newunit=this%logfile_unit, file=this%logfile, encoding='utf-8', position='append' )
      inquire ( file=this%logfile, number=this%logfile_unit )
    end if
  end subroutine rhyme_log_open_logfile


  subroutine rhyme_log_close_logfile ( this)
    implicit none

    class ( log_t ), intent ( inout ) :: this

    logical :: opened = .false.

    inquire ( file=this%logfile, number=this%logfile_unit, opened=opened )

    if ( opened ) close ( this%logfile_unit )
  end subroutine rhyme_log_close_logfile


  subroutine rhyme_log_open_errfile ( this)
    implicit none

    class ( log_t ), intent ( inout ) :: this

    logical :: opened = .false.

    inquire ( file=this%errfile, number=this%errfile_unit, opened=opened )

    if ( .not. opened ) then
      open ( newunit=this%errfile_unit, file=this%errfile, encoding='utf-8', position='append' )
      inquire ( file=this%errfile, number=this%errfile_unit )
    end if
  end subroutine rhyme_log_open_errfile


  subroutine rhyme_log_close_errfile ( this)
    implicit none

    class ( log_t ), intent ( inout ) :: this

    logical :: opened = .false.

    inquire ( file=this%errfile, number=this%errfile_unit, opened=opened )

    if ( opened ) close ( this%errfile_unit )
  end subroutine rhyme_log_close_errfile



  subroutine rhyme_log_set_vivid_logo ( this )
    implicit none

    class ( log_t ), intent ( inout ) :: this

    this%vivid_logo(1) = tc%rd//"▄"//tc%yl//"▄▄"//tc%gn//"▄▄"//tc%bl//"▄"//tc%vt//             "    ▄"//tc%rd//"▄"//tc%nc
    this%vivid_logo(2) = tc%yl//"█"//tc%gn//"█▀"//tc%bl//"▀▀"//tc%ig//"▀█"//tc%vt//"█"//tc%rd//  "  █"//tc%yl//"█"//tc%nc
    this%vivid_logo(3) = tc%gn//"█"//tc%bl//"█"//tc%vt//           "    █"//tc%rd//"█"//tc%yl//  "  █"//tc%gn//"█▄"//tc%bl//"██"//tc%ig//"██"//tc%vt//"▄"//tc%rd//"  ▀"//tc%yl//"██"//tc%bl//           "  ██"//tc%ig//"█"//tc%vt//"  █"//tc%rd//"██"//tc%yl//"█▄"//tc%gn//"██"//tc%bl//"▄"//tc%vt//           "   ▄█"//tc%rd//"██"//tc%yl//"█▄"//tc%nc
    this%vivid_logo(4) = tc%bl//"█"//tc%ig//"██"//tc%vt//"██"//tc%rd//"██"//tc%gn//             "   █"//tc%bl//"█▀"//tc%vt//           "   █"//tc%rd//"█"//tc%gn//           "   ██"//tc%bl//"▄"//tc%ig//" ██"//tc%rd//           "   █"//tc%yl//"█"//tc%gn//" ██"//tc%bl//" █"//tc%ig//"█"//tc%vt//"  █"//tc%rd//"█▄"//tc%yl//"▄▄"//tc%gn//"▄█"//tc%bl//"█"//tc%nc
    this%vivid_logo(5) = tc%ig//"█"//tc%vt//"█"//tc%rd//"  ▀"//tc%yl//"██"//tc%gn//"▄"//tc%bl//  "  █"//tc%ig//"█"//tc%rd//           "    █"//tc%yl//"█"//tc%bl//           "    █"//tc%ig//"██"//tc%vt//"█▀"//tc%yl//           "   █"//tc%gn//"█"//tc%bl//" ██"//tc%ig//" █"//tc%vt//"█"//tc%rd//"  █"//tc%yl//"█▀"//tc%gn//"▀▀"//tc%bl//"▀▀"//tc%ig//"▀"//tc%nc
    this%vivid_logo(6) = tc%vt//"█"//tc%rd//"█"//tc%gn//           "    █"//tc%bl//"█"//tc%ig//  "  █"//tc%vt//"█"//tc%yl//           "    █"//tc%gn//"█"//tc%vt//                      "     ██"//tc%rd//"█"//tc%gn//           "    █"//tc%bl//"█"//tc%ig//" ██"//tc%vt//" █"//tc%rd//"█"//tc%yl//"  ▀"//tc%gn//"██"//tc%bl//"▄▄"//tc%ig//"▄▄"//tc%vt//"█"//tc%nc
    this%vivid_logo(7) = tc%rd//"▀"//tc%yl//"▀"//tc%bl//           "    ▀"//tc%ig//"▀▀"//tc%vt//  " ▀"//tc%rd//"▀"//tc%gn//           "    ▀"//tc%bl//"▀"//tc%rd//                      "     ██"//tc%bl//                      "     ▀"//tc%ig//"▀"//tc%vt//" ▀▀"//tc%rd//" ▀"//tc%yl//"▀"//tc%bl//           "    ▀"//tc%ig//"▀▀"//tc%vt//"▀▀"//tc%nc
    this%vivid_logo(8) = tc%rd//                                                                                                                          "                      ██"//tc%yl//"█"//tc%nc//"  © Saeed Sarpas, 2019"
  end subroutine rhyme_log_set_vivid_logo


  subroutine rhyme_log_set_logo ( this )
    implicit none

    class ( log_t ), intent ( inout ) :: this

    this%logo(1) = "▄▄▄▄▄▄    ▄▄"
    this%logo(2) = "██▀▀▀▀██  ██"
    this%logo(3) = "██    ██  ██▄████▄  ▀██  ███  ████▄██▄   ▄████▄"
    this%logo(4) = "███████   ██▀   ██   ██▄ ██   ██ ██ ██  ██▄▄▄▄██"
    this%logo(5) = "██  ▀██▄  ██    ██    ████▀   ██ ██ ██  ██▀▀▀▀▀▀"
    this%logo(6) = "██    ██  ██    ██     ███    ██ ██ ██  ▀██▄▄▄▄█"
    this%logo(7) = "▀▀    ▀▀▀ ▀▀    ▀▀     ██     ▀▀ ▀▀ ▀▀    ▀▀▀▀▀"
    this%logo(8) = "                      ▀▀▀  © Saeed Sarpas, 2019"
  end subroutine rhyme_log_set_logo

end module rhyme_log
