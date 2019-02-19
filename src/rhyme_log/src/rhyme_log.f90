module rhyme_log
  use, intrinsic :: iso_fortran_env, only: stdin=>input_unit, stdout=>output_unit, stderr=>error_unit

  implicit none

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
    character ( len=1024 ) :: logfile, errfile
  contains
    procedure :: logo => rhyme_log_logo
    procedure :: vivid_logo => rhyme_log_vivid_logo
  end type log_t

contains

  subroutine rhyme_log_vivid_logo ( this )
    implicit none

    class ( log_t ), intent ( in ) :: this

    write ( stdout,* ) tc%rd//"▄"//tc%yl//"▄▄"//tc%gn//"▄▄"//tc%bl//"▄"//tc%vt//             "    ▄"//tc%rd//"▄"//tc%nc
    write ( stdout,* ) tc%yl//"█"//tc%gn//"█▀"//tc%bl//"▀▀"//tc%ig//"▀█"//tc%vt//"█"//tc%rd//  "  █"//tc%yl//"█"//tc%nc
    write ( stdout,* ) tc%gn//"█"//tc%bl//"█"//tc%vt//           "    █"//tc%rd//"█"//tc%yl//  "  █"//tc%gn//"█▄"//tc%bl//"██"//tc%ig//"██"//tc%vt//"▄"//tc%rd//"  ▀"//tc%yl//"██"//tc%bl//           "  ██"//tc%ig//"█"//tc%vt//"  █"//tc%rd//"██"//tc%yl//"█▄"//tc%gn//"██"//tc%bl//"▄"//tc%vt//           "   ▄█"//tc%rd//"██"//tc%yl//"█▄"//tc%nc
    write ( stdout,* ) tc%bl//"█"//tc%ig//"██"//tc%vt//"██"//tc%rd//"██"//tc%gn//             "   █"//tc%bl//"█▀"//tc%vt//           "   █"//tc%rd//"█"//tc%gn//           "   ██"//tc%bl//"▄"//tc%ig//" ██"//tc%rd//           "   █"//tc%yl//"█"//tc%gn//" ██"//tc%bl//" █"//tc%ig//"█"//tc%vt//"  █"//tc%rd//"█▄"//tc%yl//"▄▄"//tc%gn//"▄█"//tc%bl//"█"//tc%nc
    write ( stdout,* ) tc%ig//"█"//tc%vt//"█"//tc%rd//"  ▀"//tc%yl//"██"//tc%gn//"▄"//tc%bl//  "  █"//tc%ig//"█"//tc%rd//           "    █"//tc%yl//"█"//tc%bl//           "    █"//tc%ig//"██"//tc%vt//"█▀"//tc%yl//           "   █"//tc%gn//"█"//tc%bl//" ██"//tc%ig//" █"//tc%vt//"█"//tc%rd//"  █"//tc%yl//"█▀"//tc%gn//"▀▀"//tc%bl//"▀▀"//tc%ig//"▀"//tc%nc
    write ( stdout,* ) tc%vt//"█"//tc%rd//"█"//tc%gn//           "    █"//tc%bl//"█"//tc%ig//  "  █"//tc%vt//"█"//tc%yl//           "    █"//tc%gn//"█"//tc%vt//                      "     ██"//tc%rd//"█"//tc%gn//           "    █"//tc%bl//"█"//tc%ig//" ██"//tc%vt//" █"//tc%rd//"█"//tc%yl//"  ▀"//tc%gn//"██"//tc%bl//"▄▄"//tc%ig//"▄▄"//tc%vt//"█"//tc%nc
    write ( stdout,* ) tc%rd//"▀"//tc%yl//"▀"//tc%bl//           "    ▀"//tc%ig//"▀▀"//tc%vt//  " ▀"//tc%rd//"▀"//tc%gn//           "    ▀"//tc%bl//"▀"//tc%rd//                      "     ██"//tc%bl//                      "     ▀"//tc%ig//"▀"//tc%vt//" ▀▀"//tc%rd//" ▀"//tc%yl//"▀"//tc%bl//           "    ▀"//tc%ig//"▀▀"//tc%vt//"▀▀"//tc%nc
    write ( stdout,* ) tc%rd//                                                                                                                          "                      ██"//tc%yl//"█"//tc%nc
  end subroutine rhyme_log_vivid_logo

  subroutine rhyme_log_logo ( this )
    implicit none

    class ( log_t ), intent ( in ) :: this

    ! Writing banner
    write ( stdout,* ) "▄▄▄▄▄▄    ▄▄"
    write ( stdout,* ) "██▀▀▀▀██  ██"
    write ( stdout,* ) "██    ██  ██▄████▄  ▀██  ███  ████▄██▄   ▄████▄"
    write ( stdout,* ) "███████   ██▀   ██   ██▄ ██   ██ ██ ██  ██▄▄▄▄██"
    write ( stdout,* ) "██  ▀██▄  ██    ██    ████▀   ██ ██ ██  ██▀▀▀▀▀▀"
    write ( stdout,* ) "██    ██  ██    ██     ███    ██ ██ ██  ▀██▄▄▄▄█"
    write ( stdout,* ) "▀▀    ▀▀▀ ▀▀    ▀▀     ██     ▀▀ ▀▀ ▀▀    ▀▀▀▀▀"
    write ( stdout,* )"                      ▀▀▀"
  end subroutine rhyme_log_logo

end module rhyme_log
