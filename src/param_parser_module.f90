module param_parser_module
  use rhyme_nombre
  use rhyme_samr

  implicit none

contains

  logical function parse_params (param_file, amr) result (passed)
    implicit none

    character (len=1024), intent(in) :: param_file
    type (samr_t), intent(inout) :: amr

    integer :: ios
    character(len=1024) :: str


    open (1, file=param_file, action='read', form="formatted")

    do
      read (1, iostat=ios) str
      if ( ios .ne. 0 ) exit

      if ( adjustl(str)(1:1) .eq. "#" ) cycle

      select case ( adjustl(trim(str)) )
      case ( "base_grid" ); read (1, *) str, ! amr%base_grid
      end select
    end do

    close (1)
  end function parse_params
end module param_parser_module
