module rhyme_param_parser
  use rhyme_samr

  implicit none

contains

  logical function parse_params ( param_file, amr ) result ( passed )
    implicit none

    character (len=1024), intent(in) :: param_file
    type (samr_t) :: amr

    integer :: ios
    character(len=1024) :: key, op


    open (1, file=param_file, action='read', form="formatted")

    do
      read (1, *, iostat=ios) key
      if ( ios .ne. 0 ) exit

      key = adjustl(trim(key))
      if ( key(1:1) .eq. "#" ) cycle
      backspace (1)

      select case ( adjustl(trim(key)) )
      case ( "base_grid" )
        read (1, *) key, op, amr%base_grid(1:3)
      end select
    end do

    close (1)

    passed = .true.
  end function parse_params
end module rhyme_param_parser
