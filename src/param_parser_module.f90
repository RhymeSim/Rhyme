module param_parser_module
  use rhyme_nombre

  implicit none

contains

  logical function parse_params (param_file) result (passed)
    implicit none

    character(len=1024), intent(in) :: param_file


    open (1, file=param_file, action='read', form="formatted")

    do
    end do

    close (1)
  end function parse_params
end module param_parser_module
