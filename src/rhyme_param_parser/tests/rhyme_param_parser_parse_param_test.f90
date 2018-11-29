logical function rhyme_param_parser_parse_param_test () result ( failed )
  use rhyme_param_parser

  implicit none

  type ( samr_t ) :: samr
  character(len=1024), parameter :: param_file = "parameters.conf.example"

  failed = .not. parse_params ( param_file, samr )
  if ( failed ) return

  failed = &
  any ( samr%base_grid .ne. [ 32, 32, 1 ] ) &
  .or. any ( samr%ghost_cells .ne. [ 2, 2, 0 ] ) &
  .or. samr%nlevels .ne. 1 &
  .or. samr%nboxes .ne. 1

end function rhyme_param_parser_parse_param_test
