logical function rhyme_param_parser_init_test() result ( failed )
  use rhyme_param_parser
  use rhyme_assertion

  implicit none

  type ( assertion_t ) :: pp_tester

  character ( len=32 ), parameter :: path = "/path/to/param/file"
  character ( len=32 ), parameter :: section = "section"

  type ( config_t ) :: cfg
  type ( log_t ) :: logger

  pp_tester = .describe. "rhyme_param_parser_init"

  logger%sec = section

  call cfg%init( path, logger )

  call pp_tester%expect( cfg%path .toBe. path )
  call pp_tester%expect( cfg%logger%sec .toBe. section )

  failed = pp_tester%failed()
end function rhyme_param_parser_init_test
