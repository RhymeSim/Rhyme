logical function rhyme_param_parser_add_switch_test () result ( failed )
  use rhyme_param_parser
  use rhyme_assertion

  implicit none

  type ( assertion_t ) :: pp_tester

  character ( len=8 ), parameter :: key = 'key'
  integer, parameter :: val = 1

  type ( config_switch_t ) :: switch

  pp_tester = .describe. "rhyme_param_parser_add_switch"

  call pp_tester%expect( switch%len .toBe. 0 )

  call switch%add( key, val )

  call pp_tester%expect( switch%len .toBe. 1 )
  call pp_tester%expect( switch%keys(1) .toBe. key )
  call pp_tester%expect( switch%values(1) .toBe. val )

  failed = pp_tester%failed()
end function rhyme_param_parser_add_switch_test
