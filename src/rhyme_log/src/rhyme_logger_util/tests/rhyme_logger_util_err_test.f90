logical function rhyme_logger_util_err_test () result ( failed )
  use rhyme_logger_util

  implicit none

  type ( logger_util_t ) :: log

  call log%start
  call log%set_section( 'test' )
  call log%err( 'message' )
  call log%err( 'message', 'key' )
  call log%err( 'message', 123 )
  call log%err( 'message', 1.2e3 )
  call log%err( 'message', 1.2d3 )
  call log%err( 'message', 'key', 'op' )
  call log%err( 'message', 'key', 'op', [ 'value' ] )
  call log%err( 'message', 'key', 'op', [ 123 ] )
  call log%err( 'message', 'key', 'op', [ 1.2e3 ] )
  call log%err( 'message', 'key', 'op', [ 1.2d3 ] )
  call log%err( 'message', 'key', 'op', [ 'value 1', 'value 2' ] )
  call log%err( 'message', 'key', 'op', [ 123, 234 ] )
  call log%err( 'message', 'key', 'op', [ 1.2e3, 2.3e4 ] )
  call log%err( 'message', 'key', 'op', [ 1.2d3, 2.3d4 ] )

  ! To see the output set failed to .true.
  failed = .false.
end function rhyme_logger_util_err_test
