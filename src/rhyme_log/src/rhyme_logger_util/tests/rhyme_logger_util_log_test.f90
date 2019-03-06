logical function rhyme_logger_util_log_test () result ( failed )
  use rhyme_logger_util

  implicit none

  type ( logger_util_t ) :: log

  call log%start
  call log%set_section( 'test' )
  call log%log( 'message' )
  call log%log( 'message', 'key' )
  call log%log( 'message', 123 )
  call log%log( 'message', 1.2e3 )
  call log%log( 'message', 1.2d3 )
  call log%log( 'message', 'key', 'op' )
  call log%log( 'message', 'key', 'op', [ 'value' ] )
  call log%log( 'message', 'key', 'op', [ 123 ] )
  call log%log( 'message', 'key', 'op', [ 1.2e3 ] )
  call log%log( 'message', 'key', 'op', [ 1.2d3 ] )
  call log%log( 'message', 'key', 'op', [ 'value 1', 'value 2' ] )
  call log%log( 'message', 'key', 'op', [ 123, 234 ] )
  call log%log( 'message', 'key', 'op', [ 1.2e3, 2.3e4 ] )
  call log%log( 'message', 'key', 'op', [ 1.2d3, 2.3d4 ] )

  ! To see the output set failed to .true.
  failed = .false.
end function rhyme_logger_util_log_test
