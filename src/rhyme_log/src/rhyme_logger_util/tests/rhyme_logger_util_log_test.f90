logical function rhyme_logger_util_log_test () result ( failed )
  use rhyme_logger_util

  implicit none

  type ( logger_util_t ) :: log

  call log%init( 'log' )
  call log%begin_section( 'test' )
  call log%log( 'message' )
  call log%log( 'message', 'key' )
  call log%log( 'message', 123 )
  call log%log( 'message', 1.2e3 )
  call log%log( 'message', 1.2d3 )
  call log%begin_section( 'scalar' )
  call log%log( 'message', 'key', 'op' )
  call log%log( 'message', 'key', 'op', [ 'value' ] )
  call log%log( 'message', 'key', 'op', [ 123 ] )
  call log%log( 'message', 'key', 'op', [ 1.2e3 ] )
  call log%log( 'message', 'key', 'op', [ 1.2d3 ] )
  call log%begin_section( 'array' )
  call log%log( 'message', 'key', 'op', [ 'value 1', 'value 2' ] )
  call log%log( 'message', 'key', 'op', [ 123, 234 ] )
  call log%log( 'message', 'key', 'op', [ 1.2e3, 2.3e4 ] )
  call log%log( 'message', 'key', 'op', [ 1.2d3, 2.3d4 ] )
  call log%begin_section( '' )
  call log%log( 'done' )
  call log%end_section
  call log%end_section
  call log%end_section
  call log%end_section

  ! To see the output set failed to .true.
  failed = .false.
end function rhyme_logger_util_log_test
