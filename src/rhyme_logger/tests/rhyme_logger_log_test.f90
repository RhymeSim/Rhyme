logical function rhyme_logger_log_test () result ( failed )
  use rhyme_logger

  implicit none

  type ( logger_t ) :: logger

  call logger%init( 'log' )
  call logger%begin_section( 'test' )
  call logger%log( 'message' )
  call logger%log( 'message', 'key' )
  call logger%log( 'message', 123 )
  call logger%log( 'message', 1.2e3 )
  call logger%log( 'message', 1.2d3 )
  call logger%begin_section( 'scalar' )
  call logger%log( 'message', 'key', 'op' )
  call logger%log( 'message', 'key', 'op', [ 'value' ] )
  call logger%log( 'message', 'key', 'op', [ 123 ] )
  call logger%log( 'message', 'key', 'op', [ 1.2e3 ] )
  call logger%log( 'message', 'key', 'op', [ 1.2d3 ] )
  call logger%begin_section( 'array' )
  call logger%log( 'message', 'key', 'op', [ 'value 1', 'value 2' ] )
  call logger%log( 'message', 'key', 'op', [ 123, 234 ] )
  call logger%log( 'message', 'key', 'op', [ 1.2e3, 2.3e4 ] )
  call logger%log( 'message', 'key', 'op', [ 1.2d3, 2.3d4 ] )
  call logger%begin_section( '' )
  call logger%log( 'done' )
  call logger%end_section
  call logger%end_section
  call logger%end_section
  call logger%end_section

  ! To see the output set failed to .true.
  failed = .false.
end function rhyme_logger_log_test
