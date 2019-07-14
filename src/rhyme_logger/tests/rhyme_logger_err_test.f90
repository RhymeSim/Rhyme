logical function rhyme_logger_err_test () result ( failed )
  use rhyme_logger

  implicit none

  type ( logger_t ) :: logger

  call logger%init( 'err' )
  call logger%begin_section( 'test' )
  call logger%err( 'message' )
  call logger%err( 'message', 'key' )
  call logger%err( 'message', 123 )
  call logger%err( 'message', 1.2e3 )
  call logger%err( 'message', 1.2d3 )
  call logger%err( 'message', 'key', 'op' )
  call logger%err( 'message', 'key', 'op', [ 'value' ] )
  call logger%err( 'message', 'key', 'op', [ 123 ] )
  call logger%err( 'message', 'key', 'op', [ 1.2e3 ] )
  call logger%err( 'message', 'key', 'op', [ 1.2d3 ] )
  call logger%err( 'message', 'key', 'op', [ 'value 1', 'value 2' ] )
  call logger%err( 'message', 'key', 'op', [ 123, 234 ] )
  call logger%err( 'message', 'key', 'op', [ 1.2e3, 2.3e4 ] )
  call logger%err( 'message', 'key', 'op', [ 1.2d3, 2.3d4 ] )
  call logger%end_section

  ! To see the output set failed to .true.
  failed = .false.
end function rhyme_logger_err_test
