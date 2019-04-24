logical function rhyme_mh_workspace_test () result ( failed )
  use rhyme_mh_workspace_factory
  use rhyme_assertion

  implicit none

  type ( assertion_t ) :: mhws_tester

  type ( mh_workspace_t ) :: mhws

  call mhws_tester%expect( mhws%type .toBe. mhwsid%memory_intensive )
  call mhws_tester%expect( lbound( mhws%levels, 1 ) .toBe. 0 )
  call mhws_tester%expect( ubound( mhws%levels, 1 ) .toBe. samrid%max_nlevels )

  failed = mhws_tester%failed()
end function rhyme_mh_workspace_test
