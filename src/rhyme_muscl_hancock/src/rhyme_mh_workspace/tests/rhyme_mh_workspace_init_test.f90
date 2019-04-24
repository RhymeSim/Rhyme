logical function rhyme_mh_workspace_init_test () result ( failed )
  use rhyme_mh_workspace_factory
  use rhyme_assertion

  implicit none

  type ( assertion_t ) :: mhws_tester

  type ( mh_workspace_t ) :: mhws
  integer :: l

  call rhyme_mh_workspace_factory_init

  call mhws%init( samr, log )

  call mhws_tester%expect( mhws%nlevels .toBe. samr%nlevels )

  do l = 0, mhws%nlevels - 1
    call mhws_tester%expect( mhws%levels(l)%max_nboxes .toBe. samr%levels(l)%max_nboxes )
    call mhws_tester%expect( allocated( mhws%levels(l)%boxes ) .toBe. .true. )
    call mhws_tester%expect( size( mhws%levels(l)%boxes ) .toBe. samr%levels(l)%max_nboxes )
  end do

  failed = mhws_tester%failed()
end function rhyme_mh_workspace_init_test
