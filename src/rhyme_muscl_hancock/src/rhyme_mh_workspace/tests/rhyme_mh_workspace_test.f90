logical function rhyme_mh_workspace_test () result ( failed )
  use rhyme_mh_workspace_factory

  implicit none

  type ( mh_workspace_t ) :: mhws

  failed = &
  mhws%type .ne. mhwsid%memory_intensive &
  .or. lbound ( mhws%levels, 1 ) .ne. 0 &
  .or. ubound ( mhws%levels, 1 ) .ne. samrid%max_nlevels
end function rhyme_mh_workspace_test
