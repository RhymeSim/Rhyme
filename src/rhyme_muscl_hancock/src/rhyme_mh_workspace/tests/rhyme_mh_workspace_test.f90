logical function rhyme_mh_workspace_test () result ( failed )
  use rhyme_mh_workspace_factory

  implicit none

  type ( mh_workspace_t ) :: ws


  failed = &
  ws%initialized &
  .or. lbound ( ws%levels, 1 ) .ne. 0 &
  .or. ubound ( ws%levels, 1 ) .ne. samrid%max_nlevels
end function rhyme_mh_workspace_test
