logical function rhyme_mh_workspace_test () result ( failed )
  use rhyme_mh_workspace_factory

  implicit none

  type ( mh_workspace_t ) :: ws


  failed = &
  wsid%sl .ne. 1 &
  .or. wsid%lsides .ne. 2 &
  .or. wsid%rsides .ne. 3 &
  .or. wsid%fluxes .ne. 4 &
  .or. wsid%memory_intensive .ne. 10 &
  .or. wsid%cpu_intensive .ne. 11

  if ( failed ) return

  failed = &
  ws%initialized &
  .or. ws%type .ne. wsid%memory_intensive &
  .or. lbound ( ws%levels, 1 ) .ne. 0 &
  .or. ubound ( ws%levels, 1 ) .ne. samrid%max_nlevels
end function rhyme_mh_workspace_test
