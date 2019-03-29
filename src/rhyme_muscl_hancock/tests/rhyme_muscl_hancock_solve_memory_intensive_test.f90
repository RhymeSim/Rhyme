logical function rhyme_muscl_hancock_solve_memory_intensive_test () result ( failed )
  use rhyme_muscl_hancock_advection_factory

  implicit none

  type ( mh_workspace_t ) :: mhws

  mhws%type = mhwsid%memory_intensive

  call rhyme_muscl_hancock_advection_x_test( &
    rhyme_muscl_hancock_solve_memory_intensive, mhws, failed )

end function rhyme_muscl_hancock_solve_memory_intensive_test
