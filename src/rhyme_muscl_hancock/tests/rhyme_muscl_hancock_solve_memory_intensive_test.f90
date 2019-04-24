logical function rhyme_muscl_hancock_solve_memory_intensive_test () result ( failed )
  use rhyme_muscl_hancock_advection_factory
  use rhyme_assertion

  implicit none

  type ( assertion_t ) :: mh_tester

  type ( mh_workspace_t ) :: mhws

  mh_tester = .describe. "mh_solve_memory_intensive"

  mhws%type = mhwsid%memory_intensive

  call rhyme_muscl_hancock_advection_test( &
    rhyme_muscl_hancock_solve_memory_intensive, mhws, hyid%x, mh_tester )
  call rhyme_muscl_hancock_advection_test( &
    rhyme_muscl_hancock_solve_memory_intensive, mhws, hyid%y, mh_tester )
  call rhyme_muscl_hancock_advection_test( &
    rhyme_muscl_hancock_solve_memory_intensive, mhws, hyid%z, mh_tester )

  failed = mh_tester%failed()
end function rhyme_muscl_hancock_solve_memory_intensive_test
