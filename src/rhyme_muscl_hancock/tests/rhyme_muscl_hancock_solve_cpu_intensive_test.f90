logical function rhyme_muscl_hancock_solve_cpu_intensive_test () result ( failed )
  use rhyme_muscl_hancock_advection_factory
  use rhyme_assertion

  implicit none

  type ( assertion_t ) :: mh_tester

  type ( mh_workspace_t ) :: ws_x, ws_y, ws_z

  mh_tester = .describe. "mh_solve_cpu_intensive"

  ws_x%type = mhwsid%cpu_intensive
  ws_y%type = mhwsid%cpu_intensive
  ws_z%type = mhwsid%cpu_intensive

  call rhyme_muscl_hancock_advection_test( &
    rhyme_muscl_hancock_solve_cpu_intensive, ws_x, hyid%x, mh_tester )
  call rhyme_muscl_hancock_advection_test( &
    rhyme_muscl_hancock_solve_cpu_intensive, ws_y, hyid%y, mh_tester )
  call rhyme_muscl_hancock_advection_test( &
    rhyme_muscl_hancock_solve_cpu_intensive, ws_z, hyid%z, mh_tester )

  failed = mh_tester%failed()
end function rhyme_muscl_hancock_solve_cpu_intensive_test
