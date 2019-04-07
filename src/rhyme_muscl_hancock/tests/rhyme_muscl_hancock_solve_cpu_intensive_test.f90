logical function rhyme_muscl_hancock_solve_cpu_intensive_test () result ( failed )
  use rhyme_muscl_hancock_advection_factory

  implicit none

  type ( mh_workspace_t ) :: mhws

  mhws%type = mhwsid%cpu_intensive

  call rhyme_muscl_hancock_advection_test( &
    rhyme_muscl_hancock_solve_cpu_intensive, mhws, hyid%x, failed )
  call rhyme_muscl_hancock_advection_test( &
    rhyme_muscl_hancock_solve_cpu_intensive, mhws, hyid%y, failed )
  call rhyme_muscl_hancock_advection_test( &
    rhyme_muscl_hancock_solve_cpu_intensive, mhws, hyid%z, failed )

end function rhyme_muscl_hancock_solve_cpu_intensive_test