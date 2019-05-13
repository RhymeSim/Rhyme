logical function rhyme_muscl_hancock_init_test () result ( failed )
  use rhyme_muscl_hancock_factory
  use rhyme_samr_factory
  use rhyme_assertion

  implicit none

  type ( assertion_t ) :: mh_tester

  type ( muscl_hancock_t ) :: mh
  type ( mh_workspace_t ) :: mhws
  type ( samr_t ) :: samr

  mh_tester = .describe. "mh_init"

  samr = samr_factory%generate()
  call rhyme_muscl_hancock_factory_init

  call mh_tester%expect( mh%solver_type .toBe. mhid%memory_intensive )
  call mh_tester%expect( mh%active_axis .toBe. .false. )
  call mh_tester%expect( mh%active_flux .toBe. 0 )

  call rhyme_muscl_hancock_init( mh, samr, mhws, mh_fac_log )

  call mh_tester%expect( mh%active_axis .toBe. ( samr%base_grid > 1 ) )
  call mh_tester%expect( mh%active_flux .toBe. merge( 1, 0, samr%base_grid > 1 ) )

  failed = mh_tester%failed()
end function rhyme_muscl_hancock_init_test
