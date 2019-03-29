logical function rhyme_muscl_hancock_init_test () result ( failed )
  use rhyme_muscl_hancock_factory

  implicit none

  type ( muscl_hancock_t ) :: mh
  type ( mh_workspace_t ) :: mhws


  call rhyme_muscl_hancock_factory_init

  failed = &
  mh%solver_type .ne. mhid%memory_intensive &
  .or. any( mh%active_axis .neqv. .false. ) &
  .or. any( mh%active_flux .ne. 0 )
  if ( failed ) return

  call mh%init( mh_fac_samr, mhws, mh_fac_log )

  failed = &
  any( mh%active_axis .neqv. ( mh_fac_samr%base_grid > 1 ) ) &
  .or. any( mh%active_flux .ne. merge( 1, 0, mh_fac_samr%base_grid > 1 ) )
end function rhyme_muscl_hancock_init_test
