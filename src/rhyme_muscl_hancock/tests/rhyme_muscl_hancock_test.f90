logical function rhyme_muscl_hancock_test () result ( failed )
  use rhyme_muscl_hancock_factory

  implicit none

  type ( muscl_hancock_t ) :: mh


  call rhyme_muscl_hancock_factory_init

  failed = &
  mh%initialized &
  .or. any( mh%active_axis .eqv. .true. ) &
  .or. any( mh%active_flux .ne. 0 )
  if ( failed ) return

  call mh%init( samr, log )

  failed = &
  .not. mh%ws%initialized &
  .or. .not. mh%initialized &
  .or. any( mh%active_axis .neqv. [ .true., .true., .false. ] ) &
  .or. any( mh%active_flux .ne. [ 1, 1, 0 ] )
end function rhyme_muscl_hancock_test
