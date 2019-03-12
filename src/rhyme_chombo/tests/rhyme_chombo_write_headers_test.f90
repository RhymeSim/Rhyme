logical function rhyme_chombo_write_headers_test () result ( failed )
  use rhyme_chombo_factory

  implicit none

  ! rhyme_chombo variables
  type ( chombo_t ) :: ch

  ! variables
  integer :: ndims_read

  ! Chombo filename
  character ( len=1024 ), parameter :: nickname = "rhyme_chombo_write_headers"
  character ( len=1024 ) :: filename


  call rhyme_chombo_factory_init


  ch%nickname = nickname
  ch%iteration = chombo_fac_samr%levels(0)%iteration

  call ch%filename_generator( filename )
  call ch%create_chombo

  call ch%write_headers( chombo_fac_samr )

  call ch%close

  failed = &
  any( ch%level_ids(0:chombo_fac_samr%nlevels-1) .eq. chid%unset ) &
  .or. any( ch%level_ids(chombo_fac_samr%nlevels:) .ne. chid%unset ) &
  .or. ch%chombo_global_id .eq. chid%unset
  if ( failed ) return


  call ch%open( filename )

  call ch%read_group_attr( "/Chombo_global", "SpaceDim", ndims_read )

  failed = ndims_read .ne. 3

  call ch%close
end function rhyme_chombo_write_headers_test
