logical function rhyme_chombo_write_test () result ( failed )
  use rhyme_chombo_factory

  implicit none

  ! Constants
  character ( len=1024 ), parameter :: testfile = "./test_chombo_write.h5"

  ! rhyme_chombo variables
  type ( rhyme_chombo_t ) :: ch

  ! variables
  integer :: ndims, ndims_read


  call rhyme_chombo_factory_init
  call ch%write ( testfile, samr )

  failed = .not. ch%initialized &
  .or. any ( ch%level_ids(0:samr%nlevels-1) .eq. chid%unset ) &
  .or. any ( ch%level_ids(samr%nlevels:) .ne. chid%unset )
  if ( failed ) return

  ndims = size ( samr%base_grid ) - sum ( samr%base_grid * merge ( 1, 0, samr%base_grid <= 1 ) )
  call ch%read_group_attr ( "/chombo_global", "SpaceDim", ndims_read )
  failed = ndims .ne. ndims_read

  call ch%close
end function rhyme_chombo_write_test
