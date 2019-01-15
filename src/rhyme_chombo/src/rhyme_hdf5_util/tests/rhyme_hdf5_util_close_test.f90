logical function rhyme_hdf5_util_close_test () result ( failed )
  use rhyme_hdf5_util

  implicit none

  ! Constants
  character ( len=1024 ), parameter :: testfile = "./test_hdf5_util_close.h5"

  ! rhyme_hdf5_util variables
  type ( rhyme_hdf5_util_t ) :: h5

  call h5%create ( testfile )
  call h5%close

  failed = &
  h5%fid .ne. h5id%unset &
  .or. trim( h5%filename ) .ne. "" &
  .or. h5%initialized .eqv. .true.
end function rhyme_hdf5_util_close_test
