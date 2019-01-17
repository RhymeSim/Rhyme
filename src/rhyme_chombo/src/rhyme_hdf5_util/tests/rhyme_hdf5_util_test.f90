logical function rhyme_hdf5_util_test () result ( failed )
  use rhyme_hdf5_util

  implicit none

  type ( rhyme_hdf5_util_t ) :: h5

  failed = &
  h5id%unset .ne. -1 &
  .or. h5%initialized .eqv. .true. &
  .or. h5%fid .ne. h5id%unset
end function rhyme_hdf5_util_test
