logical function rhyme_hdf5_util_open_test () result ( failed )
  use rhyme_hdf5_util

  implicit none

  ! Constants
  character ( len=1024 ), parameter :: testfile = "./test_hdf5_util_open.h5"

  ! rhyme_hdf5_util variables
  type ( rhyme_hdf5_util_t ) :: h5

  ! variables
  integer :: hdferr
  integer ( hid_t ) :: fid


  call h5open_f ( hdferr )
  call h5fopen_f ( trim (testfile), H5F_ACC_RDWR_F, fid, hdferr )
  call h5fclose_f ( fid, hdferr )
  call h5close_f ( hdferr )

  call h5%open ( testfile )

  failed = .not. h5%initialized &
  .or. trim ( h5%filename ) .ne. trim ( testfile )
end function rhyme_hdf5_util_open_test
