logical function rhyme_hdf5_util_create_test () result ( failed )
  use rhyme_hdf5_util

  implicit none

  ! Constants
  character ( len=1024 ), parameter :: testfile = "./test_hdf5_util_create.h5"

  ! HDF5 related variables
  integer :: hdferr
  integer ( hid_t ) :: fid

  ! rhyme_hdf5_util variables
  type ( rhyme_hdf5_util_t ) :: h5


  ! Check if create procedure initialize derived bound variables
  call h5%create ( testfile )

  failed = &
  h5%fid .eq. h5id%unset &
  .or. trim ( h5%filename ) .ne. trim ( testfile )
  if ( failed ) return


  ! Check if file exists
  call h5open_f ( hdferr )
  call h5fopen_f ( trim(testfile), H5F_ACC_RDONLY_F, fid, hdferr )

  failed = hdferr < 0
  if ( failed ) return

  call h5fclose_f ( fid, hdferr )
  call h5close_f ( hdferr )
end function rhyme_hdf5_util_create_test
