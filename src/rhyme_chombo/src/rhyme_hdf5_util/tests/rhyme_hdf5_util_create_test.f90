logical function rhyme_hdf5_util_create_test () result ( failed )
  use rhyme_hdf5_util
  use rhyme_assertion

  implicit none

  type ( assertion_t ) :: h5_tester

  ! Constants
  character ( len=1024 ), parameter :: testfile = "./test_hdf5_util_create.h5"

  ! HDF5 related variables
  integer :: hdferr
  integer ( hid_t ) :: fid

  ! rhyme_hdf5_util variables
  type ( rhyme_hdf5_util_t ) :: h5

  h5_tester = .describe. "hdf5_util create"

  ! Check if create procedure initialize derived bound variables
  call h5%create ( testfile )


  call h5_tester%expect( int( h5%fid ) .notToBe. h5id%unset )
  call h5_tester%expect( h5%filename .toBe. testfile )

  ! Check if file exists
  call h5open_f ( hdferr )
  call h5fopen_f ( trim(testfile), H5F_ACC_RDONLY_F, fid, hdferr )

  call h5_tester%expect( ( hdferr >= 0 ) .toBe. .true. )

  call h5fclose_f ( fid, hdferr )
  call h5close_f ( hdferr )

  failed = h5_tester%failed()
end function rhyme_hdf5_util_create_test
