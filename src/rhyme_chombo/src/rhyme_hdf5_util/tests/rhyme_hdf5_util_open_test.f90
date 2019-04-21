logical function rhyme_hdf5_util_open_test () result ( failed )
  use rhyme_hdf5_util
  use rhyme_assertion

  implicit none

  type ( assertion_t ) :: h5_tester

  ! Constants
  character ( len=1024 ), parameter :: testfile = "./test_hdf5_util_open.h5"

  ! rhyme_hdf5_util variables
  type ( rhyme_hdf5_util_t ) :: h5

  ! variables
  integer :: hdferr
  integer ( hid_t ) :: fid

  h5_tester = .describe. "hdf5_util open"

  call h5open_f ( hdferr )
  call h5fopen_f ( trim (testfile), H5F_ACC_RDWR_F, fid, hdferr )
  call h5fclose_f ( fid, hdferr )
  call h5close_f ( hdferr )

  call h5%open ( testfile )

  call h5_tester%expect( h5%initialized .toBe. .true. )
  call h5_tester%expect( h5%filename .toBe. testfile )

  failed = h5_tester%failed()
end function rhyme_hdf5_util_open_test
