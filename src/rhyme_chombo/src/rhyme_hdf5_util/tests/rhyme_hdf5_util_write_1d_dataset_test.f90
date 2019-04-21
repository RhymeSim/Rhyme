logical function rhyme_hdf5_util_write_1d_dataset_test () result ( failed )
  use rhyme_hdf5_util
  use rhyme_assertion

  implicit none

  type ( assertion_t ) :: h5_tester

  ! Constants
  character ( len=1024 ), parameter :: testfile = "./test_hdf5_util_write_1d_dataset.h5"
  integer, parameter :: int_arr(6) = [ 1, 2, 3, 4, 5, 6 ]
  real ( kind=4 ), parameter :: real_arr(6) = [ 1.2e0, 2.3e0, 3.4e0, 4.5e0, 5.6e0, 6.7e0 ]
  real ( kind=8 ), parameter :: real8_arr(6) = [ 1.2d0, 2.3d0, 3.4d0, 4.5d0, 5.6d0, 6.7d0 ]

  integer :: int_arr_read(6)
  real ( kind=4 ) :: real_arr_read(6)
  real ( kind=8 ) :: real8_arr_read(6)

  ! HDF5 related variables
  integer :: hdferr
  integer ( hid_t ) :: fid
  logical :: exists

  ! rhyme_hdf5_util variables
  type ( rhyme_hdf5_util_t ) :: h5
  integer ( hid_t ) :: group_id = -1, dset_id = -1

  h5_tester = .describe. "hdf5_util write_1d_dataset"

  call h5%create ( testfile )
  call h5%create_group ( "/group", group_id)
  call h5%write_1d_dataset ( "/group", "intdataset", int_arr )
  call h5%write_1d_dataset ( "/group", "realdataset", real_arr )
  call h5%write_1d_dataset ( "/group", "real8dataset", real8_arr )
  call h5%close

  call h5open_f ( hdferr )
  call h5fopen_f ( testfile, H5F_ACC_RDONLY_F, fid, hdferr )


  call h5lexists_f ( fid, "/group/intdataset", exists, hdferr )
  call h5_tester%expect( exists .toBe. .true. )

  call h5lexists_f ( fid, "/group/realdataset", exists, hdferr )
  call h5_tester%expect( exists .toBe. .true. )

  call h5lexists_f ( fid, "/group/real8dataset", exists, hdferr )
  call h5_tester%expect( exists .toBe. .true. )
  call h5_tester%expect( int( group_id ) .notToBe. -1 )

  call h5dopen_f ( fid, "/group/intdataset", dset_id, hdferr )
  call h5dread_f ( dset_id, H5T_NATIVE_INTEGER, int_arr_read, &
    int(shape(int_arr), kind=hsize_t), hdferr )
  call h5dclose_f ( dset_id, hdferr )

  call h5_tester%expect( int_arr .toBe. int_arr_read )

  call h5dopen_f ( fid, "/group/realdataset", dset_id, hdferr )
  call h5dread_f ( dset_id, H5T_NATIVE_REAL, real_arr_read, &
    int(shape(int_arr), kind=hsize_t), hdferr )
  call h5dclose_f ( dset_id, hdferr )

  call h5_tester%expect( real_arr .toBe. real_arr_read )

  call h5dopen_f ( fid, "/group/real8dataset", dset_id, hdferr )
  call h5dread_f ( dset_id, H5T_NATIVE_DOUBLE, real8_arr_read, &
    int(shape(int_arr), kind=hsize_t), hdferr )
  call h5dclose_f ( dset_id, hdferr )

  call h5_tester%expect( real8_arr .toBe. real8_arr_read )

  call h5fclose_f ( fid, hdferr )
  call h5close_f ( hdferr )

  failed = h5_tester%failed()
end function rhyme_hdf5_util_write_1d_dataset_test
