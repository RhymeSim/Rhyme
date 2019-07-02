logical function rhyme_hdf5_util_write_2d_dataset_test () result ( failed )
  use rhyme_hdf5_util_factory
  use rhyme_assertion

  implicit none

  character ( len=1024 ), parameter :: testfile = "./test_hdf5_util_write_2d_dataset.h5"

  type ( assertion_t ) :: h5_tester

  type ( hdf5_util_t ) :: h5

  ! Constants
  integer :: int_arr(2,3), int_arr_read(2,3)
  real ( kind=4 ) :: real_arr(2,3), real_arr_read(2,3)
  real ( kind=8 ) :: real8_arr(2,3), real8_arr_read(2,3)
  integer :: hdferr
  integer ( hid_t ) :: group_id, fid
  logical :: exists
  integer ( hid_t ) :: dset_id

  h5_tester = .describe. "hdf5_util write_2d_dataset"

  h5 = h5_factory%generate()

  int_arr = reshape ( [ 1, 2, 3, 4, 5, 6 ], [ 2, 3 ] )
  real_arr = reshape ( [ 1.e0, 2.e0, 3.e0, 4.e0, 5.e0, 6.e0 ], [ 2, 3 ] )
  real8_arr = reshape ( [ 1.d0, 2.d0, 3.d0, 4.d0, 5.d0, 6.d0 ], [ 2, 3 ] )

  call rhyme_hdf5_util_create( h5, testfile )
  call rhyme_hdf5_util_create_group( h5, "/dataset", group_id)
  call rhyme_hdf5_util_write_2d_dataset( h5, "/dataset", "int", int_arr )
  call rhyme_hdf5_util_write_2d_dataset( h5, "/dataset", "real", real_arr )
  call rhyme_hdf5_util_write_2d_dataset( h5, "/dataset", "real8", real8_arr )
  call rhyme_hdf5_util_close( h5 )

  call h5open_f ( hdferr )
  call h5fopen_f ( testfile, H5F_ACC_RDONLY_F, fid, hdferr )

  call h5lexists_f ( fid, "/dataset/int", exists, hdferr )
  call h5_tester%expect( exists .toBe. .true. )

  call h5lexists_f ( fid, "/dataset/real", exists, hdferr )
  call h5_tester%expect( exists .toBe. .true. )

  call h5lexists_f ( fid, "/dataset/real8", exists, hdferr )
  call h5_tester%expect( exists .toBe. .true. )
  call h5_tester%expect( int( group_id ) .notToBe. -1 )

  call h5dopen_f ( fid, "/dataset/int", dset_id, hdferr )
  call h5dread_f ( dset_id, H5T_NATIVE_INTEGER, int_arr_read, &
    int(shape(int_arr), kind=hsize_t), hdferr )
  call h5dclose_f ( dset_id, hdferr )

  call h5_tester%expect( int_arr .toBe. int_arr_read )

  call h5dopen_f ( fid, "/dataset/real", dset_id, hdferr )
  call h5dread_f ( dset_id, H5T_NATIVE_REAL, real_arr_read, &
    int(shape(int_arr), kind=hsize_t), hdferr )
  call h5dclose_f ( dset_id, hdferr )

  call h5_tester%expect( real_arr .toBe. real_arr_read )

  call h5dopen_f ( fid, "/dataset/real8", dset_id, hdferr )
  call h5dread_f ( dset_id, H5T_NATIVE_DOUBLE, real8_arr_read, &
    int(shape(int_arr), kind=hsize_t), hdferr )
  call h5dclose_f ( dset_id, hdferr )

  call h5_tester%expect( real8_arr .toBe. real8_arr_read )

  call h5fclose_f ( fid, hdferr )
  call h5close_f ( hdferr )

  failed = h5_tester%failed()
end function rhyme_hdf5_util_write_2d_dataset_test
