logical function rhyme_hdf5_util_write_2d_dataset_test () result ( failed )
  use rhyme_hdf5_util

  implicit none

  ! Constants
  character ( len=1024 ), parameter :: testfile = "./test_hdf5_util_write_2d_dataset.h5"
  integer :: int_arr(2,3), int_arr_read(2,3)
  real ( kind=4 ) :: real_arr(2,3), real_arr_read(2,3)
  real ( kind=8 ) :: real8_arr(2,3), real8_arr_read(2,3)

  ! HDF5 related variables
  integer :: hdferr
  integer ( hid_t ) :: group_id, fid
  logical :: exists

  ! rhymej_hdf5_util variables
  type ( rhyme_hdf5_util_t ) :: h5
  integer ( hid_t ) :: dset_id


  int_arr = reshape ( [ 1, 2, 3, 4, 5, 6 ], [ 2, 3 ] )
  real_arr = reshape ( [ 1.e0, 2.e0, 3.e0, 4.e0, 5.e0, 6.e0 ], [ 2, 3 ] )
  real8_arr = reshape ( [ 1.d0, 2.d0, 3.d0, 4.d0, 5.d0, 6.d0 ], [ 2, 3 ] )


  call h5%create ( testfile )
  call h5%create_group ( "/dataset", group_id)
  call h5%write_2d_dataset ( "/dataset", "int", int_arr )
  call h5%write_2d_dataset ( "/dataset", "real", real_arr )
  call h5%write_2d_dataset ( "/dataset", "real8", real8_arr )
  call h5%close


  call h5open_f ( hdferr )
  call h5fopen_f ( testfile, H5F_ACC_RDONLY_F, fid, hdferr )


  call h5lexists_f ( fid, "/dataset/int", exists, hdferr )
  failed = .not. exists
  if ( failed ) return

  call h5lexists_f ( fid, "/dataset/real", exists, hdferr )
  failed = .not. exists
  if ( failed ) return

  call h5lexists_f ( fid, "/dataset/real8", exists, hdferr )
  failed = .not. exists .or. group_id .eq. -1
  if ( failed ) return


  call h5dopen_f ( fid, "/dataset/int", dset_id, hdferr )
  call h5dread_f ( dset_id, H5T_NATIVE_INTEGER, int_arr_read, &
    int(shape(int_arr), kind=hsize_t), hdferr )
  call h5dclose_f ( dset_id, hdferr )

  failed = any ( int_arr_read .ne. int_arr )
  if ( failed ) return


  call h5dopen_f ( fid, "/dataset/real", dset_id, hdferr )
  call h5dread_f ( dset_id, H5T_NATIVE_REAL, real_arr_read, &
    int(shape(int_arr), kind=hsize_t), hdferr )
  call h5dclose_f ( dset_id, hdferr )

  failed = any ( abs ( real_arr_read - real_arr ) > epsilon(0.e0) )
  if ( failed ) return


  call h5dopen_f ( fid, "/dataset/real8", dset_id, hdferr )
  call h5dread_f ( dset_id, H5T_NATIVE_DOUBLE, real8_arr_read, &
    int(shape(int_arr), kind=hsize_t), hdferr )
  call h5dclose_f ( dset_id, hdferr )

  failed = any ( abs ( real8_arr_read - real8_arr ) > epsilon(0.d0) )
  if ( failed ) return


  call h5fclose_f ( fid, hdferr )
  call h5close_f ( hdferr )

end function rhyme_hdf5_util_write_2d_dataset_test
