logical function rhyme_hdf5_util_add_group_1d_array_attr_test () result ( failed )
  use rhyme_hdf5_util

  implicit none

  ! Constants
  character ( len=1024 ), parameter :: testfile = "./test_hdf5_util_add_group_1d_array_attr.h5"
  integer, parameter :: array_int(5) = [ 1, 2, 3, 4, 5 ]
  real ( kind=4 ), parameter :: array_real(5) = [ 1.e0, 2.e0, 3.e0, 4.e0, 5.e0 ]
  real ( kind=8 ), parameter :: array_real8(5) = [ 1.d0, 2.d0, 3.d0, 4.d0, 5.d0 ]

  ! HDF5 related variables
  integer :: hdferr
  integer ( hid_t ) :: fid, attr_id
  integer ( hsize_t ) :: dims(1) = 1

  ! rhyme_hdf5_util variables
  type ( rhyme_hdf5_util_t ) :: h5

  ! variables
  character(len=128) :: key_int, key_real, key_real8
  integer :: attr_int(5) = 0
  real ( kind=4 ) :: attr_real(5) = 0.e0
  real ( kind=8 ) :: attr_real8(5) = 0.d0


  call h5%create ( testfile )
  call h5%add_group_1d_array_attr ( "/", "array_i", array_int )
  call h5%add_group_1d_array_attr ( "/", "array_r", array_real )
  call h5%add_group_1d_array_attr ( "/", "array_r8", array_real8 )
  call h5%close


  call h5open_f ( hdferr )
  call h5fopen_f ( trim(testfile), H5F_ACC_RDONLY_F, fid, hdferr )

  call h5aopen_by_name_f ( fid, "/", "array_i", attr_id, hdferr )
  call h5aread_f ( attr_id, H5T_NATIVE_INTEGER, attr_int, dims, hdferr )
  call h5aget_name_f ( attr_id, int ( 128, kind=size_t ), key_int, hdferr )
  call h5aclose_f ( attr_id, hdferr )

  failed = hdferr < 0
  if ( failed ) return

  call h5aopen_by_name_f ( fid, "/", "array_r", attr_id, hdferr )
  call h5aread_f ( attr_id, H5T_NATIVE_REAL, attr_real, dims, hdferr )
  call h5aget_name_f ( attr_id, int ( 128, kind=size_t ), key_real, hdferr )
  call h5aclose_f ( attr_id, hdferr )

  failed = hdferr < 0
  if ( failed ) return

  call h5aopen_by_name_f ( fid, "/", "array_r8", attr_id, hdferr )
  call h5aread_f ( attr_id, H5T_NATIVE_DOUBLE, attr_real8, dims, hdferr )
  call h5aget_name_f ( attr_id, int ( 128, kind=size_t ), key_real8, hdferr )
  call h5aclose_f ( attr_id, hdferr )

  failed = hdferr < 0
  if ( failed ) return

  call h5fclose_f ( fid, hdferr )

  failed = &
  any ( attr_int .ne. array_int ) .or. trim(key_int) .ne. "array_i" &
  .or. any ( abs ( attr_real - array_real ) > epsilon(0.e0) ) .or. trim(key_real) .ne. "array_r" &
  .or. any ( abs ( attr_real8 - array_real8 ) > epsilon(0.d0) ) .or. trim(key_real8) .ne. "array_r8"

end function rhyme_hdf5_util_add_group_1d_array_attr_test
