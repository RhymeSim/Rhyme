logical function rhyme_hdf5_util_write_group_attr_test () result ( failed )
  use rhyme_hdf5_util

  implicit none

  ! Constants
  character ( len=1024 ), parameter :: testfile = "./test_hdf5_util_write_group_attr.h5"

  ! HDF5 related variables
  integer :: hdferr
  integer ( hid_t ) :: fid, attr_id, dtype
  integer ( hsize_t ) :: dims(1) = 1

  ! rhyme_hdf5_util variables
  type ( rhyme_hdf5_util_t ) :: h5

  ! variables
  character(len=128) :: key
  integer :: attr_int
  real (kind=4) :: attr_real
  real (kind=8) :: attr_real8
  character(len=128), target :: attr_char


  call h5%create ( testfile )
  call h5%write_group_attr ( "/", "int", 1 )
  call h5%write_group_attr ( "/", "real", 2.34e0 )
  call h5%write_group_attr ( "/", "real8", 3.45d0 )
  call h5%write_group_attr ( "/", "string", "Hello world!" )
  call h5%close


  call h5open_f ( hdferr )
  call h5fopen_f ( trim(testfile), H5F_ACC_RDONLY_F, fid, hdferr )

  call h5aopen_by_name_f ( fid, "/", "int", attr_id, hdferr )
  call h5aread_f ( attr_id, H5T_NATIVE_INTEGER, attr_int, dims, hdferr )
  call h5aget_name_f ( attr_id, int ( 128, kind=size_t ), key, hdferr )
  call h5aclose_f ( attr_id, hdferr )

  failed = hdferr < 0 &
  .or. trim ( key ) .ne. "int" &
  .or. attr_int .ne. 1
  if ( failed ) return

  call h5aopen_by_name_f ( fid, "/", "real", attr_id, hdferr )
  call h5aread_f ( attr_id, H5T_NATIVE_REAL, attr_real, dims, hdferr )
  call h5aget_name_f ( attr_id, int ( 128, kind=size_t ), key, hdferr )
  call h5aclose_f ( attr_id, hdferr )

  failed = hdferr < 0 &
  .or. trim ( key ) .ne. "real" &
  .or. abs ( attr_real - 2.34e0 ) > epsilon(0.e0)

  if ( failed ) return

  call h5aopen_by_name_f ( fid, "/", "real8", attr_id, hdferr )
  call h5aread_f ( attr_id, H5T_NATIVE_DOUBLE, attr_real8, dims, hdferr )
  call h5aget_name_f ( attr_id, int ( 128, kind=size_t ), key, hdferr )
  call h5aclose_f ( attr_id, hdferr )

  failed = hdferr < 0 &
  .or. trim ( key ) .ne. "real8" &
  .or. abs ( attr_real8 - 3.45d0 ) > epsilon(0.d0)

  if ( failed ) return

  call h5aopen_by_name_f ( fid, "/", "string", attr_id, hdferr )

  call h5tcopy_f ( H5T_NATIVE_CHARACTER, dtype, hdferr )
  call h5tset_size_f ( dtype, int ( 128, kind=size_t ), hdferr )
  call h5aread_f ( attr_id, dtype, attr_char, dims, hdferr )
  call h5aget_name_f ( attr_id, int ( 128, kind=size_t ), key, hdferr )

  failed = hdferr < 0 &
  .or. trim ( key ) .ne. "string" &
  .or. trim ( attr_char ) .ne. "Hello world!"

  print *,attr_char

  call h5fclose_f ( fid, hdferr )
end function rhyme_hdf5_util_write_group_attr_test
