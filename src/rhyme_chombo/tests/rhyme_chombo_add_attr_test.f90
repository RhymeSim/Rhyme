logical function rhyme_chombo_add_attr_test () result ( failed )
  use rhyme_chombo_factory

  implicit none

  type ( rhyme_chombo_t ) :: ch
  integer ( hid_t ) :: file_id, attr_id, dtype
  integer ( hsize_t ) :: dims(1) = 1
  integer :: hdferr
  character(len=128) :: key

  integer :: attr_i
  real :: attr_r
  real(kind=8) :: attr_r8
  character(len=128), target :: attr_c


  character(len=256), parameter :: testfile = "./test_chombo_file_add_attr.h5"

  call ch%setup ( testfile, samr )
  call ch%add_attr ( "/", "int", 1 )
  call ch%add_attr ( "/", "real", 2.34e0 )
  call ch%add_attr ( "/", "real8", 3.45d0 )
  call ch%add_attr ( "/", "string", "Hello world!" )
  call ch%close


  call h5open_f ( hdferr )
  call h5fopen_f ( trim(testfile), H5F_ACC_RDONLY_F, file_id, hdferr )

  call h5aopen_by_name_f ( file_id, "/", "int", attr_id, hdferr )
  call h5aread_f ( attr_id, H5T_NATIVE_INTEGER, attr_i, dims, hdferr )
  call h5aget_name_f ( attr_id, int(128, kind=size_t), key, hdferr )
  call h5aclose_f ( attr_id, hdferr )

  failed = &
  hdferr < 0 &
  .or. trim(key) .ne. "int" &
  .or. attr_i .ne. 1

  if ( failed ) return

  call h5aopen_by_name_f ( file_id, "/", "real", attr_id, hdferr )
  call h5aread_f ( attr_id, H5T_NATIVE_REAL, attr_r, dims, hdferr )
  call h5aget_name_f ( attr_id, int(128, kind=size_t), key, hdferr )
  call h5aclose_f ( attr_id, hdferr )

  failed = &
  hdferr < 0 &
  .or. trim(key) .ne. "real" &
  .or. abs ( attr_r - 2.34e0 ) > epsilon(0.e0)

  if ( failed ) return

  call h5aopen_by_name_f ( file_id, "/", "real8", attr_id, hdferr )
  call h5aread_f ( attr_id, H5T_NATIVE_DOUBLE, attr_r8, dims, hdferr )
  call h5aget_name_f ( attr_id, int(128, kind=size_t), key, hdferr )
  call h5aclose_f ( attr_id, hdferr )

  failed = &
  hdferr < 0 &
  .or. trim(key) .ne. "real8" &
  .or. abs ( attr_r8 - 3.45d0 ) > epsilon(0.d0)

  if ( failed ) return

  call h5aopen_by_name_f ( file_id, "/", "string", attr_id, hdferr )

  call h5tcopy_f ( H5T_NATIVE_CHARACTER, dtype, hdferr )
  call h5tset_size_f ( dtype, int(128, kind=size_t), hdferr )
  call h5aread_f ( attr_id, dtype, attr_c, dims, hdferr )
  call h5aget_name_f ( attr_id, int(128, kind=size_t), key, hdferr )

  failed = &
  hdferr < 0 &
  .or. trim(key) .ne. "string" &
  .or. trim(attr_c) .ne. "Hello world!"

  call h5fclose_f ( file_id, hdferr )
end function rhyme_chombo_add_attr_test
