logical function rhyme_chombo_add_array_attr_test () result ( failed )
  use rhyme_chombo_factory

  implicit none

  type ( rhyme_chombo_t ) :: ch

  integer, parameter :: len = 5

  integer, parameter :: array_i(len) = [ 1, 2, 3, 4, 5 ]
  real ( kind = 4 ), parameter :: array_r(len) = [ 1.e0, 2.e0, 3.e0, 4.e0, 5.e0 ]
  real ( kind = 8 ), parameter :: array_r8(len) = [ 1.d0, 2.d0, 3.d0, 4.d0, 5.d0 ]

  integer ( hsize_t ), parameter :: dims(1) = int ( len, kind=hsize_t )
  character(len=256), parameter :: testfile = "./test_chombo_file_add_array_attr.h5"

  integer :: hdferr
  integer ( hid_t ) :: file_id, attr_id
  character(len=128) :: key_i, key_r, key_r8

  integer :: attr_i(len) = 0
  real ( kind = 4 ) :: attr_r(len) = 0.e0
  real ( kind = 8 ) :: attr_r8(len) = 0.d0

  call ch%setup ( testfile, samr )
  call ch%add_array_attr ( "/", "array_i", 1, [5], array_i )
  call ch%add_array_attr ( "/", "array_r", 1, [5], array_r )
  call ch%add_array_attr ( "/", "array_r8", 1, [5], array_r8 )
  call ch%close


  call h5open_f ( hdferr )
  call h5fopen_f ( trim(testfile), H5F_ACC_RDONLY_F, file_id, hdferr )

  call h5aopen_by_name_f ( file_id, "/", "array_i", attr_id, hdferr )
  call h5aread_f ( attr_id, H5T_NATIVE_INTEGER, attr_i, dims, hdferr )
  call h5aget_name_f ( attr_id, int ( 128, kind=size_t ), key_i, hdferr )
  call h5aclose_f ( attr_id, hdferr )

  failed = hdferr < 0
  if ( failed ) return

  call h5aopen_by_name_f ( file_id, "/", "array_r", attr_id, hdferr )
  call h5aread_f ( attr_id, H5T_NATIVE_REAL, attr_r, dims, hdferr )
  call h5aget_name_f ( attr_id, int ( 128, kind=size_t ), key_r, hdferr )
  call h5aclose_f ( attr_id, hdferr )

  failed = hdferr < 0
  if ( failed ) return

  call h5aopen_by_name_f ( file_id, "/", "array_r8", attr_id, hdferr )
  call h5aread_f ( attr_id, H5T_NATIVE_DOUBLE, attr_r8, dims, hdferr )
  call h5aget_name_f ( attr_id, int ( 128, kind=size_t ), key_r8, hdferr )
  call h5aclose_f ( attr_id, hdferr )

  failed = hdferr < 0
  if ( failed ) return

  call h5fclose_f ( file_id, hdferr )

  failed = &
  any ( attr_i .ne. array_i ) .or. trim(key_i) .ne. "array_i" &
  .or. any ( abs ( attr_r - array_r ) > epsilon(0.e0) ) .or. trim(key_r) .ne. "array_r" &
  .or. any ( abs ( attr_r8 - array_r8 ) > epsilon(0.d0) ) .or. trim(key_r8) .ne. "array_r8"

end function rhyme_chombo_add_array_attr_test
