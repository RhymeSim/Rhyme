logical function rhyme_hdf5_util_write_group_attr_test() result(failed)
   use rhyme_hdf5_util_factory
   use rhyme_assertion

   implicit none

   character(len=1024), parameter :: testfile = "./test_hdf5_util_write_group_attr.h5"

   type(assertion_t) :: h5_tester

   type(hdf5_util_t) :: h5

   integer :: hdferr
   integer(hid_t) :: fid, attr_id, dtype
   integer(hsize_t) :: dims(1) = 1
   character(len=128) :: key
   integer :: attr_int
   real(kind=4) :: attr_real
   real(kind=8) :: attr_real8
   character(len=128), target :: attr_char

   h5_tester = .describe."hdf5_util write_group_attr"

   h5 = h5_factory%generate()

   call rhyme_hdf5_util_create(h5, testfile)
   call rhyme_hdf5_util_write_group_attr(h5, "/", "int", 1)
   call rhyme_hdf5_util_write_group_attr(h5, "/", "real", 2.34e0)
   call rhyme_hdf5_util_write_group_attr(h5, "/", "real8", 3.45d0)
   call rhyme_hdf5_util_write_group_attr(h5, "/", "string", "Hello world!")
   call rhyme_hdf5_util_close(h5)

   call h5open_f(hdferr)
   call h5fopen_f(trim(testfile), H5F_ACC_RDONLY_F, fid, hdferr)

   call h5aopen_by_name_f(fid, "/", "int", attr_id, hdferr)
   call h5aread_f(attr_id, H5T_NATIVE_INTEGER, attr_int, dims, hdferr)
   call h5aget_name_f(attr_id, int(128, kind=size_t), key, hdferr)
   call h5aclose_f(attr_id, hdferr)

   call h5_tester%expect((hdferr >= 0) .toBe..true.)
   call h5_tester%expect(key.toBe.'int')
   call h5_tester%expect(attr_int.toBe.1)

   call h5aopen_by_name_f(fid, "/", "real", attr_id, hdferr)
   call h5aread_f(attr_id, H5T_NATIVE_REAL, attr_real, dims, hdferr)
   call h5aget_name_f(attr_id, int(128, kind=size_t), key, hdferr)
   call h5aclose_f(attr_id, hdferr)

   call h5_tester%expect((hdferr >= 0) .toBe..true.)
   call h5_tester%expect(key.toBe.'real')
   call h5_tester%expect(attr_real.toBe.2.34e0)

   call h5aopen_by_name_f(fid, "/", "real8", attr_id, hdferr)
   call h5aread_f(attr_id, H5T_NATIVE_DOUBLE, attr_real8, dims, hdferr)
   call h5aget_name_f(attr_id, int(128, kind=size_t), key, hdferr)
   call h5aclose_f(attr_id, hdferr)

   call h5_tester%expect((hdferr >= 0) .toBe..true.)
   call h5_tester%expect(key.toBe.'real8')
   call h5_tester%expect(attr_real8.toBe.3.45d0)

   call h5aopen_by_name_f(fid, "/", "string", attr_id, hdferr)

   call h5tcopy_f(H5T_NATIVE_CHARACTER, dtype, hdferr)
   call h5tset_size_f(dtype, int(128, kind=size_t), hdferr)
   call h5aread_f(attr_id, dtype, attr_char, dims, hdferr)
   call h5aget_name_f(attr_id, int(128, kind=size_t), key, hdferr)

   call h5_tester%expect((hdferr >= 0) .toBe..true.)
   call h5_tester%expect(key.toBe.'string')
   call h5_tester%expect(attr_char.toBe.'Hello world!')

   call h5fclose_f(fid, hdferr)
   call h5close_f(hdferr)

   failed = h5_tester%failed()
end function rhyme_hdf5_util_write_group_attr_test
