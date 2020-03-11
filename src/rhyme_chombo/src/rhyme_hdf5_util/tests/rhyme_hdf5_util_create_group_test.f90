logical function rhyme_hdf5_util_create_group_test() result(failed)
   use rhyme_hdf5_util_factory
   use rhyme_assertion

   implicit none

   character(len=1024), parameter :: testfile = "./test_hdf5_util_create_group.h5"

   type(assertion_t) :: h5_tester

   type(hdf5_util_t) :: h5

   integer :: hdferr
   integer(hid_t) :: fid
   logical :: exists
   integer(hid_t) :: group_id = -1

   h5_tester = .describe."create_group"

   h5 = h5_factory%generate()

   call rhyme_hdf5_util_create(h5, testfile)
   call rhyme_hdf5_util_create_group(h5, "/new_group", group_id)
   call rhyme_hdf5_util_create_group(h5, "/new_group/nested_group", group_id)
   call rhyme_hdf5_util_close(h5)

   call h5open_f(hdferr)
   call h5fopen_f(testfile, H5F_ACC_RDONLY_F, fid, hdferr)

   call h5lexists_f(fid, "/new_group", exists, hdferr)

   call h5_tester%expect(exists.toBe..true.)
   call h5_tester%expect(int(group_id) .notToBe.-1)

   call h5lexists_f(fid, "/new_group/nested_group", exists, hdferr)

   call h5_tester%expect(exists.toBe..true.)
   call h5_tester%expect(int(group_id) .notToBe.-1)

   call h5fclose_f(fid, hdferr)
   call h5close_f(hdferr)

   failed = h5_tester%failed()
end function rhyme_hdf5_util_create_group_test
