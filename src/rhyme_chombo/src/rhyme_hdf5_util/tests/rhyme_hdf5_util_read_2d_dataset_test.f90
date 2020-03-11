logical function rhyme_hdf5_util_read_2d_dataset_test() result(failed)
   use rhyme_hdf5_util_factory
   use rhyme_assertion

   implicit none

   character(len=1024), parameter :: testfile = "./test_hdf5_util_write_1d_dataset.h5"

   type(assertion_t) :: h5_tester

   type(hdf5_util_t) :: h5

   integer :: int_arr(2, 3), int_arr_read(2, 3)
   real(kind=4) :: real_arr(2, 3), real_arr_read(2, 3)
   real(kind=8) :: real8_arr(2, 3), real8_arr_read(2, 3)
   integer(hid_t) :: group_id = -1

   h5_tester = .describe."read_2d_dataset"

   h5 = h5_factory%generate()

   int_arr = reshape([1, 2, 3, 4, 5, 6], [2, 3])
   real_arr = reshape([1.2e0, 2.3e0, 3.4e0, 4.5e0, 5.6e0, 6.7e0], [2, 3])
   real8_arr = reshape([1.2d0, 2.3d0, 3.4d0, 4.5d0, 5.6d0, 6.7d0], [2, 3])

   ! Prepare chombo file
   call rhyme_hdf5_util_create(h5, testfile)
   call rhyme_hdf5_util_create_group(h5, "/dataset", group_id)
   call rhyme_hdf5_util_write_2d_dataset(h5, "/dataset", "int", int_arr)
   call rhyme_hdf5_util_write_2d_dataset(h5, "/dataset", "real", real_arr)
   call rhyme_hdf5_util_write_2d_dataset(h5, "/dataset", "real8", real8_arr)
   call rhyme_hdf5_util_close(h5)

   call rhyme_hdf5_util_open(h5, testfile)

   call rhyme_hdf5_util_read_2d_dataset(h5, "/dataset/int", int_arr_read)
   call h5_tester%expect(int_arr.toBe.int_arr_read)

   call rhyme_hdf5_util_read_2d_dataset(h5, "/dataset/real", real_arr_read)
   call h5_tester%expect(real_arr.toBe.real_arr_read)

   call rhyme_hdf5_util_read_2d_dataset(h5, "/dataset/real8", real8_arr_read)
   call h5_tester%expect(real8_arr.toBe. (real8_arr_read))

   call rhyme_hdf5_util_close(h5)

   failed = h5_tester%failed()
end function rhyme_hdf5_util_read_2d_dataset_test
