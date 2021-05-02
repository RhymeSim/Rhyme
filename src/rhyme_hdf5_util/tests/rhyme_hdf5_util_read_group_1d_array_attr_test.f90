logical function rhyme_hdf5_util_read_group_1d_array_attr_test() result(failed)
   use rhyme_hdf5_util_factory
   use rhyme_assertion

   implicit none

   character(len=1024), parameter :: testfile = "./test_hdf5_util_read_group_1d_array_attr.h5"

   type(assertion_t) :: h5_tester

   type(hdf5_util_t) :: h5, h5read

   integer :: int_arr(6) = [1, 2, 3, 4, 5, 6]
   integer :: int_arr_read(6) = 0
   real(kind=4) :: real_arr(6) = [1.e0, 2.e0, 3.e0, 4.e0, 5.e0, 6.e0]
   real(kind=4) :: real_arr_read(6) = 0.e0
   real(kind=8) :: real8_arr(6) = [1.d0, 2.d0, 3.d0, 4.d0, 5.d0, 6.d0]
   real(kind=8) :: real8_arr_read(6) = 0.e0

   h5_tester = .describe."read_group_1d_array_attr"

   h5 = hdf5_util_factory_generate('empty')

   ! Creating hdf5 file
   call rhyme_hdf5_util_create(h5, testfile)
   call rhyme_hdf5_util_write_group_1d_array_attr(h5, '/', 'int', int_arr)
   call rhyme_hdf5_util_write_group_1d_array_attr(h5, '/', 'real', real_arr)
   call rhyme_hdf5_util_write_group_1d_array_attr(h5, '/', 'real8', real8_arr)
   call rhyme_hdf5_util_close(h5)

   ! test
   call rhyme_hdf5_util_open(h5read, testfile)
   call rhyme_hdf5_util_read_group_1d_array_attr(h5read, '/', 'int', int_arr_read)
   call rhyme_hdf5_util_read_group_1d_array_attr(h5read, '/', 'real', real_arr_read)
   call rhyme_hdf5_util_read_group_1d_array_attr(h5read, '/', 'real8', real8_arr_read)
   call rhyme_hdf5_util_close(h5)

   call h5_tester%expect(int_arr_read.toBe.int_arr)
   call h5_tester%expect(real_arr_read.toBe.real_arr)
   call h5_tester%expect(real8_arr_read.toBe.real8_arr)

   failed = h5_tester%failed()
end function rhyme_hdf5_util_read_group_1d_array_attr_test
