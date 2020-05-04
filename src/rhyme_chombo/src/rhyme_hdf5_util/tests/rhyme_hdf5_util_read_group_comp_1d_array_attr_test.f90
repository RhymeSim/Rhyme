logical function rhyme_hdf5_util_read_group_comp_1d_array_attr_test() result(failed)
   use rhyme_hdf5_util_factory
   use rhyme_assertion

   implicit none

   character(len=1024), parameter :: testfile = "./test_hdf5_util_read_group_comp_1d_array_attr.h5"
   character(len=8), parameter :: keys(3) = ["k_1     ", "k_2     ", "k_3     "]
   integer, parameter :: array_i(3) = [7, 8, 9]

   type(assertion_t) :: h5_tester

   type(hdf5_util_t) :: h5, h5read

   integer :: array_i_read(3) = 0

   h5_tester = .describe."read_group_comp_1d_array_attr"

   h5 = hdf5_util_factory_generate('empty')

   call rhyme_hdf5_util_create(h5, testfile)
   call rhyme_hdf5_util_write_group_comp_1d_array_attr(h5, "/", "int_attr", keys, array_i)
   call rhyme_hdf5_util_close(h5)

   call rhyme_hdf5_util_open(h5read, testfile)
   call rhyme_hdf5_util_read_group_comp_1d_array_attr(h5read, '/', 'int_attr', keys, array_i_read)
   call rhyme_hdf5_util_close(h5read)

   call h5_tester%expect(array_i.toBe.array_i_read)

   failed = h5_tester%failed()
end function rhyme_hdf5_util_read_group_comp_1d_array_attr_test
