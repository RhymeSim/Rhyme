logical function rhyme_hdf5_util_get_table_size_test() result(failed)
   use rhyme_hdf5_util_factory
   use rhyme_assertion

   implicit none

   character(len=1024), parameter :: testfile = 'rhyme_hdf5_util_get_table_dims.h5'
   integer, parameter :: ncol = 3, nrow = 4
   character(len=8), parameter :: headers(ncol) = ['key_1   ', 'key_2   ', 'key_3   ']
   integer :: int_table(ncol, nrow)

   type(assertion_t) :: h5_tester

   type(hdf5_util_t) :: h5, h5read
   integer :: i, arr_size

   h5_tester = .describe."get_table_size"

   h5 = h5_factory%generate()
   int_table = reshape([(i, i=1, 12)], [ncol, nrow])

   call rhyme_hdf5_util_create(h5, testfile)
   call rhyme_hdf5_util_write_table(h5, '/', 'int-table', headers, int_table)
   call rhyme_hdf5_util_close(h5)

   call rhyme_hdf5_util_open(h5read, testfile)
   arr_size = rhyme_hdf5_util_get_table_size(h5read, '/int-table')
   call rhyme_hdf5_util_close(h5read)

   call h5_tester%expect(arr_size.toBe.nrow)

   failed = h5_tester%failed()
end function rhyme_hdf5_util_get_table_size_test
