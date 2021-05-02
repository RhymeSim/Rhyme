logical function rhyme_hdf5_util_read_table_test() result(failed)
   use rhyme_hdf5_util_factory
   use rhyme_assertion

   implicit none

   character(len=1024), parameter :: testfile = 'rhyme_hdf5_util_read_table.h5'
   integer, parameter :: ncol = 3, nrow = 4
   character(len=8), parameter :: headers(ncol) = ['key_1   ', 'key_2   ', 'key_3   ']

   type(assertion_t) :: h5_tester

   type(hdf5_util_t) :: h5, h5read

   integer :: int_table(ncol, nrow), int_table_read(ncol, nrow)
   real(kind=4) :: real_table(ncol, nrow), real_table_read(ncol, nrow)
   real(kind=8) :: real8_table(ncol, nrow), real8_table_read(ncol, nrow)
   integer :: i

   h5_tester = .describe."read_table"

   h5 = hdf5_util_factory_generate('empty')

   int_table = reshape([(i, i=1, 12)], [ncol, nrow])
   real_table = reshape([(real(i, kind=4), i=1, 12)], [ncol, nrow])
   real8_table = reshape([(real(i, kind=8), i=1, 12)], [ncol, nrow])

   ! Preparing the hdf5 file
   call rhyme_hdf5_util_create(h5, testfile)
   call rhyme_hdf5_util_write_table(h5, '/', 'int-table', headers, int_table)
   call rhyme_hdf5_util_write_table(h5, '/', 'real-table', headers, real_table)
   call rhyme_hdf5_util_write_table(h5, '/', 'real8-table', headers, real8_table)
   call rhyme_hdf5_util_close(h5)

   call rhyme_hdf5_util_open(h5read, testfile)

   call rhyme_hdf5_util_read_table(h5read, '/', 'int-table', headers, int_table_read)
   call rhyme_hdf5_util_read_table(h5read, '/', 'real-table', headers, real_table_read)
   call rhyme_hdf5_util_read_table(h5read, '/', 'real8-table', headers, real8_table_read)

   call h5_tester%expect(int_table.toBe.int_table_read)
   call h5_tester%expect(real_table.toBe.real_table_read)
   call h5_tester%expect(real8_table.toBe.real8_table_read)

   call rhyme_hdf5_util_close(h5read)

   failed = h5_tester%failed()
end function rhyme_hdf5_util_read_table_test
