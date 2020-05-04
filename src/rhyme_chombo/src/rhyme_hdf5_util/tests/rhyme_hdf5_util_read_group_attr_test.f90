logical function rhyme_hdf5_util_read_group_attr_test() result(failed)
   use rhyme_hdf5_util_factory
   use rhyme_assertion

   implicit none

   character(len=1024), parameter :: testfile = "./test_hdf5_util_read_group_attr.h5"

   type(assertion_t) :: h5_tester

   type(hdf5_util_t) :: h5

   integer :: val_int
   real(kind=4) :: val_real
   real(kind=8) :: val_real8
   character(len=128) :: val_char

   h5_tester = .describe."read_group_attr"

   h5 = hdf5_util_factory_generate('empty')

   call rhyme_hdf5_util_create(h5, testfile)
   call rhyme_hdf5_util_write_group_attr(h5, "/", "int", 1)
   call rhyme_hdf5_util_write_group_attr(h5, "/", "real", 2.34e0)
   call rhyme_hdf5_util_write_group_attr(h5, "/", "real8", 3.45d0)
   call rhyme_hdf5_util_write_group_attr(h5, "/", "char", "Hello world!")
   call rhyme_hdf5_util_close(h5)

   call rhyme_hdf5_util_open(h5, testfile)

   call rhyme_hdf5_util_read_group_attr(h5, "/", "int", val_int)
   call rhyme_hdf5_util_read_group_attr(h5, "/", "real", val_real)
   call rhyme_hdf5_util_read_group_attr(h5, "/", "real8", val_real8)
   call rhyme_hdf5_util_read_group_attr(h5, "/", "char", val_char)

   call h5_tester%expect(val_int.toBe.1)
   call h5_tester%expect(val_real.toBe.2.34e0)
   call h5_tester%expect(val_real8.toBe.3.45d0)
   call h5_tester%expect(val_char.toBe.'Hello world!')

   failed = h5_tester%failed()
end function rhyme_hdf5_util_read_group_attr_test
