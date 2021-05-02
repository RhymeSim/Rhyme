logical function rhyme_hdf5_util_close_test() result(failed)
   use rhyme_hdf5_util_factory
   use rhyme_assertion

   implicit none

   type(assertion_t) :: h5_tester

   character(len=1024), parameter :: testfile = "./test_hdf5_util_close.h5"

   type(hdf5_util_t) :: h5

   h5_tester = .describe."close"

   h5 = hdf5_util_factory_generate('empty')

   call rhyme_hdf5_util_create(h5, testfile)
   call rhyme_hdf5_util_close(h5)

   call h5_tester%expect(int(h5%fid) .toBe.h5id%unset)
   call h5_tester%expect(h5%filename.toBe."")
   call h5_tester%expect(h5%initialized.toBe..false.)

   failed = h5_tester%failed()
end function rhyme_hdf5_util_close_test
