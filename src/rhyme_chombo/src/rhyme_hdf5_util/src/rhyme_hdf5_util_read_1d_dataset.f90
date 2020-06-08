submodule(rhyme_hdf5_util) read_1d_dataset_smod
contains
module subroutine rhyme_hdf5_util_read_1d_dataset(h5, where, data)
   implicit none

   type(hdf5_util_t), intent(in) :: h5
   character(len=*), intent(in) :: where
   class(*), dimension(:), intent(out) :: data

   integer(hid_t) :: dset_id
   integer(kind=hsize_t) :: dims(1)
   integer :: hdferr

   dims(1) = int(size(data), kind=hsize_t)

   call h5dopen_f(h5%fid, trim(where), dset_id, hdferr)

   select type (d => data)
   type is (integer)
      call h5dread_f(dset_id, H5T_NATIVE_INTEGER, d, dims, hdferr)
   type is (real(kind=4))
      call h5dread_f(dset_id, H5T_NATIVE_REAL, d, dims, hdferr)
   type is (real(kind=8))
      call h5dread_f(dset_id, H5T_NATIVE_DOUBLE, d, dims, hdferr)
   end select

   call h5dclose_f(dset_id, hdferr)
end subroutine rhyme_hdf5_util_read_1d_dataset
end submodule read_1d_dataset_smod
