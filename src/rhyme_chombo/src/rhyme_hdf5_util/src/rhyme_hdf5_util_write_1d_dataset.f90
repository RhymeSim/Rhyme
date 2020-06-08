submodule(rhyme_hdf5_util) write_1d_dataset_smod
contains
module subroutine rhyme_hdf5_util_write_1d_dataset(h5, where, key, data)
   implicit none

   type(hdf5_util_t), intent(inout) :: h5
   class(*), intent(in) :: where
   character(len=*), intent(in) :: key
   class(*), dimension(:), intent(in) :: data

   integer(hid_t) :: group_id, dsetid, space_id
   integer(hsize_t) :: dims(1)
   integer :: hdferr

   select type (w => where)
   type is (character(*))
      call h5gopen_f(h5%fid, trim(w), group_id, hdferr)
   type is (integer(hid_t))
      group_id = w
   end select

   dims = int(shape(data), kind=hsize_t)

   call h5screate_simple_f(1, dims, space_id, hdferr)

   select type (d => data)
   type is (integer)
      call h5dcreate_f(group_id, key, H5T_NATIVE_INTEGER, space_id, dsetid, hdferr)
      call h5dwrite_f(dsetid, H5T_NATIVE_INTEGER, d, dims, hdferr)
   type is (real(kind=4))
      call h5dcreate_f(group_id, key, H5T_NATIVE_REAL, space_id, dsetid, hdferr)
      call h5dwrite_f(dsetid, H5T_NATIVE_REAL, d, dims, hdferr)
   type is (real(kind=8))
      call h5dcreate_f(group_id, key, H5T_NATIVE_DOUBLE, space_id, dsetid, hdferr)
      call h5dwrite_f(dsetid, H5T_NATIVE_DOUBLE, d, dims, hdferr)
   end select

   call h5dclose_f(dsetid, hdferr)
   call h5sclose_f(space_id, hdferr)

   select type (w => where)
   type is (character(*))
      call h5gclose_f(group_id, hdferr)
   end select
end subroutine rhyme_hdf5_util_write_1d_dataset
end submodule write_1d_dataset_smod
