submodule(rhyme_hdf5_util) read_group_attr_smod
contains
module subroutine rhyme_hdf5_util_read_group_attr(h5, where, key, value)
   implicit none

   type(hdf5_util_t), intent(in) :: h5
   class(*), intent(in) :: where
   character(len=*), intent(in) :: key
   class(*), intent(out) :: value

   integer(hid_t) :: group_id, attr_id, dtype
   integer(hsize_t) :: dims(1) = 1
   integer :: hdferr

   select type (w => where)
   type is (character(*))
      call h5gopen_f(h5%fid, trim(w), group_id, hdferr)
   type is (integer(hid_t))
      group_id = w
   end select

   call h5aopen_f(group_id, trim(key), attr_id, hdferr)

   select type (val => value)
   type is (integer)
      call h5aread_f(attr_id, H5T_NATIVE_INTEGER, val, dims, hdferr)
   type is (real(kind=4))
      call h5aread_f(attr_id, H5T_NATIVE_REAL, val, dims, hdferr)
   type is (real(kind=8))
      call h5aread_f(attr_id, H5T_NATIVE_DOUBLE, val, dims, hdferr)
   type is (character(*))
      call h5tcopy_f(H5T_NATIVE_CHARACTER, dtype, hdferr)
      call h5tset_size_f(dtype, int(len_trim(val), kind=size_t), hdferr)
      call h5aread_f(attr_id, dtype, val, dims, hdferr)
   end select

   call h5aclose_f(attr_id, hdferr)

   select type (w => where)
   type is (character(*))
      call h5gclose_f(group_id, hdferr)
   end select
end subroutine rhyme_hdf5_util_read_group_attr
end submodule read_group_attr_smod
