submodule(rhyme_hdf5_util) read_group_comp_1d_array_attr_smod
contains
module subroutine rhyme_hdf5_util_read_group_comp_1d_array_attr(h5, where, key, headers, buffer)
   implicit none

   type(hdf5_util_t), intent(in) :: h5
   character(len=*), intent(in) :: where, key
   character(len=*), dimension(:), intent(in) :: headers
   class(*), intent(out) :: buffer(:)

   integer(hid_t) :: group_id, attr_id, comp_id, type_id
   integer(size_t) :: element_size, tot_size, offset
   integer(hsize_t) :: dims(1) = 1
   integer :: hdferr, i

   call h5gopen_f(h5%fid, trim(where), group_id, hdferr)

   select type (buf => buffer)
   type is (integer)
      call h5tcopy_f(H5T_NATIVE_INTEGER, type_id, hdferr)
   type is (real(kind=4))
      call h5tcopy_f(H5T_NATIVE_REAL, type_id, hdferr)
   type is (real(kind=8))
      call h5tcopy_f(H5T_NATIVE_DOUBLE, type_id, hdferr)
   end select

   call h5aopen_f(group_id, trim(key), attr_id, hdferr)

   call h5tget_size_f(type_id, element_size, hdferr)
   tot_size = size(buffer)*element_size
   call h5tcreate_f(H5T_COMPOUND_F, tot_size, comp_id, hdferr)

   offset = 0
   do i = 1, size(buffer)
      call h5tinsert_f(comp_id, trim(headers(i)), offset, type_id, hdferr)
      offset = offset + element_size
   end do

   dims = int(size(buffer), kind=hsize_t)

   select type (buf => buffer)
   type is (integer)
      call h5aread_f(attr_id, comp_id, buf, dims, hdferr)
   type is (real(kind=4))
      call h5aread_f(attr_id, comp_id, buf, dims, hdferr)
   type is (real(kind=8))
      call h5aread_f(attr_id, comp_id, buf, dims, hdferr)
   end select

   call h5tclose_f(comp_id, hdferr)
   call h5aclose_f(attr_id, hdferr)
   call h5gclose_f(group_id, hdferr)
end subroutine rhyme_hdf5_util_read_group_comp_1d_array_attr
end submodule read_group_comp_1d_array_attr_smod
