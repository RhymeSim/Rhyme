submodule(rhyme_hdf5_util) write_group_1d_array_attr_smod
contains
   module subroutine rhyme_hdf5_util_write_group_1d_array_attr(h5, where, key, array)
      implicit none

      type(hdf5_util_t), intent(in) :: h5
      character(len=*), intent(in) :: where, key
      class(*), dimension(:), intent(in) :: array

      integer(hid_t) :: group_id, attr_id, space_id
      integer :: hdferr

      integer(hsize_t) :: dims(1)

      if (.not. h5%initialized) return

      call h5gopen_f(h5%fid, trim(where), group_id, hdferr)

      dims(1) = size(array)
      call h5screate_simple_f(1, dims, space_id, hdferr)

      select type (arr => array)
      type is (integer)
         call h5acreate_f(group_id, trim(key), H5T_NATIVE_INTEGER, space_id, attr_id, hdferr)
         call h5awrite_f(attr_id, H5T_NATIVE_INTEGER, arr, dims, hdferr)
      type is (real(kind=4))
         call h5acreate_f(group_id, trim(key), H5T_NATIVE_REAL, space_id, attr_id, hdferr)
         call h5awrite_f(attr_id, H5T_NATIVE_REAL, arr, dims, hdferr)
      type is (real(kind=8))
         call h5acreate_f(group_id, trim(key), H5T_NATIVE_DOUBLE, space_id, attr_id, hdferr)
         call h5awrite_f(attr_id, H5T_NATIVE_DOUBLE, arr, dims, hdferr)
      end select

      call h5aclose_f(attr_id, hdferr)
      call h5sclose_f(space_id, hdferr)
      call h5gclose_f(group_id, hdferr)
   end subroutine rhyme_hdf5_util_write_group_1d_array_attr
end submodule write_group_1d_array_attr_smod
