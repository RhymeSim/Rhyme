submodule(rhyme_hdf5_util) write_group_attr_smod
contains
module subroutine rhyme_hdf5_util_write_group_attr(h5, where, key, value)
   implicit none

   type(hdf5_util_t), intent(in) :: h5
   character(len=*), intent(in) :: where, key
   class(*), intent(in) :: value

   integer(hid_t) :: group_id
   integer :: hdferr

   if (.not. h5%initialized) return

   call h5gopen_f(h5%fid, trim(where), group_id, hdferr)

   select type (val => value)
   type is (integer)
      call write_group_attr(H5T_NATIVE_INTEGER, 1)
   type is (real(kind=4))
      call write_group_attr(H5T_NATIVE_REAL, 1)
   type is (real(kind=8))
      call write_group_attr(H5T_NATIVE_DOUBLE, 1)
   type is (character(*))
      call write_group_attr(H5T_NATIVE_CHARACTER, len_trim(val))
   end select

   call h5gclose_f(group_id, hdferr)

contains
   subroutine write_group_attr(type_id, len)
      implicit none

      integer(hid_t) :: type_id
      integer :: len

      integer(hsize_t) :: dims(1) = 1
      integer(hid_t) :: attr_id, space_id, dtype

      call h5screate_f(H5S_SCALAR_F, space_id, hdferr)

      call h5tcopy_f(type_id, dtype, hdferr)
      if (len .ne. 1) call h5tset_size_f(dtype, int(len, kind=size_t), hdferr)

      call h5acreate_f(group_id, key, dtype, space_id, attr_id, hdferr)

      select type (val => value)
      type is (integer)
         call h5awrite_f(attr_id, dtype, val, dims, hdferr)
      type is (real(kind=4))
         call h5awrite_f(attr_id, dtype, val, dims, hdferr)
      type is (real(kind=8))
         call h5awrite_f(attr_id, dtype, val, dims, hdferr)
      type is (character(*))
         call h5awrite_f(attr_id, dtype, val, dims, hdferr)
      end select

      call h5aclose_f(attr_id, hdferr)
      call h5sclose_f(space_id, hdferr)
   end subroutine write_group_attr
end subroutine rhyme_hdf5_util_write_group_attr
end submodule write_group_attr_smod
