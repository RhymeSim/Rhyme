submodule(rhyme_hdf5_util) write_group_comp_1d_array_attr_smod
contains
module subroutine rhyme_hdf5_util_write_group_comp_1d_array_attr( &
   h5, where, key, keys, values)
   implicit none

   type(hdf5_util_t), intent(inout) :: h5
   class(*), intent(in) :: where
   character(len=*), intent(in) :: key
   character(len=*), dimension(:), intent(in) :: keys
   class(*), dimension(:) :: values

   integer(hid_t) :: group_id
   integer :: hdferr

   if (.not. h5%initialized) return

   select type (w => where)
   type is (character(*))
      call h5gopen_f(h5%fid, trim(w), group_id, hdferr)
   type is (integer(hid_t))
      group_id = w
   end select

   select type (vals => values)
   type is (integer)
      call write_comp_array_attr(H5T_NATIVE_INTEGER)
   type is (real(kind=4))
      call write_comp_array_attr(H5T_NATIVE_REAL)
   type is (real(kind=8))
      call write_comp_array_attr(H5T_NATIVE_DOUBLE)
   end select

   select type (w => where)
   type is (character(*))
      call h5gclose_f(group_id, hdferr)
   end select

contains
   subroutine write_comp_array_attr(type_id)
      implicit none

      integer(hid_t) :: type_id

      integer(hid_t) :: space_id, attr_id, comp_id
      integer(size_t) :: element_size, tot_size, offset
      integer(hsize_t) :: dims(1) = 1
      integer :: i

      call h5tget_size_f(type_id, element_size, hdferr)
      tot_size = size(keys)*element_size
      call h5tcreate_f(H5T_COMPOUND_F, tot_size, comp_id, hdferr)

      offset = 0
      do i = 1, size(keys)
         call h5tinsert_f(comp_id, trim(keys(i)), offset, type_id, hdferr)
         offset = offset + element_size
      end do

      call h5screate_simple_f(1, dims, space_id, hdferr)
      call h5acreate_f(group_id, trim(key), comp_id, space_id, attr_id, hdferr)

      select type (vals => values)
      type is (integer)
         call h5awrite_f(attr_id, comp_id, vals, dims, hdferr)
      type is (real(kind=4))
         call h5awrite_f(attr_id, comp_id, vals, dims, hdferr)
      type is (real(kind=8))
         call h5awrite_f(attr_id, comp_id, vals, dims, hdferr)
      end select

      call h5aclose_f(attr_id, hdferr)
      call h5tclose_f(comp_id, hdferr)
   end subroutine write_comp_array_attr
end subroutine rhyme_hdf5_util_write_group_comp_1d_array_attr
end submodule write_group_comp_1d_array_attr_smod
