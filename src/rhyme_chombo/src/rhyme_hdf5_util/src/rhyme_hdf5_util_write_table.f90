submodule(rhyme_hdf5_util) write_table_smod
contains
module subroutine rhyme_hdf5_util_write_table(h5, where, key, headers, values)
   implicit none

   type(hdf5_util_t), intent(inout) :: h5
   class(*), intent(in) :: where
   character(len=*), intent(in) :: key
   character(len=*), dimension(:), intent(in) :: headers
   class(*), dimension(:, :) :: values ! [ column, row ]

   integer(hid_t) :: group_id
   integer :: hdferr

   select type (w => where)
   type is (character(*))
      call h5gopen_f(h5%fid, trim(w), group_id, hdferr)
   type is (integer(hid_t))
      group_id = w
   end select

   select type (vals => values)
   type is (integer)
      call write_table(H5T_NATIVE_INTEGER)
   type is (real(kind=4))
      call write_table(H5T_NATIVE_REAL)
   type is (real(kind=8))
      call write_table(H5T_NATIVE_DOUBLE)
   end select

   select type (w => where)
   type is (character(*))
      call h5gclose_f(group_id, hdferr)
   end select

contains
   subroutine write_table(type_id)
      implicit none

      integer(hid_t) :: type_id

      integer(hid_t) :: space_id, dset_id, table_id
      integer(size_t) :: type_size, row_size, offset
      integer(hsize_t) :: dims(1)
      integer :: i

      dims = int(size(values, 2), kind=hsize_t)

      call h5tget_size_f(type_id, type_size, hdferr)
      row_size = size(headers)*type_size
      call h5tcreate_f(H5T_COMPOUND_F, row_size, table_id, hdferr)

      offset = 0
      do i = 1, size(headers)
         call h5tinsert_f(table_id, trim(headers(i)), offset, type_id, hdferr)
         offset = offset + type_size
      end do

      call h5screate_simple_f(1, dims, space_id, hdferr)
      call h5dcreate_f(group_id, trim(key), table_id, space_id, dset_id, hdferr)

      select type (vals => values)
      type is (integer)
         call h5dwrite_f(dset_id, table_id, vals, dims, hdferr)
      type is (real(kind=4))
         call h5dwrite_f(dset_id, table_id, vals, dims, hdferr)
      type is (real(kind=8))
         call h5dwrite_f(dset_id, table_id, vals, dims, hdferr)
      end select

      call h5dclose_f(dset_id, hdferr)
      call h5tclose_f(table_id, hdferr)
      call h5sclose_f(space_id, hdferr)
   end subroutine write_table
end subroutine rhyme_hdf5_util_write_table
end submodule write_table_smod
