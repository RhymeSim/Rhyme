submodule(rhyme_hdf5_util) get_table_size_smod
contains
   module function rhyme_hdf5_util_get_table_size(h5, where) result(table_size)
      implicit none

      type(hdf5_util_t), intent(in) :: h5
      character(len=*), intent(in) :: where
      integer :: table_size

      integer(kind=hsize_t) :: tsize(1)
      integer(kind=hsize_t) :: max_tsize(1)
      integer(kind=hid_t) :: dset_id, space_id
      integer :: hdferr

      call h5dopen_f(h5%fid, trim(where), dset_id, hdferr)
      call h5dget_space_f(dset_id, space_id, hdferr)

      call h5sget_simple_extent_dims_f(space_id, tsize, max_tsize, hdferr)
      table_size = int(tsize(1))

      call h5dclose_f(dset_id, hdferr)
   end function rhyme_hdf5_util_get_table_size
end submodule get_table_size_smod
