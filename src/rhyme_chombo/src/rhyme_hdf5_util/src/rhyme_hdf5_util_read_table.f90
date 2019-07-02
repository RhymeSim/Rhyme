submodule ( rhyme_hdf5_util ) read_table_smod
contains
  module subroutine rhyme_hdf5_util_read_table ( h5, where, key, headers, buffer )
    implicit none

    type ( hdf5_util_t ), intent ( in ) :: h5
    character ( len=* ), intent ( in ) :: where, key
    character ( len=8 ), intent ( in ) :: headers(:)
    class (*), dimension(:,:), intent ( out ) :: buffer

    integer ( kind=hid_t ) :: type_id, table_id, group_id, dset_id
    integer ( kind=hsize_t ) :: type_size, row_size, offset, dims(2)
    integer :: h, hdferr

    call h5gopen_f ( h5%fid, trim(where), group_id, hdferr )
    call h5dopen_f ( group_id, trim(key), dset_id, hdferr )

    select type ( buf => buffer )
    type is ( integer )
      call h5tcopy_f ( H5T_NATIVE_INTEGER, type_id, hdferr )
    type is ( real( kind=4 ) )
      call h5tcopy_f ( H5T_NATIVE_REAL, type_id, hdferr )
    type is ( real( kind=8 ) )
      call h5tcopy_f ( H5T_NATIVE_DOUBLE, type_id, hdferr )
    end select


    call h5tget_size_f ( type_id, type_size, hdferr )
    row_size = size( headers ) * type_size
    call h5tcreate_f ( H5T_COMPOUND_F, row_size, table_id, hdferr )

    offset = 0
    do h = 1, size( headers )
      call h5tinsert_f ( table_id, trim( headers(h) ), offset, type_id, hdferr )
      offset = offset + type_size
    end do

    dims = int( size( buffer ), kind=hsize_t )

    select type ( buf => buffer )
    type is ( integer )
      call h5dread_f ( dset_id, table_id, buf, dims, hdferr)
    type is ( real( kind=4 ) )
      call h5dread_f ( dset_id, table_id, buf, dims, hdferr)
    type is ( real( kind=8 ) )
      call h5dread_f ( dset_id, table_id, buf, dims, hdferr)
    end select

    call h5tclose_f ( table_id, hdferr )

    call h5dclose_f ( dset_id, hdferr )
    call h5gclose_f ( group_id, hdferr )
  end subroutine rhyme_hdf5_util_read_table
end submodule read_table_smod
