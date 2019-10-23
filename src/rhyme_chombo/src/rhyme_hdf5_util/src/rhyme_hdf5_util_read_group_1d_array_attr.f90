submodule ( rhyme_hdf5_util ) read_group_1d_array_attr_smod
contains
  module subroutine rhyme_hdf5_util_read_group_1d_array_attr ( h5, where, key, array )
    implicit none

    type ( hdf5_util_t ), intent ( in ) :: h5
    character ( len=* ), intent ( in ) :: where, key
    class (*), dimension(:), intent ( out ) :: array

    integer ( hid_t ) :: group_id, attr_id
    integer :: hdferr

    integer ( hsize_t ) :: dims(1)

    call h5gopen_f( h5%fid, trim(where), group_id, hdferr )
    call h5aopen_f( group_id, trim(key), attr_id, hdferr )

    dims(1) = size( array )

    select type ( arr => array )
    type is ( integer )
      call h5aread_f( attr_id, H5T_NATIVE_INTEGER, arr, dims, hdferr )
    type is ( real ( kind=4 ) )
      call h5aread_f( attr_id, H5T_NATIVE_REAL, arr, dims, hdferr )
    type is ( real ( kind=8 ) )
      call h5aread_f( attr_id, H5T_NATIVE_DOUBLE, arr, dims, hdferr )
    end select

    call h5aclose_f( attr_id, hdferr )
    call h5gclose_f( group_id, hdferr )
  end subroutine rhyme_hdf5_util_read_group_1d_array_attr
end submodule read_group_1d_array_attr_smod
