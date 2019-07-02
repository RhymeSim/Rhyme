module rhyme_hdf5_util
  ! TODO: Split module into multiple files

  use hdf5

  implicit none

  type, private :: hdf5_util_indices_t
    integer :: unset = -1
  end type hdf5_util_indices_t

  type ( hdf5_util_indices_t ), parameter :: h5id = hdf5_util_indices_t ()

  type hdf5_util_t
    character ( len=1024 ) :: filename
    integer ( hid_t ) :: fid = h5id%unset
    logical :: initialized = .false.
  end type hdf5_util_t

  interface
    module subroutine rhyme_hdf5_util_open ( h5, path )
      type ( hdf5_util_t ), intent ( inout ) :: h5
      character ( len=* ), intent ( in ) :: path
    end subroutine rhyme_hdf5_util_open

    module subroutine rhyme_hdf5_util_create ( h5, filename )
      type ( hdf5_util_t ), intent ( inout ) :: h5
      character ( len=* ), intent ( in ) :: filename
    end subroutine rhyme_hdf5_util_create

    module subroutine rhyme_hdf5_util_create_group ( h5, where, group_id )
      type ( hdf5_util_t ), intent (in) :: h5
      character ( len=* ), intent (in) :: where
      integer ( hid_t ), intent (out) :: group_id
    end subroutine rhyme_hdf5_util_create_group

    module subroutine rhyme_hdf5_util_write_group_attr ( h5, where, key, value )
      type ( hdf5_util_t ), intent ( in ) :: h5
      character ( len=* ), intent(in) :: where, key
      class (*), intent ( in ) :: value
    end subroutine rhyme_hdf5_util_write_group_attr

    module subroutine rhyme_hdf5_util_write_group_1d_array_attr ( h5, where, key, array )
      type ( hdf5_util_t ), intent ( in ) :: h5
      character ( len=* ), intent(in) :: where, key
      class (*), dimension(:), intent ( in ) :: array
    end subroutine rhyme_hdf5_util_write_group_1d_array_attr

    module subroutine rhyme_hdf5_util_write_group_comp_1d_array_attr ( &
      h5, where, key, keys, values )
      type ( hdf5_util_t ), intent ( inout ) :: h5
      class (*), intent ( in ) :: where
      character ( len=* ), intent ( in ) :: key
      character ( len=* ), dimension(:), intent ( in ) :: keys
      class (*), dimension(:) :: values
    end subroutine rhyme_hdf5_util_write_group_comp_1d_array_attr

    module subroutine rhyme_hdf5_util_write_table ( h5, where, key, headers, values )
      type ( hdf5_util_t ), intent(inout) :: h5
      class (*), intent( in ) :: where
      character (len=*), intent(in) :: key
      character (len=*), dimension(:), intent(in) :: headers
      class (*), dimension(:,:) :: values ! [ column, row ]
    end subroutine rhyme_hdf5_util_write_table

    module subroutine rhyme_hdf5_util_read_table ( h5, where, key, headers, buffer )
      type ( hdf5_util_t ), intent ( in ) :: h5
      character ( len=* ), intent ( in ) :: where, key
      character ( len=8 ), intent ( in ) :: headers(:)
      class (*), dimension(:,:), intent ( out ) :: buffer
    end subroutine rhyme_hdf5_util_read_table

    module function rhyme_hdf5_util_get_table_size ( h5, where ) result ( table_size )
      type ( hdf5_util_t ), intent ( in ) :: h5
      character ( len=* ), intent ( in ) :: where
      integer :: table_size
    end function rhyme_hdf5_util_get_table_size

    module subroutine rhyme_hdf5_util_read_group_attr ( h5, where, key, value )
      type ( hdf5_util_t ), intent ( in ) :: h5
      class (*), intent ( in ) :: where
      character (len=*), intent( in ) :: key
      class(*), intent ( out ) :: value
    end subroutine rhyme_hdf5_util_read_group_attr

    module subroutine rhyme_hdf5_util_read_group_1d_array_attr ( h5, where, key, array )
      type ( hdf5_util_t ), intent ( in ) :: h5
      character ( len=* ), intent(in) :: where, key
      class (*), dimension(:), intent ( out ) :: array
    end subroutine rhyme_hdf5_util_read_group_1d_array_attr

    module subroutine rhyme_hdf5_util_read_group_comp_1d_array_attr ( h5, where, key, headers, buffer )
      type ( hdf5_util_t ), intent ( in ) :: h5
      character ( len=* ), intent ( in ) :: where, key
      character (len=*), dimension(:), intent(in) :: headers
      class (*), intent ( out ) :: buffer(:)
    end subroutine rhyme_hdf5_util_read_group_comp_1d_array_attr

    module subroutine rhyme_hdf5_util_write_1d_dataset ( h5, where, key, data )
      type ( hdf5_util_t ), intent ( inout ) :: h5
      class (*), intent ( in ) :: where
      character ( len=* ), intent ( in ) :: key
      class (*), dimension(:), intent ( in ) :: data
    end subroutine rhyme_hdf5_util_write_1d_dataset

    module subroutine rhyme_hdf5_util_write_2d_dataset ( h5, where, key, data )
      type ( hdf5_util_t ), intent ( inout ) :: h5
      class (*), intent ( in ) :: where
      character ( len=* ), intent ( in ) :: key
      class (*), dimension(:,:), intent ( in ) :: data
    end subroutine rhyme_hdf5_util_write_2d_dataset

    module subroutine rhyme_hdf5_util_read_1d_dataset ( h5, where, data )
      type ( hdf5_util_t ), intent ( in ) :: h5
      character ( len=* ), intent ( in ) :: where
      class (*), dimension(:), intent ( out ) :: data
    end subroutine rhyme_hdf5_util_read_1d_dataset

    module subroutine rhyme_hdf5_util_read_2d_dataset ( h5, where, data )
      type ( hdf5_util_t ), intent ( in ) :: h5
      character ( len=* ), intent ( in ) :: where
      class (*), dimension(:,:), intent ( out ) :: data
    end subroutine rhyme_hdf5_util_read_2d_dataset

    module subroutine rhyme_hdf5_util_close ( h5 )
      type ( hdf5_util_t ), intent ( inout ) :: h5
    end subroutine rhyme_hdf5_util_close
  end interface

end module rhyme_hdf5_util
