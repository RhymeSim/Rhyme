module rhyme_hdf5_util
  ! TODO: Split module into multiple files

  use hdf5

  implicit none

  type rhyme_hdf5_util_indices_t
    integer :: unset = -1
  end type rhyme_hdf5_util_indices_t

  type ( rhyme_hdf5_util_indices_t ), parameter :: h5id = rhyme_hdf5_util_indices_t ()

  type rhyme_hdf5_util_t
    character ( len=1024 ) :: filename
    integer ( hid_t ) :: fid = h5id%unset
    logical :: initialized = .false.
  contains
    procedure :: open => rhyme_hdf5_util_open
    procedure :: create => rhyme_hdf5_util_create
    procedure :: create_group => rhyme_hdf5_util_create_group
    procedure :: write_group_attr => rhyme_hdf5_util_write_group_attribute
    procedure :: write_group_1d_array_attr => rhyme_hdf5_util_write_group_1d_array_attribute
    procedure :: write_group_comp_1d_array_attr => rhyme_hdf5_util_write_group_compound_1d_array_attribute
    procedure :: write_table => rhyme_hdf5_util_write_table
    procedure :: read_table => rhyme_hdf5_util_read_table
    procedure :: read_group_attr => rhyme_hdf5_util_read_group_attribute
    procedure :: read_group_1d_array_attr => rhyme_hdf5_util_read_group_1d_array_attribute
    procedure :: write_1d_dataset => rhyme_hdf5_util_write_1d_dataset
    procedure :: write_2d_dataset => rhyme_hdf5_util_write_2d_dataset
    procedure :: read_1d_dataset => rhyme_hdf5_util_read_1d_dataset
    procedure :: read_2d_dataset => rhyme_hdf5_util_read_2d_dataset
    procedure :: close => rhyme_hdf5_util_close
  end type rhyme_hdf5_util_t

contains

  subroutine rhyme_hdf5_util_create ( this, filename )
    implicit none

    class ( rhyme_hdf5_util_t ), intent ( inout ) :: this
    character ( len=* ), intent ( in ) :: filename

    integer :: hdferr


    if ( this%initialized ) return

    call h5open_f ( hdferr )
    call h5fcreate_f ( trim(filename), H5F_ACC_TRUNC_F, this%fid, hdferr )

    this%filename = trim ( filename )
    this%initialized = .true.
  end subroutine rhyme_hdf5_util_create


  subroutine rhyme_hdf5_util_create_group ( this, where, group_id )
    ! NB: groups created by this subroutine must be closed accordingly
    implicit none

    class ( rhyme_hdf5_util_t ), intent (in) :: this
    character ( len=* ), intent (in) :: where
    integer ( hid_t ), intent (out) :: group_id

    integer :: hdferr

    if ( .not. this%initialized ) return

    call h5gcreate_f ( this%fid, trim(where), group_id, hdferr )
  end subroutine rhyme_hdf5_util_create_group


  subroutine rhyme_hdf5_util_write_group_attribute ( this, where, key, value )
    implicit none

    class ( rhyme_hdf5_util_t ), intent ( in ) :: this
    character ( len=* ), intent(in) :: where, key
    class (*), intent ( in ) :: value

    integer ( hid_t ) :: group_id
    integer :: hdferr


    if ( .not. this%initialized ) return

    call h5gopen_f ( this%fid, trim(where), group_id, hdferr )

    select type ( val => value )
    type is ( integer )
      call write_group_attribute ( H5T_NATIVE_INTEGER, 1 )
    type is ( real ( kind=4 ) )
      call write_group_attribute ( H5T_NATIVE_REAL, 1 )
    type is ( real ( kind=8 ) )
      call write_group_attribute ( H5T_NATIVE_DOUBLE, 1 )
    type is ( character (*) )
      call write_group_attribute ( H5T_NATIVE_CHARACTER, len_trim(val) )
    end select

    call h5gclose_f ( group_id, hdferr )

  contains
    subroutine write_group_attribute ( type_id, len )
      implicit none

      integer ( hid_t ) :: type_id
      integer :: len

      integer ( hsize_t ) :: dims(1) = 1
      integer ( hid_t ) :: attr_id, space_id, dtype


      call h5screate_simple_f ( 1, dims, space_id, hdferr )

      call h5tcopy_f ( type_id, dtype, hdferr )
      if ( len .ne. 1 ) call h5tset_size_f ( dtype, int(len, kind=size_t), hdferr )

      call h5acreate_f ( group_id, key, dtype, space_id, attr_id, hdferr )

      select type ( val => value )
      type is ( integer )
        call h5awrite_f ( attr_id, dtype, val, dims, hdferr )
      type is ( real ( kind=4 ) )
        call h5awrite_f ( attr_id, dtype, val, dims, hdferr )
      type is ( real ( kind=8 ) )
        call h5awrite_f ( attr_id, dtype, val, dims, hdferr )
      type is ( character (*) )
        call h5awrite_f ( attr_id, dtype, val, dims, hdferr )
      end select

      call h5aclose_f ( attr_id, hdferr )
      call h5sclose_f ( space_id, hdferr )
    end subroutine write_group_attribute
  end subroutine rhyme_hdf5_util_write_group_attribute


  subroutine rhyme_hdf5_util_write_group_1d_array_attribute ( this, where, key, array )
    implicit none

    class ( rhyme_hdf5_util_t ), intent ( in ) :: this
    character ( len=* ), intent(in) :: where, key
    class (*), dimension(:), intent ( in ) :: array

    integer ( hid_t ) :: group_id, attr_id, space_id
    integer :: hdferr

    integer ( hsize_t ) :: dims(1)


    if ( .not. this%initialized ) return

    call h5gopen_f ( this%fid, trim(where), group_id, hdferr )

    dims(1) = size ( array )
    call h5screate_simple_f ( 1, dims, space_id, hdferr )

    select type ( arr => array )
    type is ( integer )
      call h5acreate_f ( group_id, trim(key), H5T_NATIVE_INTEGER, space_id, attr_id, hdferr )
      call h5awrite_f ( attr_id, H5T_NATIVE_INTEGER, arr, dims, hdferr )
    type is ( real ( kind=4 ) )
      call h5acreate_f ( group_id, trim(key), H5T_NATIVE_REAL, space_id, attr_id, hdferr )
      call h5awrite_f ( attr_id, H5T_NATIVE_REAL, arr, dims, hdferr )
    type is ( real ( kind=8 ) )
      call h5acreate_f ( group_id, trim(key), H5T_NATIVE_DOUBLE, space_id, attr_id, hdferr )
      call h5awrite_f ( attr_id, H5T_NATIVE_DOUBLE, arr, dims, hdferr )
    end select

    call h5aclose_f ( attr_id, hdferr )
    call h5sclose_f ( space_id, hdferr )
    call h5gclose_f ( group_id, hdferr )
  end subroutine rhyme_hdf5_util_write_group_1d_array_attribute


  subroutine rhyme_hdf5_util_read_group_1d_array_attribute ( this, where, key, array )
    implicit none

    class ( rhyme_hdf5_util_t ), intent ( in ) :: this
    character ( len=* ), intent(in) :: where, key
    class (*), dimension(:), intent ( out ) :: array

    integer ( hid_t ) :: group_id, attr_id
    integer :: hdferr

    integer ( hsize_t ) :: dims(1)

    call h5gopen_f ( this%fid, trim(where), group_id, hdferr )
    call h5aopen_f ( this%fid, trim(key), attr_id, hdferr )

    dims(1) = size ( array )

    select type ( arr => array )
    type is ( integer )
      call h5aread_f ( attr_id, H5T_NATIVE_INTEGER, arr, dims, hdferr )
    type is ( real ( kind=4 ) )
      call h5aread_f ( attr_id, H5T_NATIVE_REAL, arr, dims, hdferr )
    type is ( real ( kind=8 ) )
      call h5aread_f ( attr_id, H5T_NATIVE_DOUBLE, arr, dims, hdferr )
    end select

    call h5aclose_f ( attr_id, hdferr )
    call h5gclose_f ( group_id, hdferr )
  end subroutine rhyme_hdf5_util_read_group_1d_array_attribute


  subroutine rhyme_hdf5_util_write_group_compound_1d_array_attribute ( this, where, key, keys, values )
    implicit none

    class ( rhyme_hdf5_util_t ), intent(inout) :: this
    class (*), intent( in ) :: where
    character (len=*), intent(in) :: key
    character (len=*), dimension(:), intent(in) :: keys
    class (*), dimension(:) :: values

    integer ( hid_t ) :: group_id
    integer :: hdferr


    if ( .not. this%initialized ) return

    select type ( w => where )
    type is ( character (*) )
      call h5gopen_f ( this%fid, trim(w), group_id, hdferr )
    type is ( integer ( hid_t ) )
      group_id = w
    end select

    select type ( vals => values)
    type is ( integer )
      call write_comp_array_attr ( H5T_NATIVE_INTEGER )
    type is ( real ( kind = 4 ) )
      call write_comp_array_attr ( H5T_NATIVE_REAL )
    type is ( real ( kind = 8 ) )
      call write_comp_array_attr ( H5T_NATIVE_DOUBLE )
    end select

    select type ( w => where )
    type is ( character (*) )
      call h5gclose_f ( group_id, hdferr )
    end select

  contains
    subroutine write_comp_array_attr ( type_id )
      implicit none

      integer ( hid_t ) :: type_id

      integer ( hid_t ) :: space_id, attr_id, comp_id
      integer ( size_t ) :: element_size, tot_size, offset
      integer ( hsize_t ) :: dims(1) = 1
      integer :: i

      call h5tget_size_f ( type_id, element_size, hdferr )
      tot_size = size ( keys ) * element_size
      call h5tcreate_f ( H5T_COMPOUND_F , tot_size, comp_id, hdferr )

      offset = 0
      do i = 1, size ( keys )
        call h5tinsert_f ( comp_id, trim ( keys(i) ), offset, type_id, hdferr )
        offset = offset + element_size
      end do

      call h5screate_simple_f ( 1, dims, space_id, hdferr )
      call h5acreate_f ( group_id, trim(key), comp_id, space_id, attr_id, hdferr )

      select type ( vals => values)
      type is ( integer )
        call h5awrite_f ( attr_id, comp_id, vals, dims, hdferr)
      type is ( real ( kind = 4 ))
        call h5awrite_f ( attr_id, comp_id, vals, dims, hdferr)
      type is ( real ( kind = 8 ))
        call h5awrite_f ( attr_id, comp_id, vals, dims, hdferr)
      end select

      call h5aclose_f ( attr_id, hdferr )
      call h5tclose_f ( comp_id, hdferr )
    end subroutine write_comp_array_attr
  end subroutine rhyme_hdf5_util_write_group_compound_1d_array_attribute


  subroutine rhyme_hdf5_util_write_table ( this, where, key, headers, values )
    implicit none

    class ( rhyme_hdf5_util_t ), intent(inout) :: this
    class (*), intent( in ) :: where
    character (len=*), intent(in) :: key
    character (len=*), dimension(:), intent(in) :: headers
    class (*), dimension(:,:) :: values ! [ column, row ]

    integer ( hid_t ) :: group_id
    integer :: hdferr


    select type ( w => where )
    type is ( character (*) )
      call h5gopen_f ( this%fid, trim(w), group_id, hdferr )
    type is ( integer ( hid_t ) )
      group_id = w
    end select

    select type ( vals => values)
    type is ( integer )
      call write_table ( H5T_NATIVE_INTEGER )
    type is ( real ( kind=4 ) )
      call write_table ( H5T_NATIVE_REAL )
    type is ( real ( kind=8 ) )
      call write_table ( H5T_NATIVE_DOUBLE )
    end select

    select type ( w => where )
    type is ( character (*) )
      call h5gclose_f ( group_id, hdferr )
    end select

  contains
    subroutine write_table ( type_id )
      implicit none

      integer ( hid_t ) :: type_id

      integer ( hid_t ) :: space_id, dset_id, table_id
      integer ( size_t ) :: type_size, row_size, offset
      integer ( hsize_t ) :: dims(1)
      integer :: i

      dims = int ( size( values, 2 ), kind=hsize_t )

      call h5tget_size_f ( type_id, type_size, hdferr )
      row_size = size( headers ) * type_size
      call h5tcreate_f ( H5T_COMPOUND_F, row_size, table_id, hdferr )


      offset = 0
      do i = 1, size( headers )
        call h5tinsert_f ( table_id, trim( headers(i) ), offset, type_id, hdferr )
        offset = offset + type_size
      end do


      call h5screate_simple_f ( 1, dims, space_id, hdferr )
      call h5dcreate_f ( group_id, trim(key), table_id, space_id, dset_id, hdferr )


      select type ( vals => values)
      type is ( integer )
        call h5dwrite_f ( dset_id, table_id, vals, dims, hdferr)
      type is ( real( kind=4 ) )
        call h5dwrite_f ( dset_id, table_id, vals, dims, hdferr)
      type is ( real( kind=8 ) )
        call h5dwrite_f ( dset_id, table_id, vals, dims, hdferr)
      end select

      call h5dclose_f ( dset_id, hdferr )
      call h5tclose_f ( table_id, hdferr )
      call h5sclose_f ( space_id, hdferr )
    end subroutine write_table
  end subroutine rhyme_hdf5_util_write_table


  subroutine rhyme_hdf5_util_read_table ( this, where, headers, buffer )
    implicit none

    class ( rhyme_hdf5_util_t ), intent ( in ) :: this
    class (*), intent ( in ) :: where
    character ( len=8 ), intent ( in ) :: headers(:)
    class (*), dimension(:,:), intent ( out ) :: buffer

    integer ( kind=hid_t ) :: type_id, table_id, dset_id
    integer ( kind=hsize_t ) :: type_size, row_size, offset, dims(2)
    integer :: h, hdferr


    select type ( w => where )
    type is ( character (*) )
      call h5dopen_f ( this%fid, trim(w), dset_id, hdferr )
    type is ( integer ( hid_t ) )
      dset_id = w
    end select


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

    select type ( w => where )
    type is ( character (*) )
      call h5dclose_f ( dset_id, hdferr )
    end select
  end subroutine rhyme_hdf5_util_read_table


  subroutine rhyme_hdf5_util_open ( this, path )
    implicit none

    class ( rhyme_hdf5_util_t ), intent (inout) :: this
    character ( len=* ), intent (in) :: path

    integer :: hdferr


    if ( this%initialized ) return

    call h5open_f ( hdferr )
    call h5fopen_f ( trim (path), H5F_ACC_RDWR_F, this%fid, hdferr )

    this%filename = trim ( path )
    this%initialized = .true.
  end subroutine rhyme_hdf5_util_open


  subroutine rhyme_hdf5_util_read_group_attribute ( this, where, key, value )
    implicit none

    class ( rhyme_hdf5_util_t ), intent ( in ) :: this
    class (*), intent ( in ) :: where
    character (len=*), intent( in ) :: key
    class(*), intent ( out ) :: value

    integer ( hid_t ) :: group_id, attr_id, dtype
    integer ( hsize_t ) :: dims(1) = 1
    integer :: hdferr


    select type ( w => where )
    type is ( character (*) )
      call h5gopen_f ( this%fid, trim (w), group_id, hdferr )
    type is ( integer ( hid_t ) )
      group_id = w
    end select

    call h5aopen_f ( group_id, trim (key), attr_id, hdferr )

    select type ( val => value )
    type is ( integer )
      call h5aread_f ( attr_id, H5T_NATIVE_INTEGER, val, dims, hdferr )
    type is ( real (kind=4) )
      call h5aread_f ( attr_id, H5T_NATIVE_REAL, val, dims, hdferr )
    type is ( real (kind=8) )
      call h5aread_f ( attr_id, H5T_NATIVE_DOUBLE, val, dims, hdferr )
    type is ( character (*) )
      call h5tcopy_f ( H5T_NATIVE_CHARACTER, dtype, hdferr )
      call h5tset_size_f ( dtype, int ( len_trim (val), kind=size_t ), hdferr )
      call h5aread_f ( attr_id, dtype, val, dims, hdferr )
    end select

    call h5aclose_f ( attr_id, hdferr )

    select type ( w => where )
    type is ( character (*) )
      call h5gclose_f ( group_id, hdferr )
    end select
  end subroutine rhyme_hdf5_util_read_group_attribute


  subroutine rhyme_hdf5_util_write_1d_dataset ( this, where, key, data )
    implicit none

    class ( rhyme_hdf5_util_t ), intent ( inout ) :: this
    class (*), intent ( in ) :: where
    character ( len=* ), intent ( in ) :: key
    class (*), dimension(:), intent ( in ) :: data

    integer ( hid_t ) :: group_id, dsetid, space_id
    integer ( hsize_t ) :: dims(1)
    integer :: hdferr


    select type ( w => where )
    type is ( character (*) )
      call h5gopen_f ( this%fid, trim(w), group_id, hdferr )
    type is ( integer ( hid_t ) )
      group_id = w
    end select

    dims = int(shape(data), kind=hsize_t)

    call h5screate_simple_f ( 1, dims, space_id, hdferr )

    select type ( d => data )
    type is ( integer )
      call h5dcreate_f ( group_id, key, H5T_NATIVE_INTEGER, space_id, dsetid, hdferr )
      call h5dwrite_f ( dsetid, H5T_NATIVE_INTEGER, d, dims, hdferr )
    type is ( real( kind=4 ) )
      call h5dcreate_f ( group_id, key, H5T_NATIVE_REAL, space_id, dsetid, hdferr )
      call h5dwrite_f ( dsetid, H5T_NATIVE_REAL, d, dims, hdferr )
    type is ( real( kind=8 ) )
      call h5dcreate_f ( group_id, key, H5T_NATIVE_DOUBLE, space_id, dsetid, hdferr )
      call h5dwrite_f ( dsetid, H5T_NATIVE_DOUBLE, d, dims, hdferr )
    end select

    call h5dclose_f ( dsetid, hdferr )
    call h5sclose_f ( space_id, hdferr )

    select type ( w => where )
    type is ( character (*) )
      call h5gclose_f ( group_id, hdferr )
    end select
  end subroutine rhyme_hdf5_util_write_1d_dataset


  subroutine rhyme_hdf5_util_read_1d_dataset ( this, where, data )
    implicit none

    class ( rhyme_hdf5_util_t ), intent ( in ) :: this
    character ( len=* ), intent ( in ) :: where
    class (*), dimension(:), intent ( out ) :: data

    integer ( hid_t ) :: dset_id
    integer ( kind=hsize_t ) :: dims(1)
    integer :: hdferr

    dims(1) = int(size(data), kind=hsize_t)

    call h5dopen_f ( this%fid, trim(where), dset_id, hdferr )

    select type ( d => data )
    type is ( integer )
      call h5dread_f ( dset_id, H5T_NATIVE_INTEGER, d, dims, hdferr )
    type is ( real( kind=4 ) )
      call h5dread_f ( dset_id, H5T_NATIVE_REAL, d, dims, hdferr )
    type is ( real( kind=8 ) )
      call h5dread_f ( dset_id, H5T_NATIVE_DOUBLE, d, dims, hdferr )
    end select

    call h5dclose_f ( dset_id, hdferr )
  end subroutine rhyme_hdf5_util_read_1d_dataset


  subroutine rhyme_hdf5_util_write_2d_dataset ( this, where, key, data )
    implicit none

    class ( rhyme_hdf5_util_t ), intent ( inout ) :: this
    class (*), intent ( in ) :: where
    character ( len=* ), intent ( in ) :: key
    class (*), dimension(:,:), intent ( in ) :: data

    integer ( hid_t ) :: group_id, dsetid, space_id
    integer ( hsize_t ) :: dims(2)
    integer :: hdferr


    select type ( w => where )
    type is ( character (*) )
      call h5gopen_f ( this%fid, trim(w), group_id, hdferr )
    type is ( integer ( hid_t ) )
      group_id = w
    end select

    dims = int(shape(data), kind=hsize_t)

    call h5screate_simple_f ( 2, dims, space_id, hdferr )

    select type ( d => data )
    type is ( integer )
      call h5dcreate_f ( group_id, key, H5T_NATIVE_INTEGER, space_id, dsetid, hdferr )
      call h5dwrite_f ( dsetid, H5T_NATIVE_INTEGER, d, dims, hdferr )
    type is ( real( kind=4 ) )
      call h5dcreate_f ( group_id, key, H5T_NATIVE_REAL, space_id, dsetid, hdferr )
      call h5dwrite_f ( dsetid, H5T_NATIVE_REAL, d, dims, hdferr )
    type is ( real( kind=8 ) )
      call h5dcreate_f ( group_id, key, H5T_NATIVE_DOUBLE, space_id, dsetid, hdferr )
      call h5dwrite_f ( dsetid, H5T_NATIVE_DOUBLE, d, dims, hdferr )
    end select

    call h5dclose_f ( dsetid, hdferr )
    call h5sclose_f ( space_id, hdferr )

    select type ( w => where )
    type is ( character (*) )
      call h5gclose_f ( group_id, hdferr )
    end select
  end subroutine rhyme_hdf5_util_write_2d_dataset


  subroutine rhyme_hdf5_util_read_2d_dataset ( this, where, data )
    implicit none

    class ( rhyme_hdf5_util_t ), intent ( in ) :: this
    character ( len=* ), intent ( in ) :: where
    class (*), dimension(:,:), intent ( out ) :: data

    integer ( hid_t ) :: dset_id
    integer ( kind=hsize_t ) :: dims(2)
    integer :: hdferr

    dims = int(shape(data), kind=hsize_t)

    call h5dopen_f ( this%fid, trim(where), dset_id, hdferr )

    select type ( d => data )
    type is ( integer )
      call h5dread_f ( dset_id, H5T_NATIVE_INTEGER, d, dims, hdferr )
    type is ( real( kind=4 ) )
      call h5dread_f ( dset_id, H5T_NATIVE_REAL, d, dims, hdferr )
    type is ( real( kind=8 ) )
      call h5dread_f ( dset_id, H5T_NATIVE_DOUBLE, d, dims, hdferr )
    end select

    call h5dclose_f ( dset_id, hdferr )
  end subroutine rhyme_hdf5_util_read_2d_dataset


  subroutine rhyme_hdf5_util_close ( this )
    implicit none

    class ( rhyme_hdf5_util_t ), intent (inout) :: this

    integer :: hdferr


    if ( .not. this%initialized ) return

    call h5fclose_f ( this%fid, hdferr )
    call h5close_f ( hdferr )

    if ( hdferr >= 0 ) then
      this%fid = h5id%unset
      this%filename = ""
      this%initialized = .false.
    end if
  end subroutine rhyme_hdf5_util_close

end module rhyme_hdf5_util
