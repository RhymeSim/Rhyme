module rhyme_hdf5_util
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
    procedure :: create => rhyme_hdf5_util_create
    procedure :: add_group_attr => rhyme_hdf5_util_add_group_attribute
    procedure :: add_group_1d_array_attr => rhyme_hdf5_util_add_group_1d_array_attribute
    procedure :: add_group_comp_1d_array_attr => rhyme_hdf5_util_add_group_compound_1d_array_attribute
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


  subroutine rhyme_hdf5_util_add_group_attribute ( this, where, key, value )
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
    end subroutine write_group_attribute
  end subroutine rhyme_hdf5_util_add_group_attribute


  subroutine rhyme_hdf5_util_add_group_1d_array_attribute ( this, where, key, array )
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

    call h5gclose_f ( group_id, hdferr )
  end subroutine rhyme_hdf5_util_add_group_1d_array_attribute

  subroutine rhyme_hdf5_util_add_group_compound_1d_array_attribute ( this, where, key, keys, values )
    implicit none

    class ( rhyme_hdf5_util_t ), intent(inout) :: this
    character (len=*), intent(in) :: where, key
    character (len=*), dimension(:), intent(in) :: keys
    class (*), dimension(:) :: values

    integer ( hid_t ) :: group_id
    integer :: hdferr


    if ( .not. this%initialized ) return

    call h5gopen_f ( this%fid, trim(where), group_id, hdferr )

    select type ( vals => values)
    type is ( integer )
      call write_comp_array_attr ( H5T_NATIVE_INTEGER )
    type is ( real ( kind = 4 ) )
      call write_comp_array_attr ( H5T_NATIVE_REAL )
    type is ( real ( kind = 8 ) )
      call write_comp_array_attr ( H5T_NATIVE_DOUBLE )
    end select

    call h5gclose_f ( group_id, hdferr )

  contains
    subroutine write_comp_array_attr ( type_id )
      implicit none

      integer ( hid_t ) :: type_id

      integer ( hid_t ) :: space_id, attr_id, comp_type
      integer ( size_t ) :: element_size, tot_size, offset
      integer ( hsize_t ) :: dims(1) = 1
      integer :: i

      call h5tget_size_f ( type_id, element_size, hdferr )
      tot_size = size ( keys ) * element_size
      call h5tcreate_f ( H5T_COMPOUND_F , tot_size, comp_type, hdferr )

      offset = 0
      do i = 1, size ( keys )
        call h5tinsert_f ( comp_type, trim ( keys(i) ), offset, type_id, hdferr )
        offset = offset + element_size
      end do

      call h5screate_simple_f ( 1, dims, space_id, hdferr )
      call h5acreate_f ( group_id, trim(key), comp_type, space_id, attr_id, hdferr )

      select type ( vals => values)
      type is ( integer )
        call h5awrite_f ( attr_id, comp_type, vals, dims, hdferr)
      type is ( real ( kind = 4 ))
        call h5awrite_f ( attr_id, comp_type, vals, dims, hdferr)
      type is ( real ( kind = 8 ))
        call h5awrite_f ( attr_id, comp_type, vals, dims, hdferr)
      end select

      call h5aclose_f ( attr_id, hdferr )
    end subroutine write_comp_array_attr
  end subroutine rhyme_hdf5_util_add_group_compound_1d_array_attribute



  subroutine rhyme_hdf5_util_close ( this )
    implicit none

    class ( rhyme_hdf5_util_t ), intent (inout) :: this

    integer :: hdferr

    call h5fclose_f ( this%fid, hdferr )

    if ( hdferr >= 0 ) then
      this%fid = h5id%unset
      this%filename = ""
      this%initialized = .false.
    end if
  end subroutine rhyme_hdf5_util_close

end module rhyme_hdf5_util