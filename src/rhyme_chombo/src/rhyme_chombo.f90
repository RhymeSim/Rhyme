module rhyme_chombo
  use hdf5
  use rhyme_samr

  implicit none

  type rhyme_chombo_indices_t
    integer :: unset = -1
  end type rhyme_chombo_indices_t

  type ( rhyme_chombo_indices_t ), parameter :: chid = rhyme_chombo_indices_t ()


  type rhyme_chombo_t
    character(len=1024) :: filename
    integer(hid_t) :: fid = chid%unset
    integer :: num_levels = 0
    integer :: num_components = 0
    logical :: initialized = .false.
  contains
    procedure :: setup => rhyme_chombo_setup
    procedure :: add_attr => rhyme_chombo_add_attribute
    procedure :: add_array_attr => rhyme_chombo_add_array_attribute
    procedure :: add_comp_array_attr => rhyme_chombo_add_compound_array_attribute
    procedure :: close => rhyme_chombo_close
  end type rhyme_chombo_t

contains

  subroutine rhyme_chombo_setup ( this, filename, samr )
    implicit none

    class ( rhyme_chombo_t ), intent(inout) :: this
    character(len=*), intent(in) :: filename
    type ( samr_t ), intent(in) :: samr

    integer :: i, hdferr, n_dims
    character(len=32) :: level_name
    integer(hid_t) :: group_id

    if ( this%initialized ) return

    this%filename = trim ( filename )
    this%num_levels = samr%nlevels

    call h5open_f ( hdferr )
    ! TODO: check hdferr

    call h5fcreate_f ( trim(filename), H5F_ACC_TRUNC_F, this%fid, hdferr )
    ! TODO: check hdferr

    do i = 0, samr%nlevels - 1
      write ( level_name, '(A6,I1)') "level_", i
      call h5gcreate_f ( this%fid, trim(level_name), group_id, hdferr )
    end do

    this%initialized = .true.

    n_dims = size ( samr%base_grid ) - sum ( samr%base_grid * merge ( 1, 0, samr%base_grid <= 1 ) )

    call h5gcreate_f ( this%fid, "/chombo_global", group_id, hdferr )
    call this%add_attr ( "/chombo_global", "SpaceDim", n_dims )
  end subroutine rhyme_chombo_setup


  subroutine rhyme_chombo_add_attribute ( this, where, key, value )
    implicit none

    class ( rhyme_chombo_t ), intent(inout) :: this
    character(len=*), intent(in) :: where, key
    class (*) :: value

    integer ( hid_t ) :: group_id, space_id, attr_id, dtype
    integer :: hdferr
    integer ( hsize_t ) :: attr_1d(1) = 1

    if ( .not. this%initialized ) return

    call h5gopen_f ( this%fid, trim(where), group_id, hdferr )
    ! TODO: check hdferr

    call h5screate_simple_f ( 1, attr_1d, space_id, hdferr )
    ! TODO: check hdferr

    select type ( val => value )
    type is ( integer )
      call h5tcopy_f ( H5T_NATIVE_INTEGER, dtype, hdferr )
      call h5acreate_f ( group_id, key, dtype, space_id, attr_id, hdferr )
      call h5awrite_f ( attr_id, dtype, val, attr_1d, hdferr )
      call h5aclose_f ( attr_id, hdferr )

    type is ( real(kind=4) )
      call h5tcopy_f ( H5T_NATIVE_REAL, dtype, hdferr )
      call h5acreate_f ( group_id, key, dtype, space_id, attr_id, hdferr )
      call h5awrite_f ( attr_id, dtype, val, attr_1d, hdferr )
      call h5aclose_f ( attr_id, hdferr )

    type is ( real(kind=8) )
      call h5tcopy_f ( H5T_NATIVE_DOUBLE, dtype, hdferr )
      call h5acreate_f ( group_id, key, dtype, space_id, attr_id, hdferr )
      call h5awrite_f ( attr_id, dtype, val, attr_1d, hdferr )
      call h5aclose_f ( attr_id, hdferr )

    type is ( character(*) )
      call h5tcopy_f ( H5T_NATIVE_CHARACTER, dtype, hdferr )
      call h5tset_size_f ( dtype, int(len_trim(val), kind=size_t), hdferr )
      call h5acreate_f ( group_id, key, dtype, space_id, attr_id, hdferr )
      call h5awrite_f ( attr_id, dtype, trim(val), attr_1d, hdferr )
      call h5aclose_f ( attr_id, hdferr )

    end select

    call h5gclose_f ( group_id, hdferr )
  end subroutine rhyme_chombo_add_attribute


  subroutine rhyme_chombo_add_array_attribute ( this, where, key, ndims, len, value )
    implicit none

    class ( rhyme_chombo_t ), intent(inout) :: this
    character(len=*), intent(in) :: where, key
    integer, intent(in) :: ndims
    integer, dimension(ndims), intent(in) :: len
    class (*), dimension(ndims) :: value

    integer ( hid_t ) :: group_id, space_id, attr_id
    integer :: hdferr

    if ( .not. this%initialized ) return

    call h5gopen_f ( this%fid, trim(where), group_id, hdferr )

    call h5screate_simple_f ( ndims, int(len, kind=hsize_t), space_id, hdferr )

    select type ( val => value )
    type is ( integer )
      call h5acreate_f ( group_id, trim(key), H5T_NATIVE_INTEGER, space_id, attr_id, hdferr )
      call h5awrite_f ( attr_id, H5T_NATIVE_INTEGER, val, int(len, kind=hsize_t), hdferr )

    type is ( real ( kind=4 ) )
      call h5acreate_f ( group_id, trim(key), H5T_NATIVE_REAL, space_id, attr_id, hdferr )
      call h5awrite_f ( attr_id, H5T_NATIVE_REAL, val, int(len, kind=hsize_t), hdferr )

    type is ( real ( kind=8 ) )
      call h5acreate_f ( group_id, trim(key), H5T_NATIVE_DOUBLE, space_id, attr_id, hdferr )
      call h5awrite_f ( attr_id, H5T_NATIVE_DOUBLE, val, int(len, kind=hsize_t), hdferr )

    end select

    call h5gclose_f ( group_id, hdferr )
  end subroutine rhyme_chombo_add_array_attribute


  subroutine rhyme_chombo_add_compound_array_attribute ( this, where, name, keys, values )
    implicit none

    class ( rhyme_chombo_t ), intent(inout) :: this
    character(len=*), intent(in) :: where, name
    character(len=*), dimension(:), intent(in) :: keys
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
      call h5acreate_f ( group_id, trim(name), comp_type, space_id, attr_id, hdferr )

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
  end subroutine rhyme_chombo_add_compound_array_attribute


  subroutine rhyme_chombo_close ( this )
    implicit none

    class ( rhyme_chombo_t ), intent(inout) :: this

    integer :: hdferr

    if ( .not. this%initialized ) return

    call h5fclose_f ( this%fid, hdferr )
    ! TODO: check hdferr

    this%fid = chid%unset

    this%initialized = .false.
  end subroutine rhyme_chombo_close
end module rhyme_chombo
