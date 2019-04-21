logical function rhyme_hdf5_util_write_group_comp_1d_array_attr_test () result ( failed )
  use rhyme_hdf5_util
  use rhyme_assertion

  implicit none

  type ( assertion_t ) :: h5_tester

  ! Constants
  character ( len=1024 ), parameter :: testfile = "./test_hdf5_util_write_group_comp_1d_array_attr.h5"
  character(len=8), parameter :: keys(3) = [ "key_1   ", "key_2   ", "key_3   " ]
  real (kind=8), parameter :: array_r8(3) = [ 1.d0, 2.d0, 3.d0 ]
  real (kind=4), parameter :: array_r(3) = [ 4.e0, 5.e0, 6.e0 ]
  integer, parameter :: array_i(3) = [ 7, 8, 9 ]

  ! HDF5 related variables
  integer :: hdferr
  integer ( hid_t ) :: fid

  ! rhyme_hdf5_util variables
  type ( rhyme_hdf5_util_t ) :: h5

  ! variables
  real (kind=8) :: array_r8_read(3) = 0.d0
  real (kind=4) :: array_r_read(3) = 0.e0
  integer :: array_i_read(3) = 0

  h5_tester = .describe. "hdf5_util write_group_comp_1d_array_attr"

  call h5%create ( testfile )
  call h5%write_group_comp_1d_array_attr ( "/", "int_attr", keys , array_i )
  call h5%write_group_comp_1d_array_attr ( "/", "real_attr", keys , array_r )
  call h5%write_group_comp_1d_array_attr ( "/", "real8_attr", keys , array_r8 )
  call h5%close

  call h5open_f ( hdferr )
  call h5fopen_f ( trim(testfile), H5F_ACC_RDONLY_F, fid, hdferr )

  call read_comp_array_attr ( H5T_NATIVE_INTEGER, "int_attr", array_i_read )
  call read_comp_array_attr ( H5T_NATIVE_REAL, "real_attr", array_r_read )
  call read_comp_array_attr ( H5T_NATIVE_DOUBLE, "real8_attr", array_r8_read )

  call h5fclose_f ( fid, hdferr )
  call h5close_f ( hdferr )

  call h5_tester%expect( array_i .toBe. array_i_read )
  call h5_tester%expect( array_r .toBe. array_r_read )
  call h5_tester%expect( array_r8 .toBe. array_r8_read )

  failed = h5_tester%failed()
contains
  subroutine read_comp_array_attr ( type_id, attr_name, buf )
    implicit none

    integer ( hid_t ) :: type_id
    character ( len=* ) :: attr_name
    class(*), dimension(:) :: buf

    integer ( hid_t ) :: attr_id, space_id, mem_id
    integer ( hsize_t ), dimension(1) :: dims, maxdims
    integer ( size_t ) :: element_size, tot_size, offset
    integer :: i

    call h5aopen_f ( fid, trim(attr_name), attr_id, hdferr )

    call h5tget_size_f ( type_id, element_size, hdferr )
    tot_size = size ( keys ) * element_size
    call h5tcreate_f ( H5T_COMPOUND_F , tot_size, mem_id, hdferr )

    offset = 0
    do i = 1, size ( keys )
      call h5tinsert_f ( mem_id, trim ( keys(i) ), offset, type_id, hdferr )
      offset = offset + element_size
    end do

    call h5aget_space_f ( attr_id, space_id, hdferr )
    call h5sget_simple_extent_dims_f ( space_id, dims, maxdims, hdferr )

    select type ( arr => buf )
    type is ( integer )
      call h5aread_f ( attr_id, mem_id, arr, dims, hdferr)
    type is ( real ( kind = 4 ) )
      call h5aread_f ( attr_id, mem_id, arr, dims, hdferr)
    type is ( real ( kind = 8 ) )
      call h5aread_f ( attr_id, mem_id, arr, dims, hdferr)
    end select

    call h5aclose_f ( attr_id, hdferr )
  end subroutine read_comp_array_attr
end function rhyme_hdf5_util_write_group_comp_1d_array_attr_test
