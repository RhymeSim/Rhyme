logical function rhyme_hdf5_util_write_table_test () result ( failed )
  use rhyme_hdf5_util
  use rhyme_assertion

  implicit none

  type ( assertion_t ) :: h5_tester

  type ( rhyme_hdf5_util_t ) :: h5

  character ( len=1024 ), parameter :: testfile = "rhyme_hdf5_util_write_table.h5"
  integer, parameter :: ncol = 3, nrow = 4

  character ( len=8 ), parameter :: headers(ncol) = [ "key_1   ", "key_2   ", "key_3   " ]
  integer :: int_table( ncol, nrow ), int_table_read( ncol, nrow )
  real ( kind=4 ) :: real_table( ncol, nrow ), real_table_read( ncol, nrow )
  real ( kind=8 ) :: real8_table( ncol, nrow ), real8_table_read( ncol, nrow )

  integer ( kind=hid_t ) :: file_id, dset_id, table_id
  integer ( kind=hsize_t ) :: dims(2)
  integer :: i, hdferr

  h5_tester = .describe. "hdf5_util write_table"

  int_table = reshape ( [ (i, i=1, 12) ], [ ncol, nrow ])
  real_table = reshape ( [ (real( i, kind=4 ), i=1, 12) ], [ ncol, nrow ])
  real8_table = reshape ( [ (real( i, kind=8 ), i=1, 12) ], [ ncol, nrow ])

  call h5%create ( testfile )
  call h5%write_table ( "/", "int-table", headers, int_table )
  call h5%write_table ( "/", "real-table", headers, real_table )
  call h5%write_table ( "/", "real8-table", headers, real8_table )
  call h5%close

  ! Tests
  call h5open_f ( hdferr )
  call h5fopen_f ( trim(testfile), H5F_ACC_RDONLY_F, file_id, hdferr )

  dims = int( size( int_table_read ), kind=hsize_t )

  ! Integer table
  call create_table_type ( H5T_NATIVE_INTEGER, table_id )
  call h5dopen_f ( file_id, "int-table", dset_id, hdferr )
  call h5dread_f ( dset_id, table_id, int_table_read, dims, hdferr )
  call h5dclose_f ( dset_id, hdferr )

  call h5_tester%expect( int_table .toBe. int_table_read )

  ! Real table
  call create_table_type ( H5T_NATIVE_REAL, table_id )
  call h5dopen_f ( file_id, "real-table", dset_id, hdferr )
  call h5dread_f ( dset_id, table_id, real_table_read, dims, hdferr )
  call h5dclose_f ( dset_id, hdferr )

  call h5_tester%expect( real_table .toBe. real_table_read )

  ! Real8 table
  call create_table_type ( H5T_NATIVE_DOUBLE, table_id )
  call h5dopen_f ( file_id, "real8-table", dset_id, hdferr )
  call h5dread_f ( dset_id, table_id, real8_table_read, dims, hdferr )
  call h5dclose_f ( dset_id, hdferr )

  call h5_tester%expect( real8_table .toBe. real8_table_read )

  call h5fclose_f ( file_id, hdferr )
  call h5close_f ( hdferr )

  failed = h5_tester%failed()
contains

  subroutine create_table_type ( type_id, table_type )
    implicit none

    integer ( kind=hid_t ), intent ( in ) :: type_id
    integer ( kind=hid_t ), intent ( out ) :: table_type

    integer ( kind=hsize_t ) ::  type_size, row_size, offset
    integer :: h, err


    call h5tget_size_f ( type_id, type_size, err )
    row_size = size( headers ) * type_size
    call h5tcreate_f ( H5T_COMPOUND_F, row_size, table_type, err )


    offset = 0
    do h = 1, size( headers )
      call h5tinsert_f ( table_type, trim( headers(h) ), offset, type_id, err )
      offset = offset + type_size
    end do
  end subroutine create_table_type
end function rhyme_hdf5_util_write_table_test
