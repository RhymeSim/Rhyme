logical function rhyme_hdf5_util_read_group_1d_array_attr_test () result ( failed )
  use rhyme_hdf5_util
  use rhyme_assertion

  implicit none

  type ( assertion_t ) :: h5_tester

  ! Constants
  character ( len=1024 ), parameter :: testfile = "./test_hdf5_util_read_group_1d_array_attr.h5"

  ! rhyme_hdf5_util variables
  type ( rhyme_hdf5_util_t ) :: h5, h5read
  integer :: int_arr(6) = [ 1, 2, 3, 4, 5, 6 ]
  integer :: int_arr_read(6) = 0
  real ( kind=4 ) :: real_arr(6) = [ 1.e0, 2.e0, 3.e0, 4.e0, 5.e0, 6.e0 ]
  real ( kind=4 ) :: real_arr_read(6) = 0.e0
  real ( kind=8 ) :: real8_arr(6) = [ 1.d0, 2.d0, 3.d0, 4.d0, 5.d0, 6.d0 ]
  real ( kind=8 ) :: real8_arr_read(6) = 0.e0

  h5_tester = .describe. "hdf5_util read_group_1d_array_attr"

  ! Creating hdf5 file
  call h5%create( testfile )
  call h5%write_group_1d_array_attr( '/', 'int', int_arr )
  call h5%write_group_1d_array_attr( '/', 'real', real_arr )
  call h5%write_group_1d_array_attr( '/', 'real8', real8_arr )
  call h5%close

  ! test
  call h5read%open( testfile )
  call h5read%read_group_1d_array_attr( '/', 'int', int_arr_read )
  call h5read%read_group_1d_array_attr( '/', 'real', real_arr_read )
  call h5read%read_group_1d_array_attr( '/', 'real8', real8_arr_read )
  call h5read%close

  call h5_tester%expect( int_arr_read .toBe. int_arr )
  call h5_tester%expect( real_arr_read .toBe. real_arr )
  call h5_tester%expect( real8_arr_read .toBe. real8_arr )

  failed = h5_tester%failed()
end function rhyme_hdf5_util_read_group_1d_array_attr_test
