logical function rhyme_chombo_setup_test () result ( failed )
  use rhyme_chombo_factory

  implicit none

  type ( rhyme_chombo_t ) :: ch
  integer(hid_t) :: fid, attr_id
  integer :: n_dims, hdferr
  integer ( hsize_t ), parameter :: dims(1) = int ( 1, kind=hsize_t )
  logical :: g_exists

  character(len=256), parameter :: testfile = "./test_chombo_file.h5"

  call rhyme_chombo_factory_init
  call ch%setup ( testfile, samr )


  call h5open_f ( hdferr )
  call h5fopen_f ( trim(testfile), H5F_ACC_RDONLY_F, fid, hdferr )

  failed = hdferr < 0 .or. .not. ch%initialized .or. ch%num_levels .ne. nlevels

  if ( failed ) return


  call h5aopen_by_name_f ( fid, "/chombo_global", "SpaceDim", attr_id, hdferr )
  call h5aread_f ( attr_id, H5T_NATIVE_INTEGER, n_dims, dims, hdferr )
  call h5aclose_f ( attr_id, hdferr )

  failed = n_dims .ne. 2
  if ( failed ) return


  call h5lexists_f ( fid, "/level_0", g_exists, hdferr )
  failed = hdferr < 0 .or. .not. g_exists
  if ( failed ) return


  call h5lexists_f ( fid, "/level_1", g_exists, hdferr )
  failed = hdferr < 0 .or. .not. g_exists
  if ( failed ) return


  call h5lexists_f ( fid, "/level_2", g_exists, hdferr )
  failed = hdferr < 0 .or. .not. g_exists
  if ( failed ) return


  call h5lexists_f ( fid, "/level_3", g_exists, hdferr )
  failed = hdferr < 0 .or. g_exists


  call h5fclose_f ( fid, hdferr )
  call ch%close
end function rhyme_chombo_setup_test
