logical function rhyme_chombo_setup_test () result ( failed )
  use rhyme_chombo

  implicit none

  type ( rhyme_chombo_t ) :: ch
  integer(hid_t) :: fid
  integer :: hdferr
  logical :: g_exists

  character(len=256), parameter :: testfile = "./test_chombo_file.h5"

  call ch%setup ( testfile, 2 )


  call h5open_f ( hdferr )
  call h5fopen_f ( trim(testfile), H5F_ACC_RDONLY_F, fid, hdferr )

  failed = hdferr < 0 .or. .not. ch%initialized .or. ch%num_levels .ne. 2

  if ( failed ) return


  call h5lexists_f ( fid, "/level_0", g_exists, hdferr )
  failed = hdferr < 0 .or. .not. g_exists

  if ( failed ) return


  call h5lexists_f ( fid, "/level_1", g_exists, hdferr )
  failed = hdferr < 0 .or. .not. g_exists

  if ( failed ) return


  call h5lexists_f ( fid, "/level_2", g_exists, hdferr )
  failed = hdferr < 0 .or. g_exists


  call h5fclose_f ( fid, hdferr )
  call ch%close
end function rhyme_chombo_setup_test
