logical function rhyme_chombo_setup_test () result ( failed )
  use rhyme_chombo_factory

  implicit none

  type ( rhyme_chombo_t ) :: ch

  character(len=256), parameter :: testfile = "./test_chombo_file.h5"

  call rhyme_chombo_factory_init
  call ch%setup ( testfile, samr )
end function rhyme_chombo_setup_test
