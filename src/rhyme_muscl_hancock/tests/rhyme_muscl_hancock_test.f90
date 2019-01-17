logical function rhyme_muscl_hancock_test () result ( failed )
  use rhyme_muscl_hancock_factory

  implicit none

  type ( muscl_hancock_t ) :: mh


  call rhyme_muscl_hancock_factory_init
  call mh%init ( samr )

  failed = .not. mh%ws%initialized
end function rhyme_muscl_hancock_test
