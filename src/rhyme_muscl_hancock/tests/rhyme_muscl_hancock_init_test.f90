logical function rhyme_muscl_hancock_init_test () result ( failed )
  use rhyme_muscl_hancock_factory

  implicit none

  type ( muscl_hancock_t ) :: mh


  call rhyme_muscl_hancock_factory_init

  call mh%init( samr, log )

  failed = .not. mh%ws%initialized .or. .not. mh%initialized
end function rhyme_muscl_hancock_init_test
