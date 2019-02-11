logical function rhyme_muscl_hancock_init_test () result ( failed )
  use rhyme_muscl_hancock_factory

  implicit none

  type ( muscl_hancock_t ) :: mh


  call rhyme_muscl_hancock_factory_init


  mh%cfl = cfl
  mh%ig = ig
  mh%irs_config = irs_config
  mh%sl = sl


  call mh%init ( samr )

  failed = &
  mh%ws%type .ne. wsid%memory_intensive &
  .or. .not. mh%ws%initialized
end function rhyme_muscl_hancock_init_test
