logical function rhyme_muscl_hancock_init_with_test () result ( failed )
  use rhyme_muscl_hancock_factory

  implicit none

  type ( muscl_hancock_t ) :: mh

  call rhyme_muscl_hancock_factory_init


  call mh%init_with ( cfl, ig, sl, samr )


  failed = &
  mh%cfl%courant_number .ne. cfl%courant_number &
  .or. mh%ig%type .ne. ig%type &
  .or. mh%sl%type .ne. sl%type &
  .or. .not. mh%initialized
end function rhyme_muscl_hancock_init_with_test
