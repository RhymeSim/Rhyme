logical function rhyme_muscl_hancock_init_with_test () result ( failed )
  use rhyme_muscl_hancock_factory

  implicit none

  type ( muscl_hancock_t ) :: mh

  call rhyme_muscl_hancock_factory_init


  call mh%init_with ( cfl, ig, irs, sl, samr )


  failed = &
  .not. mh%initialized &
  .or. mh%ig%type .ne. ig%type &
  .or. mh%sl%type .ne. sl%type &
  .or. mh%irs%n_iteration .ne. irs%n_iteration &
  .or. abs ( mh%cfl%courant_number - cfl%courant_number ) > epsilon(0.d0) &
  .or. abs ( mh%irs%tolerance - irs%tolerance ) > epsilon(0.d0) &
  .or. abs ( mh%irs%pressure_floor - irs%pressure_floor ) > epsilon(0.d0) &
  .or. .not. mh%initialized
end function rhyme_muscl_hancock_init_with_test
