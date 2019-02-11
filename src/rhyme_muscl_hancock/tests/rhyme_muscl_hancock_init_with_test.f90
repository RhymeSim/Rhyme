logical function rhyme_muscl_hancock_init_with_test () result ( failed )
  use rhyme_muscl_hancock_factory

  implicit none

  type ( muscl_hancock_t ) :: mh

  call rhyme_muscl_hancock_factory_init


  call mh%init_with ( cfl, ig, irs_config, sl, samr )


  failed = &
  mh%cfl%courant_number .ne. cfl%courant_number &
  .or. mh%ig%type .ne. ig%type &
  .or. mh%sl%type .ne. sl%type &
  .or. mh%irs_config%n_iteration .ne. mh%irs_config%n_iteration &
  .or. abs ( mh%irs_config%tolerance - irs_config%tolerance ) > epsilon(0.d0) &
  .or. abs ( mh%irs_config%pressure_floor - irs_config%pressure_floor ) > epsilon(0.d0) &
  .or. .not. mh%initialized
end function rhyme_muscl_hancock_init_with_test
