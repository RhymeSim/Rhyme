logical function rhyme_chombo_test () result ( failed )
  use rhyme_chombo

  implicit none

  type ( rhyme_chombo_t ) :: ch

  failed = &
  ch%num_levels .ne. 0 &
  .or. ch%num_components .ne. 0 &
  .or. ch%initialized .eqv. .true.
end function rhyme_chombo_test
