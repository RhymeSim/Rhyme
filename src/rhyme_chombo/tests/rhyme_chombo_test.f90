logical function rhyme_chombo_test () result ( failed )
  use rhyme_chombo

  implicit none

  type ( chombo_t ) :: ch

  failed = &
  chid%unset .ne. h5id%unset &
  .or. ch%is_opened &
  .or. ch%iteration .ne. chid%unset &
  .or. ch%num_levels .ne. chid%unset &
  .or. ch%num_components .ne. chid%unset &
  .or. any ( ch%level_ids .ne. chid%unset ) &
  .or. ch%initialized .eqv. .true.
end function rhyme_chombo_test
