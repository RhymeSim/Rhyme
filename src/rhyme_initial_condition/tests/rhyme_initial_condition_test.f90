logical function rhyme_initial_condition_test () result ( failed )
  use rhyme_initial_condition

  implicit none

  type ( initial_condition_t ) :: ic

  failed = &
  icid%simple .ne. 1 &
  .or. icid%snapshot .ne. 2 &
  .or. icid%rhyme .ne. 10 &
  .or. icid%radamesh .ne. 11 &
  .or. icid%r2c_2d .ne. 12 &
  .or. icid%unset .ne. -1
  if ( failed ) return

  failed = &
  ic%type .ne. icid%unset &
  .or. any( ic%base_grid .ne. icid%unset ) &
  .or. ic%nlevels .ne. icid%unset &
  .or. any( ic%max_nboxes .ne. 0 ) &
  .or. trim(ic%snapshot_path) .ne. ''
end function rhyme_initial_condition_test
