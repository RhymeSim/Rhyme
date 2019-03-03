logical function rhyme_initial_condition_load_snapshot_test () result ( failed )
  use rhyme_samr_factory
  use rhyme_initial_condition_factory

  implicit none

  ! type ( initial_condition_t ) :: ic
  ! type ( samr_t ) :: samr

  ! TODO: test if load_snapshot sets nboxes to zero
  failed = .true.
end function rhyme_initial_condition_load_snapshot_test
