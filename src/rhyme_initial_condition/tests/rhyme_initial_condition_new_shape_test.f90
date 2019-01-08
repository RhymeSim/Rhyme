logical function rhyme_initial_condition_new_shape_test () result (failed)
  use rhyme_initial_condition

  implicit none

  type ( initial_condition_t ) :: ic
  type ( ic_shape_t ), pointer :: shape

  shape => ic%new_shape ( icid%circle )

  failed = &
  .not. associated ( ic%shapes ) &
  .or. .not. associated ( shape ) &
  .or. ic%shapes%type .ne. icid%circle &
  .or. shape%type .ne. icid%circle &
  .or. ic%shapes%fill%type .ne. icid%unset &
  .or. shape%fill%type .ne. icid%unset &
  .or. ic%shapes%trans%type .ne. icid%unset &
  .or. shape%trans%type .ne. icid%unset


  if ( failed ) return

  shape => ic%new_shape ( icid%rect )

  failed = &
  .not. associated ( ic%shapes%next ) &
  .or. ic%shapes%type .ne. icid%circle &
  .or. ic%shapes%next%type .ne. icid%rect &
  .or. ic%shapes%next%fill%type .ne. icid%unset &
  .or. ic%shapes%next%trans%type .ne. icid%unset
end function rhyme_initial_condition_new_shape_test
