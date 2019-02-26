logical function rhyme_drawing_new_shape_test () result (failed)
  use rhyme_drawing

  implicit none

  type ( drawing_t ) :: draw
  type ( shape_t ), pointer :: shape

  shape => draw%new_shape ( drid%circle )

  failed = &
  .not. associated ( draw%shapes ) &
  .or. .not. associated ( shape ) &
  .or. draw%shapes%type .ne. drid%circle &
  .or. shape%type .ne. drid%circle &
  .or. draw%shapes%fill%type .ne. drid%unset &
  .or. shape%fill%type .ne. drid%unset &
  .or. draw%shapes%trans%type .ne. drid%unset &
  .or. shape%trans%type .ne. drid%unset


  if ( failed ) return

  shape => draw%new_shape ( drid%rect )

  failed = &
  .not. associated ( draw%shapes%next ) &
  .or. draw%shapes%type .ne. drid%circle &
  .or. draw%shapes%next%type .ne. drid%rect &
  .or. draw%shapes%next%fill%type .ne. drid%unset &
  .or. draw%shapes%next%trans%type .ne. drid%unset
end function rhyme_drawing_new_shape_test
