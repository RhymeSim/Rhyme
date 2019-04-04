logical function rhyme_drawing_new_shape_test () result (failed)
  use rhyme_drawing

  implicit none

  type ( drawing_t ) :: draw
  type ( shape_t ), pointer :: shape

  shape => draw%new_shape( drid%sphere )

  failed = &
  .not. associated( draw%shapes ) &
  .or. .not. associated( shape ) &
  .or. draw%shapes%type .ne. drid%sphere &
  .or. shape%type .ne. drid%sphere &
  .or. draw%shapes%fill%type .ne. drid%unset &
  .or. shape%fill%type .ne. drid%unset &
  .or. any( draw%shapes%trans%type .ne. drid%none ) &
  .or. any( shape%trans%type .ne. drid%none ) &
  .or. any( abs( draw%shapes%trans%sigma ) > epsilon(0.d0) ) &
  .or. any( abs( shape%trans%sigma ) > epsilon(0.d0) )
  if ( failed ) return


  shape => draw%new_shape( drid%cuboid )

  failed = &
  .not. associated ( draw%shapes%next ) &
  .or. draw%shapes%type .ne. drid%sphere &
  .or. draw%shapes%next%type .ne. drid%cuboid &
  .or. draw%shapes%next%fill%type .ne. drid%unset &
  .or. any( draw%shapes%next%trans%type .ne. drid%none ) &
  .or. any( abs( draw%shapes%next%trans%sigma ) > epsilon(0.d0) )
end function rhyme_drawing_new_shape_test
