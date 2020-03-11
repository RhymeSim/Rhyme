logical function rhyme_drawing_new_shape_test() result(failed)
   use rhyme_drawing
   use rhyme_assertion

   implicit none

   type(assertion_t) :: dr_tester

   type(drawing_t) :: draw
   type(shape_t), pointer :: shape

   dr_tester = .describe."drawing new_shape"

   shape => draw%new_shape(drid%sphere)

   call dr_tester%expect(associated(draw%shapes) .toBe..true.)
   call dr_tester%expect(associated(shape) .toBe..true.)
   call dr_tester%expect(draw%shapes%type.toBe.drid%sphere)
   call dr_tester%expect(shape%type.toBe.drid%sphere)
   call dr_tester%expect(draw%shapes%fill%type.toBe.drid%unset)
   call dr_tester%expect(shape%fill%type.toBe.drid%unset)

   shape => draw%new_shape(drid%cuboid)

   call dr_tester%expect(associated(draw%shapes%next) .toBe..true.)
   call dr_tester%expect(draw%shapes%type.toBe.drid%sphere)
   call dr_tester%expect(draw%shapes%next%type.toBe.drid%cuboid)
   call dr_tester%expect(draw%shapes%next%fill%type.toBe.drid%unset)

   failed = dr_tester%failed()
end function rhyme_drawing_new_shape_test
