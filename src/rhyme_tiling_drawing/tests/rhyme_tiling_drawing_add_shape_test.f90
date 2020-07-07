logical function rhyme_tiling_drawing_add_shape_test() result(failed)
   use rhyme_tiling_drawing_factory
   use rhyme_assertion

   implicit none

   type(assertion_t) :: tester
   type(tiling_drawing_t) :: tdraw

   type(tiling_shape_t), pointer :: shape => null(), added_shape => null()

   tester = .describe.'tiling_drawing_add_shape'

   call tester%expect(associated(tdraw%shapes) .toBe..false..hint.'Initial shapes')

   added_shape => tdraw%add(tdrid%sphere)
   shape => tdraw%shapes
   call tester%expect(associated(shape) .toBe..true..hint.'1st shape')
   call tester%expect(shape%type.toBe.tdrid%sphere.hint.'1st shape, type')
   call tester%expect(associated(added_shape, shape) .toBe..true..hint.'1st shape, order')

   added_shape => tdraw%add(tdrid%sphere)
   shape => tdraw%shapes%next
   call tester%expect(associated(shape) .toBe..true..hint.'2nd shape')
   call tester%expect(shape%type.toBe.tdrid%sphere.hint.'2nd shape, type')
   call tester%expect(associated(added_shape, shape) .toBe..true..hint.'2nd shape, order')

   failed = tester%failed()
end function rhyme_tiling_drawing_add_shape_test
