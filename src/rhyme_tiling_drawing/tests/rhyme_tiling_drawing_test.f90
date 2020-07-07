logical function rhyme_tiling_drawing_test() result(failed)
   use rhyme_tiling_drawing_factory
   use rhyme_assertion

   implicit none

   type(assertion_t) :: tester

   type(tiling_drawing_t) :: draw

   tester = .describe."tiling_drawing"

   draw = tiling_drawing_factory_generate('default')

   call tester%expect(tdrid%unset.toBe.-1.hint.'unset')

   ! Shapes
   call tester%expect(tdrid%sphere.toBe.1.hint.'sphere')

   ! Smoothing functions
   call tester%expect(tdrid%tanh.toBe.1.hint.'tanh')

   ! Filling types
   call tester%expect(tdrid%uniform.toBe.1.hint.'uniform')

   ! Fiiling modes
   call tester%expect(tdrid%absolute.toBe.1.hint.'absolute')
   call tester%expect(tdrid%add.toBe.2.hint.'add')

   failed = tester%failed()
end function rhyme_tiling_drawing_test
