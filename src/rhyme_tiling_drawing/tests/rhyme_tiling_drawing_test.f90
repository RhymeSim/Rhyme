logical function rhyme_tiling_drawing_test() result(failed)
   use rhyme_tiling_drawing_factory
   use rhyme_assertion

   implicit none

   type(assertion_t) :: tester

   type(tiling_drawing_t) :: draw

   tester = .describe."tiling_drawing"

   draw = tiling_drawing_factory_generate('default')

   call tester%expect(tdrawid%idx.toBe.0.hint.'Placeholder test')

   failed = tester%failed()
end function rhyme_tiling_drawing_test
