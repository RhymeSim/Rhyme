logical function rhyme_tiling_drawing_init_test() result(failed)
   use rhyme_tiling_drawing_factory
   use rhyme_logger_factory
   use rhyme_assertion

   implicit none

   type(assertion_t) :: tester

   type(tiling_drawing_t) :: draw
   type(logger_t) :: logger

   tester = .describe."tiling_drawing_init"

   draw = tiling_drawing_factory_generate('default')
   logger = logger_factory_generate('default')

   call rhyme_tiling_drawing_init(draw, logger)

   call tester%expect(.false..toBe..true..hint.'Placeholder test')

   failed = tester%failed()
end function rhyme_tiling_drawing_init_test
