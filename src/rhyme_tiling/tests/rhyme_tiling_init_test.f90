logical function rhyme_tiling_init_test() result(failed)
   use rhyme_tiling_factory
   use rhyme_logger_factory
   use rhyme_assertion

   implicit none

   type(assertion_t) :: tester

   type(tiling_t) :: tile
   type(logger_t) :: logger

   tester = .describe."tiling_init"

   tile = tiling_factory_generate('3levels')
   logger = logger_factory_generate('default')

   call rhyme_tiling_init(tile, logger)

   call tester%expect(allocated(tile%cells) .toBe..true..hint.'Cells')
   call tester%expect(associated(tile%tiles) .toBe..true..hint.'Tiles')
   call tester%expect(allocated(tile%is_allocated) .toBe..true..hint.'is_allocated')

   failed = tester%failed()
end function rhyme_tiling_init_test
