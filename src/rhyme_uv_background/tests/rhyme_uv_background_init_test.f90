logical function rhyme_uv_background_init_test() result(failed)
   use rhyme_uv_background_factory
   use rhyme_logger_factory
   use rhyme_assertion

   implicit none

   type(assertion_t) :: tester

   type(uv_background_t) :: uvb
   type(logger_t) :: logger

   tester = .describe."uv_background_init"

   uvb = uv_background_factory_generate('HM')
   logger = logger_factory_generate('default')

   call rhyme_uv_background_init(uvb, logger)

   call tester%expect(.false..toBe..true..hint.'Placeholder test')

   failed = tester%failed()
end function rhyme_uv_background_init_test
