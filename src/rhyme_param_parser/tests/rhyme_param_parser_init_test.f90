logical function rhyme_param_parser_init_test() result(failed)
   use rhyme_param_parser
   use rhyme_assertion

   implicit none

   type(assertion_t) :: pp_tester

   character(len=32), parameter :: path = "/path/to/param/file"

   type(config_t) :: cfg

   pp_tester = .describe."rhyme_param_parser_init"

   call cfg%init(path)

   call pp_tester%expect(cfg%path.toBe.path)

   failed = pp_tester%failed()
end function rhyme_param_parser_init_test
