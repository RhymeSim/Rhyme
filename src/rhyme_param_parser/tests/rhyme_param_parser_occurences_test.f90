logical function rhyme_param_parser_occurences_test() result(failed)
   use rhyme_param_parser
   use rhyme_assertion

   implicit none

   type(assertion_t) :: pp_tester

   type(config_t) :: config
   integer :: n_occur

   pp_tester = .describe."rhyme_param_parser_occurences"

   call config%init('occurences.txt')
   n_occur = config%occur('key1')

   call pp_tester%expect(n_occur.toBe.3)

   failed = pp_tester%failed()
end function rhyme_param_parser_occurences_test
