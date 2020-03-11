logical function rhyme_param_parser_new_term_test() result(failed)
   use rhyme_param_parser
   use rhyme_assertion

   implicit none

   type(assertion_t) :: pp_tester

   type(config_term_t) :: term

   pp_tester = .describe."rhyme_param_parser_new_term"

   term = 'key'.at.1

   call pp_tester%expect(term%key.toBe.'key')
   call pp_tester%expect(term%location.toBe.1)

   term = 'keyword'.at.2

   call pp_tester%expect(term%key.toBe.'keyword')
   call pp_tester%expect(term%location.toBe.2)

   failed = pp_tester%failed()
end function rhyme_param_parser_new_term_test
