logical function rhyme_param_parser_add_occur_test() result(failed)
   use rhyme_param_parser
   use rhyme_assertion

   implicit none

   type(assertion_t) :: pp_tester

   type(config_term_t) :: term, nterm

   pp_tester = .describe."rhyme_param_parser_add_occur"

   call pp_tester%expect(term%occurence.toBe.1)

   nterm = term.occur.4

   call pp_tester%expect(nterm%occurence.toBe.4)

   failed = pp_tester%failed()
end function rhyme_param_parser_add_occur_test
