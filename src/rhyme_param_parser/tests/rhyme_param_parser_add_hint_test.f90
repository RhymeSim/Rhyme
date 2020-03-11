logical function rhyme_param_parser_add_hint_test() result(failed)
   use rhyme_param_parser
   use rhyme_assertion

   implicit none

   type(assertion_t) :: pp_tester

   type(config_term_t) :: term, nterm

   pp_tester = .describe."rhyme_param_parser_add_hint"

   call pp_tester%expect(term%hint.toBe.'')

   nterm = term.hint.'hint'

   call pp_tester%expect(nterm%hint.toBe.'hint')

   failed = pp_tester%failed()
end function rhyme_param_parser_add_hint_test
