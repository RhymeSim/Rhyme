logical function rhyme_chombo_output_new_rule_test() result(failed)
   use rhyme_chombo_factory
   use rhyme_assertion

   implicit none

   type(assertion_t) :: tester

   type(chombo_output_t) :: ch_output

   tester = .describe.'chombo output new rule'

   ch_output = chombo_factory_generate_output('log')

   call tester%expect(ch_output%rules%type.toBe.chid%log.hint.'log')
   call tester%expect(ch_output%rules%next%type.toBe.chid%log.hint.'log')

   failed = tester%failed()
end function rhyme_chombo_output_new_rule_test
