logical function rhyme_chombo_output_new_rule_test() result(failed)
   use rhyme_chombo_factory
   use rhyme_assertion

   implicit none

   type(assertion_t) :: tester

   type(chombo_output_t) :: ch_output
   type(chombo_output_rule_t), pointer :: rule

   tester = .describe.'chombo output new rule'

   rule => ch_output%new_rule(chid%log)
   call tester%expect(ch_output%rules%type.toBe.chid%log.hint.'log')
   call tester%expect(rule%type.toBe.chid%log.hint.'log')

   rule => ch_output%new_rule(chid%linear)
   call tester%expect(ch_output%rules%next%type.toBe.chid%linear.hint.'linear')
   call tester%expect(rule%type.toBe.chid%linear.hint.'linear')

   failed = tester%failed()
end function rhyme_chombo_output_new_rule_test
