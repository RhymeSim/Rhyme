logical function rhyme_param_parser_add_switch_test() result(failed)
   use rhyme_param_parser
   use rhyme_assertion

   implicit none

   type(assertion_t) :: pp_tester

   type(config_switch_t) :: switch

   pp_tester = .describe."rhyme_param_parser_add_switch"

   call pp_tester%expect(switch%len.toBe.0)

   call switch%add('key_1', 1234)

   call pp_tester%expect(switch%len.toBe.1)
   call pp_tester%expect(switch%types(1) .toBe.'int')
   call pp_tester%expect(switch%keys(1) .toBe.'key_1')
   call pp_tester%expect(switch%int_values(1) .toBe.1234)

   call switch%add('key_2', .true.)

   call pp_tester%expect(switch%len.toBe.2)
   call pp_tester%expect(switch%types(2) .toBe.'log')
   call pp_tester%expect(switch%keys(2) .toBe.'key_2')
   call pp_tester%expect(switch%log_values(2) .toBe..true.)

   failed = pp_tester%failed()
end function rhyme_param_parser_add_switch_test
