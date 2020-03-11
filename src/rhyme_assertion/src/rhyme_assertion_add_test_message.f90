submodule(rhyme_assertion) rhyme_assertion_add_test_message_submodule
contains
pure module function rhyme_assertion_add_test_message(t, msg) result(test)
   implicit none

   type(test_t), intent(in) :: t
   character(len=*), intent(in) :: msg

   type(test_t) :: test

   test%msg = trim(adjustl(msg))
   test%type = t%type
   test%is_passed = t%is_passed
   test%val = t%val
   test%op = t%op
   test%exp = t%exp
end function rhyme_assertion_add_test_message
end submodule rhyme_assertion_add_test_message_submodule
