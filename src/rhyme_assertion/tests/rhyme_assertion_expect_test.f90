logical function rhyme_assertion_expect_test() result(failed)
   use rhyme_assertion

   implicit none

   type(assertion_t) :: tester
   type(test_t) :: test1, test2

   test1%is_passed = .false.
   test1%type = 1234

   test2%is_passed = .true.
   test2%type = 4321

   tester = .describe.'tester'
   call tester%expect(test1)
   call tester%expect(test2)

   failed = &
      tester%tests%is_passed &
      .or. tester%tests%type .ne. 1234 &
      .or. .not. tester%tests%next%is_passed &
      .or. tester%tests%next%type .ne. 4321
end function rhyme_assertion_expect_test
