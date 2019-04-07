logical function rhyme_assertion_test () result ( failed )
  use rhyme_assertion

  implicit none

  failed = &
  assertid%int .ne. 1 .or. assertid%real .ne. 2 &
  .or. assertid%double .ne. 3 .or. assertid%char .ne. 4 &
  .or. assertid%int_arr .ne. 11 .or. assertid%real_arr .ne. 12 &
  .or. assertid%double_arr .ne. 13 .or. assertid%char_arr .ne. 14 &
  .or. assertid%unset .ne. -1
end function rhyme_assertion_test
