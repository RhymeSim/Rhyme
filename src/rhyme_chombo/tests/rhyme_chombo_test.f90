logical function rhyme_chombo_test() result(failed)
   use rhyme_chombo
   use rhyme_assertion

   implicit none

   type(assertion_t) :: tester

   type(chombo_t) :: ch

   tester = .describe."chombo"

   call tester%expect(chid%unset.toBe.h5id%unset)
   call tester%expect(ch%is_opened.toBe..false.)
   call tester%expect(ch%iteration.toBe.chid%unset)
   call tester%expect(ch%num_levels.toBe.chid%unset)
   call tester%expect(ch%num_components.toBe.chid%unset)
   call tester%expect(int(ch%level_ids) .toBe.chid%unset)
   call tester%expect(ch%file%initialized.toBe..false.)

   call tester%expect(chid%boxes_headers.toBe. ['lo_i', 'lo_j', 'lo_k', 'hi_i', 'hi_j', 'hi_k'])
   call tester%expect(chid%unset.toBe.h5id%unset)
   call tester%expect(chid%log.toBe.100)
   call tester%expect(chid%linear.toBe.101)

   failed = tester%failed()
end function rhyme_chombo_test
