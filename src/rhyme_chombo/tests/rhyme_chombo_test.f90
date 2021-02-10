logical function rhyme_chombo_test() result(failed)
   use rhyme_chombo
   use rhyme_assertion

   implicit none

   type(assertion_t) :: ch_tester

   type(chombo_t) :: ch

   ch_tester = .describe."chombo"

   call ch_tester%expect(chid%unset.toBe.h5id%unset)
   call ch_tester%expect(ch%is_opened.toBe..false.)
   call ch_tester%expect(ch%iteration.toBe.chid%unset)
   call ch_tester%expect(ch%num_levels.toBe.chid%unset)
   call ch_tester%expect(ch%num_components.toBe.chid%unset)
   call ch_tester%expect(int(ch%level_ids) .toBe.chid%unset)
   call ch_tester%expect(ch%file%initialized.toBe..false.)

   call ch_tester%expect(chid%boxes_headers.toBe. ['lo_i', 'lo_j', 'lo_k', 'hi_i', 'hi_j', 'hi_k'])
   call ch_tester%expect(chid%unset.toBe.h5id%unset)
   call ch_tester%expect(chid%log.toBe.100)
   call ch_tester%expect(chid%linear.toBe.101)

   failed = ch_tester%failed()
end function rhyme_chombo_test
