logical function rhyme_deep_rs_test() result(failed)
   use rhyme_deep_rs_factory
   use rhyme_assertion

   implicit none

   type(assertion_t) :: tester

   type(deep_rs_t) :: drs

   tester = .describe."deep_rs"

   drs = deep_rs_factory_generate('default')

   call tester%expect(drs%drho.toBe.0e0.hint.'drho')
   call tester%expect(drs%dp.toBe.0e0.hint.'dp')
   call tester%expect(drs%dv.toBe.0e0.hint.'dv')
   call tester%expect(drs%path.toBe.'./v20210502.h5'.hint.'path')
   call tester%expect(drs%d_norm.toBe.''.hint.'d_norm')
   call tester%expect(drs%p_norm.toBe.''.hint.'p_norm')
   call tester%expect(drs%v_norm.toBe.''.hint.'v_norm')

   failed = tester%failed()
end function rhyme_deep_rs_test
