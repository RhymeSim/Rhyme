logical function rhyme_stabilizer_test() result(failed)
   use rhyme_stabilizer_factory
   use rhyme_assertion

   implicit none

   type(assertion_t) :: tester

   type(stabilizer_t) :: st

   tester = .describe."stabilizer"

   st = stabilizer_t()

   call tester%expect(st%enabled.toBe..false..hint.'Enabled')
   call tester%expect(st%weight.toBe.cid%rho.hint.'Weight')
   call tester%expect(st%weight_power.toBe.1.hint.'Weight power')
   call tester%expect(st%extrapolation_type.toBe.stid%none.hint.'Extrapolation type')
   call tester%expect(st%max_displacement.toBe.1.hint.'Maxiimum displacement')
   call tester%expect(st%tolerance.toBe.1d1.hint.'Tolerance')
   call tester%expect(st%max_frequency.toBe.1.hint.'Maxiimum frequency')
   call tester%expect(st%next_timestep.toBe.0.hint.'Next timestep')
   call tester%expect(st%initialize_target.toBe..true..hint.'Initialize target')
   call tester%expect(st%target_center.toBe.0d0.hint.'Target center')

   call tester%expect(stid%unset.toBe.-1.hint.'unset')
   call tester%expect(stid%none.toBe.1.hint.'None')
   call tester%expect(stid%linear.toBe.2.hint.'Linear')
   call tester%expect(stid%quadratic.toBe.3.hint.'Quadratic')

   failed = tester%failed()
end function rhyme_stabilizer_test
