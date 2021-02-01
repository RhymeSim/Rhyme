logical function rhyme_sanity_check_test() result(failed)
   use rhyme_sanity_check_factory
   use rhyme_assertion

   implicit none

   type(assertion_t) :: tester

   type(sanity_check_t) :: sc

   tester = .describe."sanity_check"

   sc = sanity_check_factory_generate('default')

   call tester%expect(scid%rho.toBe.1.hint.'rho id')
   call tester%expect(scid%vx.toBe.2.hint.'vx id')
   call tester%expect(scid%vy.toBe.3.hint.'vy id')
   call tester%expect(scid%vz.toBe.4.hint.'vz id')
   call tester%expect(scid%e_tot.toBe.5.hint.'e_tot id')
   call tester%expect(scid%temp.toBe.6.hint.'temp id')
   call tester%expect(scid%ntr_frac_0.toBe.7.hint.'ntr_frac_0 id')
   call tester%expect(scid%ntr_frac_1.toBe.8.hint.'ntr_frac_1 id')
   call tester%expect(scid%ntr_frac_2.toBe.9.hint.'ntr_frac_2 id')
   call tester%expect(scid%total_mass.toBe.10.hint.'total_mass id')
   call tester%expect(scid%total_energy.toBe.11.hint.'total_energy id')
   call tester%expect(scid%abs_v.toBe.12.hint.'|v| id')
   call tester%expect(scid%cs.toBe.13.hint.'cs id')

   failed = tester%failed()
end function rhyme_sanity_check_test
