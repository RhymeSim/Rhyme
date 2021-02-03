logical function rhyme_samr_calc_total_energy_test() result(failed)
   use rhyme_samr_factory
   use rhyme_assertion

   implicit none

   type(assertion_t) :: tester

   type(samr_t) :: samr
   real(kind=8) :: total_energy, total_energy_exp

   tester = .describe."samr_calc_total_energy"

   samr = samr_factory%generate(physical=.true.)

   total_energy = rhyme_samr_calc_total_energy(samr)

#if NDIM == 1
#define JDX
#define KDX
#endif
#if NDIM == 2
#define JDX , :
#define KDX
#endif
#if NDIM == 3
#define JDX , :
#define KDX , :
#endif

   total_energy_exp = sum(samr%levels(0)%boxes(1)%cells(:JDX KDX, cid%e_tot))

   call tester%expect(total_energy.toBe.total_energy_exp*product(samr%box_lengths) .hint.'total energy')

   failed = tester%failed()
end function rhyme_samr_calc_total_energy_test
