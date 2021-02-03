logical function rhyme_samr_calc_total_mass_test() result(failed)
   use rhyme_samr_factory
   use rhyme_assertion

   implicit none

   type(assertion_t) :: tester

   type(samr_t) :: samr
   real(kind=8) :: total_mass, total_mass_exp

   tester = .describe."samr_calc_total_mass"

   samr = samr_factory%generate(physical=.true.)

   total_mass = rhyme_samr_calc_total_mass(samr)

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

   total_mass_exp = sum(samr%levels(0)%boxes(1)%cells(:JDX KDX, cid%rho))

   call tester%expect(total_mass.toBe.total_mass_exp*product(samr%box_lengths) .hint.'total mass')

   failed = tester%failed()
end function rhyme_samr_calc_total_mass_test
