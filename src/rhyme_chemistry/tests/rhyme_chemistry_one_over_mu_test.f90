logical function rhyme_chemistry_one_over_mu_test() result(failed)
   use rhyme_chemistry_factory
   use rhyme_physics_factory
   use rhyme_logger_factory
   use rhyme_assertion

   implicit none

   type(assertion_t) :: tester

   type(chemistry_t) :: chemistry
   type(physics_t) :: physics
   type(logger_t) :: logger

   real(kind=8) :: one_over_mu, one_over_mu_expected, ntr_frac(NSPE)

   tester = .describe.'chemistry_one_over_mu'

   chemistry = chemistry_factory_generate('H+He')
   physics = physics_factory_generate('SI')
   logger = logger_factory_generate('default')

   call rhyme_nombre_init()
   call rhyme_physics_init(physics, logger)

   call rhyme_chemistry_init(chemistry, physics, logger)

#if NSPE > 2
   ntr_frac(1:3) = [1, 1, 0]

   one_over_mu_expected = &
      chemistry%element_abundances(1)/chemistry%elements(1)%atomic_weight &
      *(1 + 1*(1 - 1)) + &
      chemistry%element_abundances(2)/chemistry%elements(2)%atomic_weight &
      *(1 + 1*(0) + 2*(1 - 1 - 0))

   one_over_mu = rhyme_chemistry_one_over_mu(chemistry, ntr_frac)
   call tester%expect(one_over_mu.toBe.one_over_mu_expected.hint.'one_over_mu neutral')

   ntr_frac(1:3) = [0, 0, 1]

   one_over_mu_expected = &
      chemistry%element_abundances(1)/chemistry%elements(1)%atomic_weight &
      *(1 + 1*(1 - 0)) + &
      chemistry%element_abundances(2)/chemistry%elements(2)%atomic_weight &
      *(1 + 1*(1) + 2*(1 - 0 - 1))

   one_over_mu = rhyme_chemistry_one_over_mu(chemistry, ntr_frac)
   call tester%expect(one_over_mu.toBe.one_over_mu_expected.hint.'one_over_mu fully ionized I')

   ntr_frac(1:3) = [0, 0, 0]

   one_over_mu_expected = &
      chemistry%element_abundances(1)/chemistry%elements(1)%atomic_weight &
      *(1 + 1*(1 - 0)) + &
      chemistry%element_abundances(2)/chemistry%elements(2)%atomic_weight &
      *(1 + 1*(0) + 2*(1 - 0 - 0))

   one_over_mu = rhyme_chemistry_one_over_mu(chemistry, ntr_frac)
   call tester%expect(one_over_mu.toBe.one_over_mu_expected.hint.'one_over_mu fully ionized II')

   ntr_frac(1:3) = [0.0, 0.5, 0.25]

   one_over_mu_expected = &
      chemistry%element_abundances(1)/chemistry%elements(1)%atomic_weight &
      *(1 + 1*(1 - 0)) + &
      chemistry%element_abundances(2)/chemistry%elements(2)%atomic_weight &
      *(1 + 1*(0.25) + 2*(1 - 0.5 - 0.25))

   one_over_mu = rhyme_chemistry_one_over_mu(chemistry, ntr_frac)
   call tester%expect(one_over_mu.toBe.one_over_mu_expected.hint.'one_over_mu fully ionized II')
#endif

   failed = tester%failed()
end function rhyme_chemistry_one_over_mu_test
