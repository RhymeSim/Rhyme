logical function rhyme_chemistry_ne_test() result(failed)
   use rhyme_chemistry_factory
   use rhyme_physics_factory
   use rhyme_logger_factory
   use rhyme_assertion

   implicit none

   type(assertion_t) :: tester

   type(chemistry_t) :: chemistry
   type(physics_t) :: physics
   type(logger_t) :: logger

   real(kind=8) :: ne, ne_expected, density, ntr_frac(NSPE)

   tester = .describe.'chemistry_ne'

   chemistry = chemistry_factory_generate('H+He')
   physics = physics_factory_generate('SI')
   logger = logger_factory_generate('default')

   call rhyme_nombre_init()
   call rhyme_physics_init(physics, logger)

   call rhyme_chemistry_init(chemistry, physics, logger)

   density = 1.23d4

   ntr_frac(1:3) = [1, 1, 0]
   ne_expected = density/1.6735575d-18*(.75/1.00811*(1 - 1) + .25/4.002602*(2 - 2*1 - 0))
   ne = rhyme_chemistry_ne(chemistry, density, ntr_frac)
   call tester%expect(ne.toBe.ne_expected.within.7.hint.'ne neutral')

   ntr_frac(1:3) = [0, 0, 1]
   ne_expected = density/1.6735575d-18*(.75/1.00811*(1 - 0) + .25/4.002602*(2 - 2*0 - 1))
   ne = rhyme_chemistry_ne(chemistry, density, ntr_frac)
   call tester%expect(ne.toBe.ne_expected.within.7.hint.'ne ionized')

   ntr_frac(1:3) = [0.0, 0.5, 0.5]
   ne_expected = density/1.6735575d-18*(.75/1.00811*(1 - 0) + .25/4.002602*(2 - 2*0.5 - 0.5))
   ne = rhyme_chemistry_ne(chemistry, density, ntr_frac)
   call tester%expect(ne.toBe.ne_expected.within.7.hint.'ne ionized')

   failed = tester%failed()
end function rhyme_chemistry_ne_test
