logical function rhyme_ionisation_equilibrium_pick_test() result(failed)
   use rhyme_ionisation_equilibrium_factory
   use rhyme_units_factory
   use rhyme_chemistry_factory
   use rhyme_uv_background_factory
   use rhyme_logger_factory
   use rhyme_assertion

   implicit none

   type(assertion_t) :: tester

   type(ionisation_equilibrium_t) :: ie
   type(units_t) :: units
   type(chemistry_t) :: chemistry
   type(uv_background_t) :: uvb
   type(logger_t) :: logger

   real(kind=8) :: z, ntr_frac(NSPE)
   real(kind=8) :: mid_temp, mid_density
   integer :: tend, dend

   tester = .describe.'ionisation_equilibrium_pick'

   ie = ionisation_equilibrium_factory_generate('CaseA-CPIE')
   units = units_factory_generate('SI')
   chemistry = chemistry_factory_generate('H+He')
   uvb = uv_background_factory_generate('HM12')
   logger = logger_factory_generate('default')

   z = 1.37d0

   call rhyme_nombre_init
   call rhyme_color_init
   call rhyme_units_init(units, logger)
   call rhyme_chemistry_init(chemistry, units, logger)
   call rhyme_uv_background_init(uvb, units, logger)
   call rhyme_ionisation_equilibrium_init(ie, units, chemistry, logger)
   call rhyme_ionisation_equilibrium_update_table(ie, chemistry, uvb, z, logger)

   tend = ie%table_sizes(1)
   dend = ie%table_sizes(2)

   ntr_frac = rhyme_ionisation_equilibrium_pick(ie, ie%table_temp_range(1)%v, ie%table_density_range(1)%v)
   call tester%expect(ntr_frac.toBe.ie%table(:, 1, 1) .hint.'bottom left')

   ntr_frac = rhyme_ionisation_equilibrium_pick(ie, ie%table_temp_range(1)%v, ie%table_density_range(2)%v)
   call tester%expect(ntr_frac.toBe.ie%table(:, 1, dend) .hint.'bottom right')

   ntr_frac = rhyme_ionisation_equilibrium_pick(ie, ie%table_temp_range(2)%v, ie%table_density_range(1)%v)
   call tester%expect(ntr_frac.toBe.ie%table(:, tend, 1) .hint.'top left')

   ntr_frac = rhyme_ionisation_equilibrium_pick(ie, ie%table_temp_range(2)%v, ie%table_density_range(2)%v)
   call tester%expect(ntr_frac.toBe.ie%table(:, tend, dend) .hint.'top right')

   mid_temp = &
      10**(log10(ie%table_temp_range(1)%v) &
           + ( &
           log10(ie%table_temp_range(2)%v) &
           - log10(ie%table_temp_range(1)%v) &
           )/2 &
           )
   mid_density = &
      10**(log10(ie%table_density_range(1)%v) &
           + ( &
           log10(ie%table_density_range(2)%v) &
           - log10(ie%table_density_range(1)%v) &
           )/2 &
           )
   ntr_frac = rhyme_ionisation_equilibrium_pick(ie, mid_temp, mid_density)
   call tester%expect(ntr_frac.toBe.ie%table(:, tend/2, dend/2) .hint.'mid point')

   ntr_frac = rhyme_ionisation_equilibrium_pick(ie, mid_temp, ie%table_density_range(1)%v)
   call tester%expect(ntr_frac.toBe.ie%table(:, tend/2, 1) .hint.'mid point')

   ntr_frac = rhyme_ionisation_equilibrium_pick(ie, mid_temp, ie%table_density_range(2)%v)
   call tester%expect(ntr_frac.toBe.ie%table(:, tend/2, dend) .hint.'mid point')

   ntr_frac = rhyme_ionisation_equilibrium_pick(ie, ie%table_temp_range(1)%v, mid_density)
   call tester%expect(ntr_frac.toBe.ie%table(:, 1, dend/2) .hint.'mid point')

   ntr_frac = rhyme_ionisation_equilibrium_pick(ie, ie%table_temp_range(2)%v, mid_density)
   call tester%expect(ntr_frac.toBe.ie%table(:, tend, dend/2) .hint.'mid point')

   failed = tester%failed()
end function rhyme_ionisation_equilibrium_pick_test
