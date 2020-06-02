logical function rhyme_ionisation_equilibrium_init_test() result(failed)
   use rhyme_ionisation_equilibrium_factory
   use rhyme_units_factory
   use rhyme_chemistry_factory
   use rhyme_logger_factory
   use rhyme_nombre_assertion
   use rhyme_assertion

   implicit none

   type(assertion_t) :: tester

   type(ionisation_equilibrium_t) :: ie
   type(units_t) :: units
   type(chemistry_t) :: chemistry
   type(logger_t) :: logger

   integer :: si

   tester = .describe."ionisation_equilibrium_init"

   units = units_factory_generate('SI')
   chemistry = chemistry_factory_generate('H+He')
   logger = logger_factory_generate('default')

   ie = ionisation_equilibrium_factory_generate('CaseA-CIE')

   call rhyme_nombre_init
   call rhyme_units_init(units, logger)
   call rhyme_chemistry_init(chemistry, units, logger)

   call rhyme_ionisation_equilibrium_init(ie, units, chemistry, logger)

   do si = 1, NSPE
      call tester%expect(associated(ie%species(si)%RI) .toBe..true..hint.'ie RI')
      call tester%expect(associated(ie%species(si)%CI) .toBe..true..hint.'ie CI')
      call tester%expect(associated(ie%species(si)%CIE) .toBe..true..hint.'ie CIE')
      call tester%expect(associated(ie%species(si)%CPIE) .toBe..true..hint.'ie CPIE')
   end do

   call tester%expect(ie%table_temp_range(1)%u.toBe.units%temperature.hint.'temp unit')
   call tester%expect(ie%table_temp_range(2)%u.toBe.units%temperature.hint.'temp unit')
   call tester%expect(ie%log_temp_min.toBe.log10(ie%table_temp_range(1)%v) .hint.'log temp_min')
   call tester%expect(ie%log_temp_max.toBe.log10(ie%table_temp_range(2)%v) .hint.'log temp_max')
   call tester%expect( &
      ie%dlog_temp.toBe. &
      ((log10(ie%table_temp_range(2)%v) - log10(ie%table_temp_range(1)%v))/ie%table_sizes(1)) &
      .hint.'d(log(temp))')
   call tester%expect( &
      10**(ie%log_temp_min + ie%table_sizes(1)*ie%dlog_temp) .toBe. &
      (ie%table_temp_range(2)%v) .within.15 &
      .hint.'dlog_temp compare to temp range')

   call tester%expect(ie%table_density_range(1)%u.toBe.units%rho.hint.'density unit')
   call tester%expect(ie%table_density_range(2)%u.toBe.units%rho.hint.'density unit')
   call tester%expect(ie%log_density_min.toBe.log10(ie%table_density_range(1)%v) .hint.'log density_min')
   call tester%expect(ie%log_density_max.toBe.log10(ie%table_density_range(2)%v) .hint.'log density_max')
   call tester%expect( &
      ie%dlog_density.toBe. &
      ((log10(ie%table_density_range(2)%v) - log10(ie%table_density_range(1)%v))/ie%table_sizes(1)) &
      .hint.'d(log(density))')
   call tester%expect( &
      10**(ie%log_density_min + ie%table_sizes(1)*ie%dlog_density) .toBe. &
      (ie%table_density_range(2)%v) .within.15 &
      .hint.'dlog_density compare to density range')

   failed = tester%failed()
end function rhyme_ionisation_equilibrium_init_test
