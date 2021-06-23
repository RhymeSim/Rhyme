logical function rhyme_irs_guess_p_star_test() result(failed)
   use rhyme_irs_factory
   use rhyme_units_factory
   use rhyme_thermo_base_factory
   use rhyme_logger_factory
   use rhyme_assertion

   implicit none

   type(assertion_t) :: tester

   type(irs_t) :: irs
   type(units_t) :: units
   type(thermo_base_t) :: thermo
   type(logger_t) :: logger

   real(kind=8) :: l_rho, l_v(NDIM), l_p, l_cs, r_rho, r_v(NDIM), r_p, r_cs
   real(kind=8) :: p_star(6)
   real(kind=8), dimension(1 + NDIM + 1 + 1) :: rnd1, rnd2
   integer :: i, axis

   tester = .describe.'irs_guess_p_star'

   irs = irs_factory_generate('default')
   units = units_factory_generate('SI')
   logger = logger_factory_generate('default')

   call rhyme_nombre_init()
   thermo = thermo_base_factory_generate('monatomic')
   call rhyme_thermo_base_init(thermo, units, logger)

   call rhyme_irs_init(irs, thermo, logger)

   do i = 1, 10
      call random_number(rnd1)
      call random_number(rnd2)

      l_rho = rnd1(1)
      l_v = rnd1(2:2 + NDIM - 1)
      l_p = rnd1(2 + NDIM)
      l_cs = rnd1(2 + NDIM + 1)

      r_rho = rnd2(1)
      r_v = rnd2(2:2 + NDIM - 1)
      r_p = rnd2(2 + NDIM)
      r_cs = rnd2(2 + NDIM + 1)

      do axis = 1, NDIM
         p_star = rhyme_irs_guess_p_star( &
                  l_rho, l_v, l_p, l_cs, r_rho, r_v, r_p, r_cs, axis)
         call tester%expect(all((p_star + tiny(0d0)) > 0d0) .toBe..true.)
      end do
   end do

   failed = tester%failed()
end function rhyme_irs_guess_p_star_test
