logical function rhyme_irs_exec_test() result(failed)
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

   integer :: iostat, i
   real(kind=8) :: r_r, u_r, p_r, cs_r, f_r, r_l, u_l, p_l, cs_l, f_l
   real(kind=8) :: p_star, exp_p_star
   real(kind=8) :: v_r(NDIM), v_l(NDIM)

   tester = .describe."irs_exec"

   irs = irs_factory_generate('default')

   irs = irs_factory_generate('default')
   units = units_factory_generate('SI')
   logger = logger_factory_generate('default')

   call rhyme_nombre_init()
   thermo = thermo_base_factory_generate('monatomic')
   call rhyme_thermo_base_init(thermo, units, logger)

   call rhyme_irs_init(irs, thermo, logger)

   irs%n_iteration = 100000
   irs%tolerance = 1d-10

   open (1001, file='p_stars.txt', access='sequential', form='formatted', action='read')
   read (1001, *)

   do i = 1, 1000
      read (1001, *, iostat=iostat) r_l, u_l, p_l, r_r, u_r, p_r, exp_p_star
      if (iostat /= 0) exit

      v_r = 0d0
      v_l = 0d0
      v_r(1) = u_r
      v_l(1) = u_l

      cs_r = sqrt(5./3.*p_r/r_r)
      cs_l = sqrt(5./3.*p_l/r_l)

      p_star = rhyme_irs_exec( &
               irs, r_l, v_l, p_l, cs_l, f_l, r_r, v_r, p_r, cs_r, f_r, 1)

      call tester%expect(p_star.toBe.exp_p_star.within.6)
   end do

   close (1001)

   failed = tester%failed()
end function rhyme_irs_exec_test
