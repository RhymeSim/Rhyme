logical function rhyme_deep_rs_exec_test() result(failed)
   use rhyme_deep_rs_factory
   use rhyme_units_factory
   use rhyme_logger_factory
   use rhyme_assertion

   implicit none

   type(assertion_t) :: tester

   type(deep_rs_t) :: drs
   type(units_t) :: units
   type(logger_t) :: logger

   real(kind=8) :: r_r, u_r, p_r, r_l, u_l, p_l
   real(kind=8) :: p_star, exp_p_star
   integer :: i, iostat

   tester = .describe."deep_rs_exec"

   units = units_factory_generate('SI')
   logger = logger_factory_generate('default')

   call rhyme_nombre_init
   call rhyme_units_init(units, logger)

   call rhyme_deep_rs_init(drs, units, logger)

   p_star = rhyme_deep_rs_exec( &
            drs, 1d0, 8.63360831d-11, 1d0, 8.63360831d-11, 0d0)

   call tester%expect(p_star.toBe.1.4896335759294049E-013)

   open (1002, file='p_stars.txt', access='sequential', form='formatted', action='read')
   read (1002, *)

   do i = 1, 10
      read (1002, *, iostat=iostat) r_l, u_l, p_l, r_r, u_r, p_r, exp_p_star
      if (iostat /= 0) exit

      p_star = rhyme_deep_rs_exec(drs, r_l, p_l, r_r, p_r, u_r - u_l)

      call tester%expect(.notToBeNaN.p_star)
      call tester%expect(p_star.toBe.exp_p_star)
   end do

   close (1002)

   failed = tester%failed()
end function rhyme_deep_rs_exec_test
