submodule(rhyme_irs) exec_submodule
contains
   module function rhyme_irs_exec( &
      irs, l_rho, l_v, l_p, l_cs, l_f, r_rho, r_v, r_p, r_cs, r_f, axis) result(p_star)
      implicit none

      type(irs_t), intent(in) :: irs
      real(kind=8), intent(in) :: &
         l_rho, l_v(NDIM), l_p, l_cs, r_rho, r_v(NDIM), r_p, r_cs
      real(kind=8), intent(inout) :: l_f, r_f
      integer, intent(in) :: axis
      real(kind=8) :: p_star

      real(kind=8) :: p_star_prev, guessed_p(6)
      real(kind=8) :: l_fprime, r_fprime

      integer :: i, guess_id

      guessed_p = rhyme_irs_guess_p_star( &
                  l_rho, l_v, l_p, l_cs, &
                  r_rho, r_v, r_p, r_cs, &
                  axis)

      do guess_id = 1, size(guessed_p)
         p_star = guessed_p(guess_id)
         p_star_prev = p_star

         do i = 1, irs%n_iteration
            call rhyme_irs_nonlinear_wave_function(l_rho, l_p, l_cs, p_star, l_f, l_fprime)
            call rhyme_irs_nonlinear_wave_function(r_rho, r_p, r_cs, p_star, r_f, r_fprime)

            p_star = p_star_prev - (l_f + r_f + (r_v(axis) - l_v(axis)))/(l_fprime + r_fprime)

            if (p_star < 0.d0) exit

            if (2*abs(p_star - p_star_prev)/(p_star + p_star_prev) < irs%tolerance) exit

            p_star_prev = p_star
         end do

         if (p_star > 0.d0) exit
      end do

      if (p_star < 0.d0) p_star = 0d0
   end function rhyme_irs_exec
end submodule exec_submodule
