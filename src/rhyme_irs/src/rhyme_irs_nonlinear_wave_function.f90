submodule(rhyme_irs) rhyme_irs_nonlinear_wave_function_submodule
contains
   pure module subroutine rhyme_irs_nonlinear_wave_function( &
      state_rho, state_p, state_cs, p, f, fprime)
      implicit none

      real(kind=8), intent(in) :: state_rho, state_p, state_cs, p
      real(kind=8), intent(inout) :: f, fprime

      real(kind=8) :: factor
      real(kind=8) :: Ak, Bk

      if (p > state_p) then
         ! Shock wave
         Ak = 2.d0/(gp1*state_rho)
         Bk = gm1*state_p/gp1
         factor = sqrt(AK/(Bk + p))
         f = (p - state_p)*factor
         fprime = factor*(1.d0 - (p - state_p)/(2.d0*(Bk + p)))
      else
         ! Rarefaction wave
         f = 2.d0*state_cs/gm1*((p/state_p)**gm1_2g - 1.d0)
         fprime = 1.d0/(state_rho*state_cs)*(p/state_p)**(-gp1_2g)
      end if
   end subroutine rhyme_irs_nonlinear_wave_function
end submodule rhyme_irs_nonlinear_wave_function_submodule
