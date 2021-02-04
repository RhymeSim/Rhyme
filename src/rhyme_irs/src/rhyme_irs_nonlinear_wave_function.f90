submodule(rhyme_irs) rhyme_irs_nonlinear_wave_function_submodule
contains
   pure module subroutine rhyme_irs_nonlinear_wave_function(state, p, star)
      implicit none

      type(rp_side_t), intent(in) :: state
      real(kind=8), intent(in) :: p
      type(rp_star_side_t), intent(inout) :: star

      real(kind=8) :: factor
      real(kind=8) :: Ak, Bk

      Ak = 2.d0/(gp1*state%rho)
      Bk = gm1*state%p/gp1

      if (p > state%p) then
         factor = sqrt(AK/(Bk + p))
         star%f = (p - state%p)*factor
         star%fprime = factor*(1.d0 - (p - state%p)/(2.d0*(Bk + p)))
      else
         star%f = 2.d0*state%cs/gm1*((p/state%p)**gm1_2g - 1.d0)
         star%fprime = 1.d0/(state%rho*state%cs)*(p/state%p)**(-gp1_2g)
      end if
   end subroutine rhyme_irs_nonlinear_wave_function
end submodule rhyme_irs_nonlinear_wave_function_submodule
