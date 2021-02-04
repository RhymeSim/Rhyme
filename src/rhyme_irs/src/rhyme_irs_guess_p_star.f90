submodule(rhyme_irs) rhyme_irs_guess_p_star_submodule
contains
   pure module function rhyme_irs_guess_p_star(l, r, axis, p_vacuum) result(p_star)
      implicit none

      type(rp_side_t), intent(in) :: l, r
      integer, intent(in) :: axis
      real(kind=8), intent(in) :: p_vacuum
      real(kind=8) :: p_star(5)

      real(kind=8) :: l_g, r_g

      ! Ramses guess
      p_star(1) = ( &
                  r%rho*r%cs*l%p &
                  + l%rho*l%cs*r%p &
                  + r%cs*l%cs*(l%rho*l%v(axis) - r%rho*r%v(axis)) &
                  )/(r%rho*r%cs + l%rho*l%cs)

      ! Two rarefaction approximation
      p_star(2) = ( &
                  ( &
                  l%cs + r%cs - .5d0*gm1*(r%v(axis) - l%v(axis)) &
                  )/( &
                  (l%cs/l%p)**gm1_2g + (r%cs/r%p)**gm1_2g &
                  ) &
                  )**real(1.d0/gm1_2g, kind=8)

      ! Two shock approximation
      l_g = rhyme_irs_guess_p_star_g_K(l)
      r_g = rhyme_irs_guess_p_star_g_K(r)
      p_star(3) = (l_g*l%p + r_g*r%p - (r%v(axis) - l%v(axis)))/(l_g + r_g)

      ! p_PV
      p_star(4) = .5d0*(l%p + r%p) &
                  - .125d0*(r%v(axis) - l%v(axis))*(l%rho + r%rho)*(l%cs + r%cs)

      ! Arithmetic mean
      p_star(5) = .5d0*(l%p + r%p)

      ! NB: Here we are enforcing a pressure floor for the guessed p_stars
      ! where( p_star < 0d0 ) p_star = p_vacuum might be a better and more conservative approach
      p_star = max(p_star, p_vacuum)
   end function rhyme_irs_guess_p_star

   real(kind=8) pure function rhyme_irs_guess_p_star_g_K(s) result(g_K)
      implicit none

      type(rp_side_t), intent(in) :: s

      g_K = sqrt((2.d0/(gp1*s%rho))/(s%p + gm1_gp1*s%p))
   end function rhyme_irs_guess_p_star_g_K
end submodule rhyme_irs_guess_p_star_submodule
