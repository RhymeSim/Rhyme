submodule(rhyme_irs) rhyme_irs_guess_p_star_submodule
contains
   pure module function rhyme_irs_guess_p_star(l, r, axis, p_vacuum) result(p_star)
      implicit none

      type(rp_side_t), intent(in) :: l, r
      integer, intent(in) :: axis
      real(kind=8), intent(in) :: p_vacuum
      real(kind=8) :: p_star(6)

      real(kind=8) :: l_g, r_g

      real(kind=8) :: q_user, cup, ppv, p_min, p_max, q_max, pq, um, ptl, ptr, gel, ger

      q_user = 2d0

      cup = 0.25*(l%rho + r%rho)*(l%cs + r%cs)
      ppv = max(0.5*(l%p + r%p) + 0.5*(l%v(axis) - r%v(axis))*cup, 0d0)
      p_min = min(l%p, r%p)
      p_max = max(l%p, r%p)
      q_max = p_max/p_min

      if (q_max <= q_user .and. (p_min <= ppv .and. ppv <= p_max)) then
         ! PVRS Riemann solver
         p_star(1) = ppv
      else
         if (ppv <= p_min) then
            ! Two-Rarefaction Riemann solver
            pq = (l%p/r%p)**(gm1_2g)
            um = (pq*l%v(axis)/l%cs + r%v(axis)/r%cs + (pq - 1.0)/gm1_2g)/(pq/l%cs + 1.0/r%cs)
            ptl = 1.0 + gm1/2*(l%v(axis) - um)/l%cs
            ptr = 1.0 + gm1/2*(um - r%v(axis))/r%cs
            p_star(1) = 0.5*(l%p*ptl**(1d0/gm1_2g) + r%p*ptr**(1d0/gm1_2g))
         else
            ! Two-Shock Riemann solver
            gel = sqrt(((2d0/(gp1))/l%rho)/(gm1_gp1*l%p + ppv))
            ger = sqrt(((2d0/(gp1))/r%rho)/(gm1_gp1*r%p + ppv))
            p_star(1) = (gel*l%p + ger*r%p - (r%v(axis) - l%v(axis)))/(gel + ger)
         end if
      end if

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
