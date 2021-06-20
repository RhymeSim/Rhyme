submodule(rhyme_irs) rhyme_irs_guess_p_star_submodule
contains
   pure module function rhyme_irs_guess_p_star(l_rho, l_v, l_p, l_cs, r_rho, r_v, r_p, r_cs, axis) result(p_star)
      implicit none

      real(kind=8), intent(in) :: l_rho, l_v(NDIM), l_p, l_cs, r_rho, r_v(NDIM), r_p, r_cs
      integer, intent(in) :: axis
      real(kind=8) :: p_star(6)

      real(kind=8) :: l_g, r_g

      real(kind=8) :: q_user, cup, ppv, p_min, p_max, q_max, pq, um, ptl, ptr, gel, ger

      q_user = 2d0

      cup = 0.25*(l_rho + r_rho)*(l_cs + r_cs)
      ppv = max(0.5*(l_p + r_p) + 0.5*(l_v(axis) - r_v(axis))*cup, 0d0)
      p_min = min(l_p, r_p)
      p_max = max(l_p, r_p)
      q_max = p_max/p_min

      if (q_max <= q_user .and. (p_min < ppv .and. ppv < p_max)) then
         ! PVRS Riemann solver
         p_star(1) = ppv
      else
         if (ppv < p_min) then
            ! Two-Rarefaction Riemann solver
            pq = (l_p/r_p)**(gm1_2g)
            um = (pq*l_v(axis)/l_cs + r_v(axis)/r_cs + 2.0/gm1*(pq - 1.0))/(pq/l_cs + 1.0/r_cs)
            ptl = 1.0 + gm1/2*(l_v(axis) - um)/l_cs
            ptr = 1.0 + gm1/2*(um - r_v(axis))/r_cs
            p_star(1) = 0.5*(l_p*ptl**(1d0/gm1_2g) + r_p*ptr**(1d0/gm1_2g))
         else
            ! Two-Shock Riemann solver
            gel = sqrt(((2d0/(gp1))/l_rho)/(gm1_gp1*l_p + ppv))
            ger = sqrt(((2d0/(gp1))/r_rho)/(gm1_gp1*r_p + ppv))
            p_star(1) = (gel*l_p + ger*r_p - (r_v(axis) - l_v(axis)))/(gel + ger)
         end if
      end if

      ! Ramses guess
      p_star(2) = ( &
                  r_rho*r_cs*l_p &
                  + l_rho*l_cs*r_p &
                  + r_cs*l_cs*(l_rho*l_v(axis) - r_rho*r_v(axis)) &
                  )/(r_rho*r_cs + l_rho*l_cs)

      ! Two rarefaction approximation
      p_star(3) = ( &
                  ( &
                  l_cs + r_cs - .5d0*gm1*(r_v(axis) - l_v(axis)) &
                  )/( &
                  (l_cs/l_p)**gm1_2g + (r_cs/r_p)**gm1_2g &
                  ) &
                  )**real(1.d0/gm1_2g, kind=8)

      ! Two shock approximation
      l_g = rhyme_irs_guess_p_star_g_K(l_rho, l_p)
      r_g = rhyme_irs_guess_p_star_g_K(r_rho, r_p)
      p_star(4) = (l_g*l_p + r_g*r_p - (r_v(axis) - l_v(axis)))/(l_g + r_g)

      ! p_PV
      p_star(5) = .5d0*(l_p + r_p) &
                  - .125d0*(r_v(axis) - l_v(axis))*(l_rho + r_rho)*(l_cs + r_cs)

      ! Arithmetic mean
      p_star(6) = .5d0*(l_p + r_p)

      ! NB: Here we are enforcing a pressure floor for the guessed p_stars
      ! where( p_star < 0d0 ) p_star = p_vacuum might be a better and more conservative approach
      p_star = max(p_star, tiny(0d0))
   end function rhyme_irs_guess_p_star

   real(kind=8) pure function rhyme_irs_guess_p_star_g_K(rho, p) result(g_K)
      implicit none

      real(kind=8), intent(in) :: rho, p

      g_K = sqrt((2.d0/(gp1*rho))/(p + gm1_gp1*p))
   end function rhyme_irs_guess_p_star_g_K
end submodule rhyme_irs_guess_p_star_submodule
