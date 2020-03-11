submodule(rhyme_slope_limiter) rhyme_sl_van_leer_smod
contains
pure module subroutine rhyme_slope_limiter_van_leer(sl, ul, u, ur, delta)
   implicit none

   class(slope_limiter_t), intent(in) :: sl
   real(kind=8), dimension(cid%rho:cid%e_tot), intent(in) :: ul, u, ur
   real(kind=8), dimension(cid%rho:cid%e_tot), intent(out) :: delta

   real(kind=8), dimension(cid%rho:cid%e_tot) :: d_L, d_R, d, r
   integer :: i

   d_L = u - ul
   d_R = ur - u

   d = .5d0*(1.d0 + sl%w)*d_L + .5d0*(1.d0 + sl%w)*d_R

   call rhyme_slope_limiter_r(ul, u, ur, r)

   do i = cid%rho, cid%e_tot
      if (r(i) > 0.d0) then
         delta(i) = min( &
                    2.d0*r(i)/(1.d0 + r(i)), rhyme_slope_limiter_xi_R(sl, r(i)) &
                    )*d(i)
      else
         delta(i) = 0.d0
      end if
   end do
end subroutine rhyme_slope_limiter_van_leer
end submodule rhyme_sl_van_leer_smod
