submodule(rhyme_slope_limiter) rhyme_sl_xi_l_smod
contains
   pure module function rhyme_slope_limiter_xi_l(sl, r) result(xi_l)
      implicit none

      class(slope_limiter_t), intent(in) :: sl
      real(kind=8), intent(in) :: r

      real(kind=8) :: xi_l

      xi_l = 2.d0*r/(1.d0 - sl%w + (1.d0 + sl%w)*r)
   end function rhyme_slope_limiter_xi_l
end submodule rhyme_sl_xi_l_smod
