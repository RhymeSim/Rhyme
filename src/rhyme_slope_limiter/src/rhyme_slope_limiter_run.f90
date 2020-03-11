submodule(rhyme_slope_limiter) rhyme_sl_run_smod
contains
pure module subroutine rhyme_slope_limiter_run(sl, ul, u, ur, delta)
   implicit none

   class(slope_limiter_t), intent(in) :: sl
   real(kind=8), dimension(cid%rho:cid%e_tot), intent(in) :: ul, u, ur
   real(kind=8), dimension(cid%rho:cid%e_tot), intent(out) :: delta

   select case (sl%type)
   case (slid%van_Leer)
      call rhyme_slope_limiter_van_leer(sl, ul, u, ur, delta)
   case (slid%minmod)
      call rhyme_slope_limiter_minmod(sl, ul, u, ur, delta)
   case (slid%van_albada)
      call rhyme_slope_limiter_van_albada(sl, ul, u, ur, delta)
   case (slid%superbee)
      call rhyme_slope_limiter_superbee(sl, ul, u, ur, delta)
   end select
end subroutine rhyme_slope_limiter_run
end submodule rhyme_sl_run_smod
