submodule(rhyme_ionisation_equilibrium) pick_smod
contains
pure module function rhyme_ionisation_equilibrium_pick(ie, temp, density) result(ntr_frac)
   implicit none

   type(ionisation_equilibrium_t), intent(in) :: ie
   real(kind=8), intent(in) :: temp, density
   real(kind=8) :: ntr_frac(NSPE)

   integer :: ti, di

   ti = int((log10(temp) - ie%log_temp_min)/ie%dlog_temp)
   ti = min(ie%table_sizes(1), max(1, ti))

   di = int((log10(density) - ie%log_density_min)/ie%dlog_density)
   di = min(ie%table_sizes(2), max(1, di))

   ntr_frac = ie%table(:, ti, di)
end function rhyme_ionisation_equilibrium_pick
end submodule pick_smod
