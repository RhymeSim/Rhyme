submodule(rhyme_tiling) total_ntiles_smod
contains
pure module function rhyme_tiling_total_ntiles(level, base) result(ntiles)
   implicit none

   integer, intent(in) :: level, base
   integer :: ntiles

   integer :: i

   ntiles = 0

   do i = 0, level
      ntiles = ntiles + base**(i*NDIM)
   end do
end function rhyme_tiling_total_ntiles
end submodule total_ntiles_smod
