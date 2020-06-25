submodule(rhyme_tiling) to_coordinate_smod
contains
pure module function rhyme_tiling_to_coordinate(i, grid) result(coor)
   implicit none

   integer, intent(in) :: i, grid(NDIM)
   integer :: coor(NDIM)

#if NDIM == 1
   coor(1) = mod(i - 1, grid(1)) + 1
#elif NDIM == 2
   coor(1) = mod(i - 1, grid(1)) + 1
   coor(2) = (i - 1)/grid(1) + 1
#elif NDIM == 3
   coor(1) = mod(i - 1, grid(1)) + 1
   coor(2) = mod((i - 1)/grid(1), grid(2)) + 1
   coor(3) = (i - 1)/(grid(1)*grid(2)) + 1
#endif

end function rhyme_tiling_to_coordinate
end submodule to_coordinate_smod
