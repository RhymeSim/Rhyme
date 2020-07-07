submodule(rhyme_tiling) level_tile_ids_smod
contains
pure module function rhyme_tiling_level_tile_ids(base, level) result(ids)
   implicit none

   integer, intent(in) :: base, level
   integer :: ids(base**(NDIM*level))

   integer :: i, l

   if (level == 0) then
      ids(1) = 1
   else if (level > 0) then
      do i = 1, base**(NDIM*level)
         ids(i) = rhyme_tiling_total_ntiles(level - 1, base) + i
      end do
   else
      ids = -1
   end if
end function rhyme_tiling_level_tile_ids
end submodule level_tile_ids_smod
