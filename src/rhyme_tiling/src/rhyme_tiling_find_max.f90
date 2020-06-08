submodule(rhyme_tiling) find_max_smod
contains
module function rhyme_tiling_find_max(tiling, func) result(maximum)
   implicit none

   type(tiling_t), target, intent(in) :: tiling
   interface
      pure function func(variables, dx, dt) result(res)
         real(kind=8), intent(in) :: variables(:)
         real(kind=8), intent(in) :: dx(NDIM), dt
         real(kind=8) :: res
      end function
   end interface
   real(kind=8) :: maximum

   integer :: i, j, k, idx, ti, tj, tk, tile_number, level
   real(kind=8) :: this_maximum, maximum_tiles
   type(tiling_t), pointer :: tiles(:, :, :)

   maximum = -huge(0d0)
   maximum_tiles = -huge(0d0)

   !$OMP PARALLEL DO &
   !$OMP& SHARED(tiling) &
   !$OMP& PRIVATE(this_maximum) &
   !$OMP& FIRSTPRIVATE(maximum) &
   !$OMP& REDUCTION(max:maximum)
   do k = 1, tiling%domain(3)
   do j = 1, tiling%domain(2)
   do i = 1, tiling%domain(1)
      this_maximum = func(tiling%cells(i, j, k, :), tiling%dx(:, 0), tiling%dt(0))

      if (this_maximum > maximum) then
         maximum = this_maximum
      end if
   end do
   end do
   end do
   !$OMP END PARALLEL DO

   !$OMP PARALLEL DO &
   !$OMP& SHARED(tiling) &
   !$OMP& PRIVATE(this_maximum, ti, tj, tk, tile_number, level) &
   !$OMP& FIRSTPRIVATE(maximum_tiles) &
   !$OMP& REDUCTION(max:maximum_tiles)
   do idx = 1, product(tiling%grid)
      ti = mod(idx, tiling%grid(1)) + 1
      tj = mod(idx/tiling%grid(1), tiling%grid(3)) + 1
      tk = idx/product(tiling%grid(1:2)) + 1

      do tile_number = 1, 2**(NDIM*tiling%max_levels)
         if (.not. allocated(tiling%tiles(tile_number, ti, tj, tk)%cells)) then
            cycle
         end if

         level = int(log(real(tile_number))/log(2e0)/NDIM) + 1

         do k = 1, tiling%tile_domain(3)
         do j = 1, tiling%tile_domain(2)
         do i = 1, tiling%tile_domain(1)
            this_maximum = func(tiling%tiles(tile_number, ti, tj, tk)%cells(i, j, k, :), tiling%dx(:, level), tiling%dt(level))

            if (this_maximum > maximum_tiles) then
               maximum_tiles = this_maximum
            end if
         end do
         end do
         end do
      end do
   end do
   !$OMP END PARALLEL DO

   maximum = max(maximum, maximum_tiles)
end function rhyme_tiling_find_max
end submodule find_max_smod
