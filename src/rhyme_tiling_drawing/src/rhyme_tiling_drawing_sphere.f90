submodule(rhyme_tiling_drawing) sphere_smod
contains
   module subroutine rhyme_tiling_drawing_sphere(tiling, shape)
      implicit none

      type(tiling_t), intent(inout) :: tiling
      type(tiling_shape_t), intent(in) :: shape

#if NDIM == 1
#define JDX
#define KDX
#define LOOP_J
#define LOOP_K
#define LOOP_J_END
#define LOOP_K_END
#elif NDIM == 2
#define JDX , j
#define KDX
#define LOOP_J do j = 1, tiling%domain(2)
#define LOOP_K
#define LOOP_J_END end do
#define LOOP_K_END
#elif NDIM == 3
#define JDX , j
#define KDX , k
#define LOOP_J do j = 1, tiling%domain(2)
#define LOOP_K do k = 1, tiling%domain(3)
#define LOOP_J_END end do
#define LOOP_K_END end do
#endif

      integer :: grid_id, grid_coor(NDIM), tile_id
      integer :: i JDX KDX

      do grid_id = 1, product(tiling%domain)
         grid_coor = rhyme_tiling_to_coordinate(grid_id, tiling%domain)

         LOOP_K
         LOOP_J
         do i = 1, tiling%domain(1)
         end do
         LOOP_J_END
         LOOP_K_END
      end do

   end subroutine rhyme_tiling_drawing_sphere
end submodule sphere_smod
