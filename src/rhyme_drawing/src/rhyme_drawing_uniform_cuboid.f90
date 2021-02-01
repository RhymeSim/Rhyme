submodule(rhyme_drawing) rhyme_drawing_uniform_cuboid_submoduel
contains
module subroutine rhyme_drawing_uniform_cuboid(samr, shape, logger)
   implicit none

   type(samr_t), intent(inout) :: samr
   type(shape_t), intent(in) :: shape
   type(logger_t), intent(inout) :: logger

#if NDIM == 1
#define JDX
#define KDX
#define LOOP_J
#define LOOP_K
#define LOOP_J_END
#define LOOP_K_END
#elif NDIM == 2
#define JDX ,j
#define KDX
#define LOOP_J do j = 1, samr%levels(l)%boxes(b)%dims(2)
#define LOOP_K
#define LOOP_J_END end do
#define LOOP_K_END
#elif NDIM == 3
#define JDX ,j
#define KDX ,k
#define LOOP_J do j = 1, samr%levels(l)%boxes(b)%dims(2)
#define LOOP_K do k = 1, samr%levels(l)%boxes(b)%dims(3)
#define LOOP_J_END end do
#define LOOP_K_END end do
#endif

   integer :: l, b, i JDX KDX, d
   real(kind=8) :: dist_vec_px(NDIM), dist_px, r_px, sigma_px, fac
   real(kind=8) :: center(NDIM), half_lengths(NDIM), norm(NDIM)

   call logger%begin_section('cuboid')

   call logger%log('left_corner', '[px]', '=', shape%cuboid%left_corner)
   call logger%log('lengths', '[px]', '=', shape%cuboid%lengths)
   call logger%log('sigma', '[px]', '=', [shape%cuboid%sigma])
   call logger%log('color1', '', '=', shape%fill%colors(:, 1))
   call logger%log('color2', '', '=', shape%fill%colors(:, 2))

   do l = 0, samr%nlevels - 1
   do b = 1, samr%levels(l)%nboxes

      center = shape%cuboid%left_corner + .5d0*shape%cuboid%lengths
      half_lengths = shape%cuboid%lengths/2d0

      LOOP_K
      LOOP_J
      do i = 1, samr%levels(l)%boxes(b)%dims(1)

         dist_vec_px = [i JDX KDX] - .5 - center
         dist_px = sqrt(sum(dist_vec_px**2))

#if NDIM == 2
         norm = dist_vec_px/dist_px

         if (abs(dist_vec_px(1)) < epsilon(0d0)) then
            r_px = half_lengths(1)
            sigma_px = shape%cuboid%sigma
         else if (abs(dist_vec_px(2)) < epsilon(0d0)) then
            r_px = half_lengths(2)
            sigma_px = shape%cuboid%sigma
         else
            fac = min(abs(half_lengths(1)/norm(1)), abs(half_lengths(2)/norm(2)))
            r_px = sqrt(sum((fac*norm)**2))
            sigma_px = shape%cuboid%sigma/max((half_lengths(1)/r_px), (half_lengths(2)/r_px))
         end if

         color = smoothing_factor(dist_px, r_px, sigma_px, shape%fill%colors)

         do d = cid%rho, cid%e_tot
            if (abs(samr%levels(l)%boxes(b)%cells(i JDX KDX, d)) < abs(color(d))) then
               samr%levels(l)%boxes(b)%cells(i JDX KDX, d) = color(d)
            end if
         end do
         do d = cid%temp, NCMP
            samr%levels(l)%boxes(b)%cells(i JDX KDX, d) = color(d)
         end do
#endif
      end do
      LOOP_J_END
      LOOP_K_END
   end do
   end do

   call logger%end_section  ! cuboid
contains
   pure function smoothing_factor(dist, r, sigma, w) result(u)
      implicit none

      real(kind=8), intent(in) :: dist
      real(kind=8), intent(in) :: r, sigma
      real(kind=8), dimension(cid%rho:NCMP, 2), intent(in) :: w

      real(kind=8), dimension(cid%rho:NCMP) :: new_w
      real(kind=8), dimension(cid%rho:NCMP) :: u

      real(kind=8) :: f

      f = (1d0 + tanh((r - dist)/sigma))/2d0

      new_w = f*w(1:NCMP, 1) + (1 - f)*w(1:NCMP, 2)

      call conv_prim_to_cons(new_w(cid%rho:cid%p), u(cid%rho:cid%e_tot))
      u(cid%e_tot + 1:NCMP) = new_w(cid%e_tot + 1:NCMP)
   end function smoothing_factor

end subroutine rhyme_drawing_uniform_cuboid
end submodule rhyme_drawing_uniform_cuboid_submoduel
