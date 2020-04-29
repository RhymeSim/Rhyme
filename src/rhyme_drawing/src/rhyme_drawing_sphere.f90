submodule(rhyme_drawing) rhyme_drawing_sphere_submodule
contains
module subroutine rhyme_drawing_sphere(samr, ic, shape, logger)
   implicit none

   type(samr_t), intent(inout) :: samr
   type(initial_condition_t), intent(in) :: ic
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

   real(kind=8), dimension(cid%rho:NCMP) :: color
   integer :: l, b, i JDX KDX, d
   real(kind=8) :: origin_px(NDIM), r_px, sigma_px, box_lengths(NDIM)

   call logger%begin_section('sphere')

   do d = 1, NDIM
      box_lengths(d) = rhyme_nombre_get_value(ic%box_lengths(d) .to.shape%sphere%unit)
   end do
   origin_px = shape%sphere%origin/box_lengths*ic%base_grid
   r_px = shape%sphere%r/box_lengths(1)*ic%base_grid(1)
   sigma_px = shape%sphere%sigma/box_lengths(1)*ic%base_grid(1)

   call logger%log('', 'origin_px', '=', origin_px)
   call logger%log('', 'r_px', '=', [r_px])
   call logger%log('', 'sigma_px', '=', [sigma_px])

   select case (shape%fill%modes(1))
   case (drid%add)

   case (drid%absolute)
      call logger%begin_section('absolute')
      do l = 0, samr%nlevels - 1
         do b = 1, samr%levels(l)%nboxes
            LOOP_K
            LOOP_J
            do i = 1, samr%levels(l)%boxes(b)%dims(1)
               color = smoothing_factor([i JDX KDX] - .5d0, origin_px, r_px, sigma_px, shape%fill%colors)

               do d = cid%rho, cid%e_tot
                  if (samr%levels(l)%boxes(b)%cells(i JDX KDX, d) < color(d)) then
                     samr%levels(l)%boxes(b)%cells(i JDX KDX, d) = color(d)
                  end if
               end do
               do d = cid%temp, NCMP
                  samr%levels(l)%boxes(b)%cells(i JDX KDX, d) = color(d)
               end do
            end do
            LOOP_J_END
            LOOP_K_END
         end do
      end do
      call logger%end_section
   case default
      call logger%err('Unknonw mode!')
   end select

   call logger%end_section
end subroutine rhyme_drawing_sphere

function smoothing_factor(x, o, r, sigma, w) result(u)
   implicit none

   real(kind=8), dimension(NDIM), intent(in) :: x, o
   real(kind=8), intent(in) :: r, sigma
   real(kind=8), dimension(cid%rho:NCMP, 2), intent(in) :: w

   real(kind=8), dimension(cid%rho:NCMP) :: new_w
   real(kind=8), dimension(cid%rho:NCMP) :: u

   real(kind=8) :: dist, f

   dist = sqrt(sum((x - o)**2))
   f = (1d0 + tanh((r - dist)/sigma))/2d0

   new_w = f*w(1:NCMP, 1) + (1 - f)*w(1:NCMP, 2)

   call conv_prim_to_cons(new_w(cid%rho:cid%p), u(cid%rho:cid%e_tot))
   u(cid%e_tot + 1:NCMP) = new_w(cid%e_tot + 1:NCMP)
end function smoothing_factor
end submodule rhyme_drawing_sphere_submodule
