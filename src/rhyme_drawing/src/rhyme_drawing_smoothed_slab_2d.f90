submodule(rhyme_drawing) rhyme_drawing_smoothed_slab_2d_submodule
contains
module subroutine rhyme_drawing_smoothed_slab_2d(samr, shape, logger)
   ! TODO: Add test

   implicit none

   type(samr_t), intent(inout) :: samr
   type(shape_t), intent(in) :: shape
   type(logger_t), intent(inout) :: logger

#if NDIM > 1

#if NDIM == 2
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

   integer :: l, b, i JDX KDX
   real(kind=8) :: x

   do l = 0, samr%nlevels - 1
      do b = 1, samr%levels(l)%nboxes
         LOOP_K
         LOOP_J
         do i = 1, samr%levels(l)%boxes(b)%dims(1)
            select case (shape%slab_2d%axis)
            case (drid%x)
               x = real(i - .5d0 + samr%levels(l)%boxes(b)%left_edge(1) - 1)/2**l
#if NDIM > 1
            case (drid%y)
               x = real(j - .5d0 + samr%levels(l)%boxes(b)%left_edge(2) - 1)/2**l
#endif
#if NDIM > 2
            case (drid%z)
               x = real(k - .5d0 + samr%levels(l)%boxes(b)%left_edge(3) - 1)/2**l
#endif
            case DEFAULT
               call logger%err('Unknown slab direction', 'axis', '=', [shape%slab_2d%axis])
               return
            end select

            samr%levels(l)%boxes(b)%cells(i JDX KDX, cid%rho:cid%e_tot) = ramp_func(x, shape)
         end do
         LOOP_J_END
         LOOP_K_END
      end do
   end do

contains

   pure function ramp_func(x, shape) result(u)
      implicit none

      real(kind=8), intent(in) :: x
      type(shape_t), intent(in) :: shape
      real(kind=8) :: u(cid%rho:cid%e_tot)

      real(kind=8) :: factor, Rs, slab_center
      real(kind=8) :: w(cid%rho:cid%p)

      slab_center = (shape%slab_2d%pos(1) + shape%slab_2d%pos(2))/2.d0
      Rs = abs(slab_center - shape%slab_2d%pos(1))

      factor = (1 + tanh((Rs - (x - slab_center))/shape%slab_2d%sigma(1))) &
               *(1 + tanh((Rs + (x - slab_center))/shape%slab_2d%sigma(2)))

      w = shape%fill%colors(cid%rho:cid%e_tot, 1) + 0.25*( &
          shape%fill%colors(cid%rho:cid%e_tot, 2) &
          - shape%fill%colors(cid%rho:cid%e_tot, 1) &
          )*factor

      call conv_prim_to_cons(w, u)
   end function ramp_func
#endif
end subroutine rhyme_drawing_smoothed_slab_2d
end submodule rhyme_drawing_smoothed_slab_2d_submodule
