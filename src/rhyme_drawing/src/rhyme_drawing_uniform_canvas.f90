submodule(rhyme_drawing) rhyme_drawing_uniform_canvas_submodule
contains
module subroutine rhyme_drawing_uniform_canvas(samr, bg_prim)
   implicit none

   type(samr_t), intent(inout) :: samr
   real(kind=8), intent(in) :: bg_prim(NCMP)

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

   integer :: l, b, i JDX KDX
   real(kind=8) :: bg(cid%rho:cid%e_tot)

   call conv_prim_to_cons(bg_prim, bg)

   do l = 0, samr%nlevels - 1
      do b = 1, samr%levels(l)%nboxes

         LOOP_K
         LOOP_J
         do i = 1, samr%levels(l)%boxes(b)%dims(1)
            samr%levels(l)%boxes(b)%cells(i JDX KDX, cid%rho:cid%e_tot) = bg(cid%rho:cid%e_tot)
            samr%levels(l)%boxes(b)%cells(i JDX KDX, cid%e_tot + 1:NCMP) = bg_prim(cid%p + 1:NCMP)
         end do
         LOOP_J_END
         LOOP_K_END

      end do
   end do

end subroutine rhyme_drawing_uniform_canvas
end submodule rhyme_drawing_uniform_canvas_submodule
