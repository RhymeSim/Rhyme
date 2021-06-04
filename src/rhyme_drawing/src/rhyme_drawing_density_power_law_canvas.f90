! TODO: convert units to physical units instead of pixels
submodule(rhyme_drawing) density_power_law_canvas_smod
contains
   module subroutine rhyme_drawing_density_power_law_canvas(samr, c, rho0, r0, r1, power, bg_prim)
      implicit none

      type(samr_t), intent(inout) :: samr
      real(kind=8), intent(in) :: c(NDIM), rho0, r0, r1, power, bg_prim(NCMP)

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
      real(kind=8) :: bg(cid%rho:cid%e_tot), rho

      call conv_prim_to_cons(bg_prim, bg)

      do l = 0, samr%nlevels - 1
         do b = 1, samr%levels(l)%nboxes

            LOOP_K
            LOOP_J
            do i = 1, samr%levels(l)%boxes(b)%dims(1)
               rho = density_power_law([i JDX KDX], rho0, c, r0, r1, power)
               samr%levels(l)%boxes(b)%cells(i JDX KDX, cid%rho) = rho
               samr%levels(l)%boxes(b)%cells(i JDX KDX, cid%rho_u:cid%e_tot) = bg(cid%rho_u:cid%e_tot)
               samr%levels(l)%boxes(b)%cells(i JDX KDX, cid%e_tot + 1:NCMP) = bg_prim(cid%p + 1:NCMP)
            end do
            LOOP_J_END
            LOOP_K_END
         end do
      end do

   contains
      pure function density_power_law(x, rho0, c, r0, r1, p) result(new_rho)
         implicit none

         integer, intent(in) :: x(NDIM)
         real(kind=8), intent(in) :: rho0, c(NDIM), r0, r1, p
         real(kind=8) :: new_rho

         real(kind=8) :: dist

         dist = sqrt(sum((x - c)**2))

         if (dist <= r0) then
            new_rho = rho0
         else
            new_rho = rho0*(dist/r1)**p
         end if
      end function density_power_law
   end subroutine rhyme_drawing_density_power_law_canvas
end submodule density_power_law_canvas_smod
