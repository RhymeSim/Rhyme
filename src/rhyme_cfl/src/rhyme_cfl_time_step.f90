submodule(rhyme_cfl) rhyme_cfl_time_step_smod
contains
pure module function rhyme_cfl_time_step(c, samr) result(dt)
   use rhyme_ideal_gas

   implicit none

   real(kind=8), intent(in) :: c ! c: courant number
   type(samr_t), intent(in) :: samr
   real(kind=8) :: dt

#if NDIM == 1
#define LOOP_J
#define LOOP_K
#define LOOP_J_END
#define LOOP_K_END
#define JDX
#define KDX
#elif NDIM == 2
#define LOOP_J do j = 1, samr%levels(0)%boxes(1)%dims(2)
#define LOOP_K
#define LOOP_J_END end do
#define LOOP_K_END
#define JDX ,j
#define KDX
#elif NDIM == 3
#define LOOP_J do j = 1, samr%levels(0)%boxes(1)%dims(2)
#define LOOP_K do k = 1, samr%levels(0)%boxes(1)%dims(3)
#define LOOP_J_END end do
#define LOOP_K_END end do
#define JDX ,j
#define KDX ,k
#endif

   integer :: i JDX KDX
   real(kind=8) :: max_u, u

   max_u = 0.d0

   LOOP_K
   LOOP_J
   do i = 1, samr%levels(0)%boxes(1)%dims(1)
      u = calc_u(samr%levels(0)%boxes(1)%cells(i JDX KDX, cid%rho:cid%e_tot))
      if (u > max_u) max_u = u
   end do
   LOOP_J_END
   LOOP_K_END

   dt = c*minval(samr%levels(0)%dx)/max_u

contains
   pure real(kind=8) function calc_u(u) result(v)
      implicit none

      real(kind=8), intent(in) :: u(cid%rho:cid%e_tot)

      v = sqrt(sum(u(cid%rho_u:cid%rho_u + NDIM - 1)**2)/u(cid%rho)**2) &
          + calc_cs(u)
   end function calc_u
end function rhyme_cfl_time_step
end submodule rhyme_cfl_time_step_smod
