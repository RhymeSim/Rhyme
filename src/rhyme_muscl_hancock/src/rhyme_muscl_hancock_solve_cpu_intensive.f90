submodule(rhyme_muscl_hancock) rhyme_mh_solve_cpu_intensive_submodule
contains
module subroutine rhyme_muscl_hancock_solve_cpu_intensive( &
   box, dx, dt, irs, sl, ws)
   implicit none

   type(samr_box_t), intent(inout) :: box
   real(kind=8), intent(in) :: dx(NDIM), dt
   type(irs_t), intent(inout) :: irs
   type(slope_limiter_t), intent(in) :: sl
   type(mh_workspace_t), intent(inout) :: ws

#if NDIM == 1
#define JDX
#define KDX
#define LOOP_J
#define LOOP_J_END
#define LOOP_K
#define LOOP_K_END
#endif

#if NDIM == 2
#define JDX ,j
#define KDX
#define LOOP_J do j = 1, box%dims(2)
#define LOOP_J_END end do
#define LOOP_K
#define LOOP_K_END
#endif

#if NDIM == 3
#define JDX ,j
#define KDX ,k
#define LOOP_J do j = 1, box%dims(2)
#define LOOP_J_END end do
#define LOOP_K do k = 1, box%dims(3)
#define LOOP_K_END end do
#endif

   real(kind=8) :: delta(-1:1, cid%rho:cid%e_tot, NDIM)
   real(kind=8) :: half_step_left(-1:1, cid%rho:cid%e_tot, NDIM)
   real(kind=8) :: half_step_right(-1:1, cid%rho:cid%e_tot, NDIM)
   real(kind=8) :: edge_state(-1:0, cid%rho:cid%e_tot, NDIM)
   real(kind=8) :: flux(-1:0, cid%rho:cid%e_tot, NDIM)
   real(kind=8) :: df(cid%rho:cid%e_tot, NDIM)

   integer :: idx, l, b, i JDX KDX, axis
   integer :: ub(NDIM)

   l = box%level
   b = box%number

   call rhyme_mh_workspace_check(ws, box)

   !$OMP PARALLEL DO &
   !$OMP& SHARED(box, dx, dt, irs, sl, ws, l, b) &
   !$OMP& PRIVATE(idx, axis, ub, delta, half_step_left, half_step_right, edge_state, flux, df)
   LOOP_K
   LOOP_J
   do i = 1, box%dims(1)
      ws%levels(l)%boxes(b)%u(i JDX KDX, cid%rho:cid%e_tot) = &
         box%cells(i JDX KDX, cid%rho:cid%e_tot)

      do idx = -1, 1
         call rhyme_slope_limiter_run( &
            sl, &
            box%cells(i + idx - 1 JDX KDX, cid%rho:cid%e_tot), &
            box%cells(i + idx JDX KDX, cid%rho:cid%e_tot), &
            box%cells(i + idx + 1 JDX KDX, cid%rho:cid%e_tot), &
            delta(idx, cid%rho:cid%e_tot, samrid%x))

         call rhyme_muscl_hancock_half_step_extrapolation( &
            box%cells(i + idx JDX KDX, cid%rho:cid%e_tot), &
            delta(idx, cid%rho:cid%e_tot, samrid%x), &
            samrid%x, dx(samrid%x), dt, &
            half_step_left(idx, cid%rho:cid%e_tot, samrid%x), &
            half_step_right(idx, cid%rho:cid%e_tot, samrid%x))
      end do

#if NDIM > 1
      do idx = -1, 1
         call rhyme_slope_limiter_run( &
            sl, &
            box%cells(i JDX + idx - 1 KDX, cid%rho:cid%e_tot), &
            box%cells(i JDX + idx KDX, cid%rho:cid%e_tot), &
            box%cells(i JDX + idx + 1 KDX, cid%rho:cid%e_tot), &
            delta(idx, cid%rho:cid%e_tot, samrid%y))

         call rhyme_muscl_hancock_half_step_extrapolation( &
            box%cells(i JDX + idx KDX, cid%rho:cid%e_tot), &
            delta(idx, cid%rho:cid%e_tot, samrid%y), &
            samrid%y, dx(samrid%y), dt, &
            half_step_left(idx, cid%rho:cid%e_tot, samrid%y), &
            half_step_right(idx, cid%rho:cid%e_tot, samrid%y))
      end do
#endif

#if NDIM > 2
      do idx = -1, 1
         call rhyme_slope_limiter_run( &
            sl, &
            box%cells(i JDX KDX + idx - 1, cid%rho:cid%e_tot), &
            box%cells(i JDX KDX + idx, cid%rho:cid%e_tot), &
            box%cells(i JDX KDX + idx + 1, cid%rho:cid%e_tot), &
            delta(idx, cid%rho:cid%e_tot, samrid%z))

         call rhyme_muscl_hancock_half_step_extrapolation( &
            box%cells(i JDX KDX + idx, cid%rho:cid%e_tot), &
            delta(idx, cid%rho:cid%e_tot, samrid%z), &
            samrid%z, dx(samrid%z), dt, &
            half_step_left(idx, cid%rho:cid%e_tot, samrid%z), &
            half_step_right(idx, cid%rho:cid%e_tot, samrid%z))
      end do
#endif

      do axis = samrid%x, samrid%x + NDIM - 1
         do idx = -1, 0
            call rhyme_irs_solve( &
               irs, &
               half_step_right(idx, cid%rho:cid%e_tot, axis), &
               half_step_left(idx + 1, cid%rho:cid%e_tot, axis), &
               0.d0, dt, axis, &
               edge_state(idx, cid%rho:cid%e_tot, axis))

            flux(idx, cid%rho:cid%e_tot, axis) = &
               calc_flux(edge_state(idx, cid%rho:cid%e_tot, axis), axis)
         end do

         df(cid%rho:cid%e_tot, axis) = flux(-1, cid%rho:cid%e_tot, axis) &
                                       - flux(0, cid%rho:cid%e_tot, axis)

         ws%levels(l)%boxes(b)%u(i JDX KDX, cid%rho:cid%e_tot) = &
            ws%levels(l)%boxes(b)%u(i JDX KDX, cid%rho:cid%e_tot) &
            + dt/dx(axis)*df(cid%rho:cid%e_tot, axis)
      end do

   end do
   LOOP_J_END
   LOOP_K_END
   !$OMP END PARALLEL DO

#if NDIM == 1
#define RANGE_J
#define RANGE_K
#elif NDIM == 2
#define RANGE_J , 1:ub(2)
#define RANGE_K
#elif NDIM == 3
#define RANGE_J , 1:ub(2)
#define RANGE_K , 1:ub(3)
#endif

   ub = box%dims

   box%cells(1:ub(1) RANGE_J RANGE_K, cid%rho:cid%e_tot) = &
      ws%levels(l)%boxes(b)%u(1:ub(1) RANGE_J RANGE_K, cid%rho:cid%e_tot)

end subroutine rhyme_muscl_hancock_solve_cpu_intensive
end submodule rhyme_mh_solve_cpu_intensive_submodule
