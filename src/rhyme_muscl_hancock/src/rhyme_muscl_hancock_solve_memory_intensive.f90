submodule(rhyme_muscl_hancock) rhyme_mh_solve_memory_intensive_submodule
contains
   module subroutine rhyme_muscl_hancock_solve_memory_intensive( &
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
#define LOOP_J do j = lb(2), ub(2)
#define LOOP_J_END end do
#define LOOP_K
#define LOOP_K_END
#endif

#if NDIM == 3
#define JDX ,j
#define KDX ,k
#define LOOP_J do j = lb(2), ub(2)
#define LOOP_J_END end do
#define LOOP_K do k = lb(3), ub(3)
#define LOOP_K_END end do
#endif

      integer :: l, b, i JDX KDX
      integer :: lb(NDIM), ub(NDIM)
      real(kind=8) :: delta(cid%rho:cid%e_tot), evolved_state(cid%rho:cid%e_tot)

      l = box%level
      b = box%number

      call rhyme_mh_workspace_check(ws, box)

      lb = 0
      ub = box%dims + 1

      !$OMP PARALLEL DO &
      !$OMP& SHARED(box, ws) &
      !$OMP& FIRSTPRIVATE(dx, dt, irs, sl, l, b, lb, ub) &
      !$OMP& PRIVATE(delta)
      LOOP_K
      LOOP_J
      do i = lb(1), ub(1)
         call rhyme_slope_limiter_run( &
            sl, &
            box%cells(i - 1 JDX KDX, cid%rho:cid%e_tot), &
            box%cells(i JDX KDX, cid%rho:cid%e_tot), &
            box%cells(i + 1 JDX KDX, cid%rho:cid%e_tot), delta)
         call rhyme_muscl_hancock_half_step_extrapolation( &
            box%cells(i JDX KDX, cid%rho:cid%e_tot), &
            delta, samrid%x, dx(samrid%x), dt, &
            ws%levels(l)%boxes(b)%ul(i JDX KDX, cid%rho:cid%e_tot, samrid%x), &
            ws%levels(l)%boxes(b)%ur(i JDX KDX, cid%rho:cid%e_tot, samrid%x))

#if NDIM > 1
         call rhyme_slope_limiter_run( &
            sl, &
            box%cells(i JDX - 1 KDX, cid%rho:cid%e_tot), &
            box%cells(i JDX KDX, cid%rho:cid%e_tot), &
            box%cells(i JDX + 1 KDX, cid%rho:cid%e_tot), delta)
         call rhyme_muscl_hancock_half_step_extrapolation( &
            box%cells(i JDX KDX, cid%rho:cid%e_tot), &
            delta, samrid%y, dx(samrid%y), dt, &
            ws%levels(l)%boxes(b)%ul(i JDX KDX, cid%rho:cid%e_tot, samrid%y), &
            ws%levels(l)%boxes(b)%ur(i JDX KDX, cid%rho:cid%e_tot, samrid%y))
#endif

#if NDIM > 2
         call rhyme_slope_limiter_run( &
            sl, &
            box%cells(i, j KDX - 1, cid%rho:cid%e_tot), &
            box%cells(i, j KDX, cid%rho:cid%e_tot), &
            box%cells(i, j KDX + 1, cid%rho:cid%e_tot), delta)
         call rhyme_muscl_hancock_half_step_extrapolation( &
            box%cells(i, j KDX, cid%rho:cid%e_tot), &
            delta, samrid%z, dx(samrid%z), dt, &
            ws%levels(l)%boxes(b)%ul(i, j KDX, cid%rho:cid%e_tot, samrid%z), &
            ws%levels(l)%boxes(b)%ur(i, j KDX, cid%rho:cid%e_tot, samrid%z))
#endif
      end do
      LOOP_J_END
      LOOP_K_END
      !$OMP END PARALLEL DO

      lb = 0
      ub = box%dims

      !$OMP PARALLEL DO &
      !$OMP& SHARED(box, ws) &
      !$OMP& FIRSTPRIVATE(dx, dt, irs, sl, l, b, lb, ub) &
      !$OMP& PRIVATE(evolved_state)
      LOOP_K
      LOOP_J
      do i = lb(1), ub(1)
         call rhyme_irs_solve( &
            irs, &
            ws%levels(l)%boxes(b)%ur(i JDX KDX, cid%rho:cid%e_tot, samrid%x), &
            ws%levels(l)%boxes(b)%ul(i + 1 JDX KDX, cid%rho:cid%e_tot, samrid%x), &
            0.d0, dt, samrid%x, evolved_state)
         ws%levels(l)%boxes(b)%fr(i JDX KDX, cid%rho:cid%e_tot, samrid%x) = &
            calc_flux(evolved_state, samrid%x)

#if NDIM > 1
         call rhyme_irs_solve( &
            irs, &
            ws%levels(l)%boxes(b)%ur(i JDX KDX, cid%rho:cid%e_tot, samrid%y), &
            ws%levels(l)%boxes(b)%ul(i JDX + 1 KDX, cid%rho:cid%e_tot, samrid%y), &
            0.d0, dt, samrid%y, evolved_state)
         ws%levels(l)%boxes(b)%fr(i JDX KDX, cid%rho:cid%e_tot, samrid%y) = &
            calc_flux(evolved_state, samrid%y)
#endif

#if NDIM > 2
         call rhyme_irs_solve( &
            irs, &
            ws%levels(l)%boxes(b)%ur(i, j KDX, cid%rho:cid%e_tot, samrid%z), &
            ws%levels(l)%boxes(b)%ul(i, j KDX + 1, cid%rho:cid%e_tot, samrid%z), &
            0.d0, dt, samrid%z, evolved_state)
         ws%levels(l)%boxes(b)%fr(i, j KDX, cid%rho:cid%e_tot, samrid%z) = &
            calc_flux(evolved_state, samrid%z)
#endif
      end do
      LOOP_J_END
      LOOP_K_END
      !$OMP END PARALLEL DO

      lb = 1
      ub = box%dims

      !$OMP PARALLEL DO &
      !$OMP& SHARED(box, ws) &
      !$OMP& FIRSTPRIVATE(dx, dt, irs, sl, l, b, lb, ub)
      LOOP_K
      LOOP_J
      do i = 1, box%dims(1)
         box%cells(i JDX KDX, cid%rho:cid%e_tot) = &
            box%cells(i JDX KDX, cid%rho:cid%e_tot) + &
            ( &
            (dt/dx(samrid%x) &
             *( &
             ws%levels(l)%boxes(b)%fr(i - 1 JDX KDX, cid%rho:cid%e_tot, samrid%x) &
             - ws%levels(l)%boxes(b)%fr(i JDX KDX, cid%rho:cid%e_tot, samrid%x) &
             ) &
             ) &
#if NDIM > 1
            + (dt/dx(samrid%y) &
               *( &
               ws%levels(l)%boxes(b)%fr(i, j - 1 KDX, cid%rho:cid%e_tot, samrid%y) &
               - ws%levels(l)%boxes(b)%fr(i, j KDX, cid%rho:cid%e_tot, samrid%y) &
               ) &
               ) &
#endif
#if NDIM > 2
            + (dt/dx(samrid%z) &
               *( &
               ws%levels(l)%boxes(b)%fr(i, j, k - 1, cid%rho:cid%e_tot, samrid%z) &
               - ws%levels(l)%boxes(b)%fr(i, j, k, cid%rho:cid%e_tot, samrid%z) &
               ) &
               ) &
#endif
            )
      end do
      LOOP_J_END
      LOOP_K_END
      !$OMP END PARALLEL DO
   end subroutine rhyme_muscl_hancock_solve_memory_intensive
end submodule rhyme_mh_solve_memory_intensive_submodule
