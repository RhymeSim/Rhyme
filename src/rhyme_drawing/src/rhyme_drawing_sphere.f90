submodule(rhyme_drawing) rhyme_drawing_sphere_submodule
contains
   module subroutine rhyme_drawing_sphere(samr, ic, shape, logger, ie, units, chemistry)
      implicit none

      type(samr_t), intent(inout) :: samr
      type(initial_condition_t), intent(in) :: ic
      type(shape_t), intent(in) :: shape
      type(logger_t), intent(inout) :: logger
      type(ionisation_equilibrium_t), intent(in), optional :: ie
      type(units_t), intent(in), optional :: units
      type(chemistry_t), intent(in), optional :: chemistry

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
      real(kind=8) :: temp_over_mu, temp, temp_prev, density, density_prev, p, v(NDIM)
      real(kind=8) :: ntr_frac(NSPE), one_over_mu, kb_over_amu
      real(kind=8) :: temp_accuracy, density_accuracy
      integer :: niterations

      call logger%begin_section('sphere')

      do d = 1, NDIM
         box_lengths(d) = rhyme_nombre_get_value(ic%box_lengths(d) .to.shape%sphere%unit)
      end do

      origin_px = shape%sphere%origin/box_lengths*ic%base_grid
      r_px = shape%sphere%r/box_lengths(1)*ic%base_grid(1)
      sigma_px = shape%sphere%sigma/box_lengths(1)*ic%base_grid(1)

      call logger%log('origin', '[px]', '=', origin_px)
      call logger%log('r', '[px]', '=', [r_px])
      call logger%log('sigma', '[px]', '=', [sigma_px])
      call logger%log('color1', '', '=', shape%fill%colors(:, 1))
      call conv_prim_to_cons(shape%fill%colors(:, 1), color)
      call logger%log('color1', '[conserved]', '=', color(cid%rho:cid%e_tot))
      call logger%log('color2', '', '=', shape%fill%colors(:, 2))
      call conv_prim_to_cons(shape%fill%colors(:, 2), color)
      call logger%log('color2', '[conserved]', '=', color(cid%rho:cid%e_tot))

      select case (shape%fill%modes(1))
      case (drid%add)
         call logger%err('Add mode is not implemented yet!')

      case (drid%absolute)
         call logger%log('Absolute mode')
         do l = 0, samr%nlevels - 1
         do b = 1, samr%levels(l)%nboxes
            LOOP_K
            LOOP_J
            do i = 1, samr%levels(l)%boxes(b)%dims(1)
               color = smoothing_factor([i JDX KDX] - .5d0, origin_px, r_px, sigma_px, shape%fill%colors)

               do d = cid%rho, cid%e_tot
                  ! if (abs(samr%levels(l)%boxes(b)%cells(i JDX KDX, d)) < abs(color(d))) then
                  samr%levels(l)%boxes(b)%cells(i JDX KDX, d) = color(d)
                  ! end if
               end do
               do d = cid%temp, NCMP
                  samr%levels(l)%boxes(b)%cells(i JDX KDX, d) = color(d)
               end do
            end do
            LOOP_J_END
            LOOP_K_END
         end do
         end do
      case default
         call logger%err('Unknonw mode!')
      end select

      if ( &
         present(ie) &
         .and. present(units) &
         .and. present(chemistry) &
         .and. abs( &
         shape%fill%colors(cid%p, 1) - shape%fill%colors(cid%p, 2) &
         ) < epsilon(0d0)*abs(shape%fill%colors(cid%p, 1)) &
         ) then
         ! Pressure equilibrium
         call logger%begin_section('pressure_equilibrium')

         kb_over_amu = units%kb%v/units%amu%v

         temp_accuracy = 1d-4
         density_accuracy = 1d-4
         call logger%log('temp accuracy', '[%]', '=', [100*temp_accuracy])
         call logger%log('density accuracy', '[%]', '=', [100*density_accuracy])

         p = shape%fill%colors(cid%p, 1)

         do l = 0, samr%nlevels - 1
         do b = 1, samr%levels(l)%nboxes
            LOOP_K
            LOOP_J
            do i = 1, samr%levels(l)%boxes(b)%dims(1)
               ntr_frac = samr%levels(l)%boxes(b)%cells(i JDX KDX, cid%ntr_frac_0:cid%ntr_frac_0 + NSPE - 1)
               temp_over_mu = calc_t_mu(samr%levels(l)%boxes(b)%cells(i JDX KDX, cid%rho:cid%e_tot))
               one_over_mu = rhyme_chemistry_one_over_mu(chemistry, ntr_frac)

               temp = temp_over_mu/one_over_mu
               density = samr%levels(l)%boxes(b)%cells(i JDX KDX, cid%rho)

               temp_prev = 1d0
               density_prev = 1d0

               v = samr%levels(l)%boxes(b)%cells(i JDX KDX, cid%rho_u:cid%rho_u + NDIM - 1) &
                   /samr%levels(l)%boxes(b)%cells(i JDX KDX, cid%rho)
               niterations = 0

               do while ( &
                  ( &
                  abs((temp_prev - temp)/temp_prev) > temp_accuracy &
                  .or. abs((density_prev - density)/density_prev) > density_accuracy &
                  ) &
                  .and. niterations < ie%max_niterations &
                  )
                  temp_prev = temp
                  density_prev = density

                  ntr_frac = rhyme_ionisation_equilibrium_pick(ie, temp, density)

                  temp_over_mu = calc_t_mu(samr%levels(l)%boxes(b)%cells(i JDX KDX, cid%rho:cid%e_tot))
                  one_over_mu = rhyme_chemistry_one_over_mu(chemistry, ntr_frac)
                  temp = temp_over_mu/one_over_mu

                  density = p/one_over_mu/kb_over_amu/temp

                  samr%levels(l)%boxes(b)%cells(i JDX KDX, cid%rho) = density
                  samr%levels(l)%boxes(b)%cells(i JDX KDX, cid%rho_u:cid%rho_u + NDIM - 1) = density*v
                  samr%levels(l)%boxes(b)%cells(i JDX KDX, cid%e_tot) = .5d0*density*sum(v**2) + p/(get_gamma() - 1)

                  niterations = niterations + 1
               end do

               samr%levels(l)%boxes(b)%cells(i JDX KDX, cid%temp) = p/(density*one_over_mu*kb_over_amu)
               samr%levels(l)%boxes(b)%cells(i JDX KDX, cid%ntr_frac_0:cid%ntr_frac_0 + NSPE - 1) = ntr_frac
            end do
            LOOP_J_END
            LOOP_K_END
         end do
         end do

         call logger%end_section(print_duration=.true.)  ! pressure_equilibrium
      end if

      call logger%end_section
   end subroutine rhyme_drawing_sphere

   pure function smoothing_factor(x, o, r, sigma, w) result(u)
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
