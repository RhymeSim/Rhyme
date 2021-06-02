submodule(rhyme_sanity_check) fill_smod
contains
   pure module subroutine rhyme_sanity_check_fill(sc, samr)
      implicit none

      type(sanity_check_t), intent(inout) :: sc
      type(samr_t), intent(in) :: samr

#if NDIM == 1
#define JDX
#define KDX
#define LOOP_J
#define LOOP_K
#define LOOP_J_END
#define LOOP_K_END
#endif
#if NDIM == 2
#define JDX , j
#define KDX
#define LOOP_J do j = 1, samr%levels(0)%boxes(1)%dims(2)
#define LOOP_K
#define LOOP_J_END end do
#define LOOP_K_END
#endif
#if NDIM == 3
#define JDX , j
#define KDX , k
#define LOOP_J do j = 1, samr%levels(0)%boxes(1)%dims(2)
#define LOOP_K do k = 1, samr%levels(0)%boxes(1)%dims(3)
#define LOOP_J_END end do
#define LOOP_K_END end do
#endif

      integer :: i JDX KDX
      real(kind=8) :: v

      sc%rho_info%nbelow = 0
      sc%rho_info%nabove = 0

      sc%vx_info%nbelow = 0
      sc%vx_info%nabove = 0

      sc%vy_info%nbelow = 0
      sc%vy_info%nabove = 0

      sc%vz_info%nbelow = 0
      sc%vz_info%nabove = 0

      sc%rho_info%nbelow = 0
      sc%rho_info%nabove = 0

      sc%e_tot_info%nbelow = 0
      sc%e_tot_info%nabove = 0

      sc%temp_info%nbelow = 0
      sc%temp_info%nabove = 0

      sc%ntr_frac_0_info%nbelow = 0
      sc%ntr_frac_0_info%nabove = 0

      sc%ntr_frac_1_info%nbelow = 0
      sc%ntr_frac_1_info%nabove = 0

      sc%ntr_frac_2_info%nbelow = 0
      sc%ntr_frac_2_info%nabove = 0

      sc%abs_v_info%nbelow = 0
      sc%abs_v_info%nabove = 0

      sc%mach_info%nbelow = 0
      sc%mach_info%nabove = 0

      LOOP_K
      LOOP_J
      do i = 1, samr%levels(0)%boxes(1)%dims(1)
         if (sc%properties(scid%rho)) then
            v = samr%levels(0)%boxes(1)%cells(i JDX KDX, cid%rho)
            call update_info(i JDX KDX, v, sc%rho_range, sc%rho_info)
         end if
         if (sc%properties(scid%vx)) then
            v = samr%levels(0)%boxes(1)%cells(i JDX KDX, cid%rho_u) &
                /samr%levels(0)%boxes(1)%cells(i JDX KDX, cid%rho)
            call update_info(i JDX KDX, v, sc%vx_range, sc%vx_info)
         end if
#if NDIM > 1
         if (sc%properties(scid%vy)) then
            v = samr%levels(0)%boxes(1)%cells(i JDX KDX, cid%rho_v) &
                /samr%levels(0)%boxes(1)%cells(i JDX KDX, cid%rho)
            call update_info(i JDX KDX, v, sc%vy_range, sc%vy_info)
         end if
#endif
#if NDIM > 2
         if (sc%properties(scid%vz)) then
            v = samr%levels(0)%boxes(1)%cells(i JDX KDX, cid%rho_w) &
                /samr%levels(0)%boxes(1)%cells(i JDX KDX, cid%rho)
            call update_info(i JDX KDX, v, sc%vz_range, sc%vz_info)
         end if
#endif
         if (sc%properties(scid%e_tot)) then
            v = samr%levels(0)%boxes(1)%cells(i JDX KDX, cid%e_tot)
            call update_info(i JDX KDX, v, sc%e_tot_range, sc%e_tot_info)
         end if
         if (sc%properties(scid%temp)) then
            v = samr%levels(0)%boxes(1)%cells(i JDX KDX, cid%temp)
            call update_info(i JDX KDX, v, sc%temp_range, sc%temp_info)
         end if
         if (sc%properties(scid%ntr_frac_0)) then
            v = samr%levels(0)%boxes(1)%cells(i JDX KDX, cid%ntr_frac_0)
            call update_info(i JDX KDX, v, sc%ntr_frac_0_range, sc%ntr_frac_0_info)
         end if
#if NSPE > 1
         if (sc%properties(scid%ntr_frac_1)) then
            v = samr%levels(0)%boxes(1)%cells(i JDX KDX, cid%ntr_frac_1)
            call update_info(i JDX KDX, v, sc%ntr_frac_1_range, sc%ntr_frac_1_info)
         end if
#endif
#if NSPE > 2
         if (sc%properties(scid%ntr_frac_2)) then
            v = samr%levels(0)%boxes(1)%cells(i JDX KDX, cid%ntr_frac_2)
            call update_info(i JDX KDX, v, sc%ntr_frac_2_range, sc%ntr_frac_2_info)
         end if
#endif
         if (sc%properties(scid%abs_v)) then
            v = sqrt(sum(samr%levels(0)%boxes(1)%cells(i JDX KDX, cid%rho_u:cid%rho_u + NDIM - 1)**2)) &
                /samr%levels(0)%boxes(1)%cells(i JDX KDX, cid%rho)
            call update_info(i JDX KDX, v, sc%abs_v_range, sc%abs_v_info)
         end if
         if (sc%properties(scid%mach)) then
            v = ( &
                sqrt(sum(samr%levels(0)%boxes(1)%cells(i JDX KDX, cid%rho_u:cid%rho_u + NDIM - 1)**2)) &
                /samr%levels(0)%boxes(1)%cells(i JDX KDX, cid%rho) &
                )/calc_cs(samr%levels(0)%boxes(1)%cells(i JDX KDX, cid%rho_u:cid%e_tot))
            call update_info(i JDX KDX, v, sc%mach_range, sc%mach_info)
         end if
      end do
      LOOP_J_END
      LOOP_K_END

      sc%vtotal_energy(1) = sc%vtotal_energy(2)
      sc%vtotal_energy(2) = rhyme_samr_calc_total_energy(samr)

      ! Be ready to see one of those hacks! Crazy :(
      if (sc%vtotal_energy(1) < tiny(0d0)) then
         sc%vtotal_energy(1) = sc%vtotal_energy(2)
      end if

      sc%vtotal_mass(1) = sc%vtotal_mass(2)
      sc%vtotal_mass(2) = rhyme_samr_calc_total_mass(samr)

      if (sc%vtotal_mass(1) < tiny(0d0)) then
         sc%vtotal_mass(1) = sc%vtotal_mass(2)
      end if

   contains
      pure subroutine update_info(i JDX KDX, val, rng, info)
         integer, intent(in) :: i JDX KDX
         real(kind=8), intent(in) :: val, rng(2)
         type(sanity_check_info_t), intent(inout) :: info

         if (val < rng(1)) then
            info%nbelow = info%nbelow + 1
            if (val < info%vbelow(2)) then
               info%vbelow(1) = info%vbelow(2)
               info%cbelow(1, :) = info%cbelow(2, :)
               info%vbelow(2) = val
               info%cbelow(2, :) = [i JDX KDX]
            end if
         end if

         if (val > rng(2)) then
            info%nabove = info%nabove + 1
            if (val > info%vabove(2)) then
               info%vabove(1) = info%vabove(2)
               info%cabove(1, :) = info%cabove(2, :)
               info%vabove(2) = v
               info%cabove(2, :) = [i JDX KDX]
            end if
         end if
      end subroutine update_info
   end subroutine rhyme_sanity_check_fill
end submodule fill_smod
