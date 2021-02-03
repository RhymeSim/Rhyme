submodule(rhyme_sanity_check) print_smod
contains
   module subroutine rhyme_sanity_check_print(sc, logger)
      implicit none

      type(sanity_check_t), intent(inout) :: sc
      type(logger_t), intent(inout) :: logger

      if (sc%properties(scid%rho)) then
         call log_property('rho', sc%rho_info)
      end if

      if (sc%properties(scid%vx)) then
         call log_property('vx', sc%vx_info)
      end if

      if (sc%properties(scid%vy)) then
         call log_property('vy', sc%vy_info)
      end if

      if (sc%properties(scid%vz)) then
         call log_property('vz', sc%vz_info)
      end if

      if (sc%properties(scid%e_tot)) then
         call log_property('e_tot', sc%e_tot_info)
      end if

      if (sc%properties(scid%temp)) then
         call log_property('temp', sc%temp_info)
      end if

      if (sc%properties(scid%ntr_frac_0)) then
         call log_property('ntr_frac_0', sc%ntr_frac_0_info)
      end if

      if (sc%properties(scid%ntr_frac_1)) then
         call log_property('ntr_frac_1', sc%ntr_frac_1_info)
      end if

      if (sc%properties(scid%ntr_frac_2)) then
         call log_property('ntr_frac_2', sc%ntr_frac_2_info)
      end if

      if (sc%properties(scid%abs_v)) then
         call log_property('|v|', sc%abs_v_info)
      end if

      if (sc%properties(scid%mach)) then
         call log_property('Mach number', sc%mach_info)
      end if

      if (sc%properties(scid%total_energy)) then
         call log_ratio('E_tot', sc%total_energy_range, sc%vtotal_energy)
      end if

      if (sc%properties(scid%total_mass)) then
         call log_ratio('M_tot', sc%total_mass_range, sc%vtotal_mass)
      end if

   contains
      subroutine log_property(tag, info)
         character(len=*), intent(in) :: tag
         type(sanity_check_info_t), intent(in) :: info

         if (info%nbelow(2) > 0) then
            call logger%warn(tag, '(# below)', '=', [info%nbelow(2)])
            call logger%warn('minimum '//trim(tag), info%vbelow(2), '@', info%cbelow(2, :))
            call logger%warn('minimum '//trim(tag)//' (prev. check)', info%vbelow(1), '@', info%cbelow(1, :))
         end if
         !
         if (info%nabove(2) > 0) then
            call logger%warn(tag, '(# above)', '=', [info%nabove(2)])
            call logger%warn('maximum '//trim(tag), info%vabove(2), '@', info%cabove(2, :))
            call logger%warn('maximum '//trim(tag)//' (prev. check)', info%vabove(1), '@', info%cabove(1, :))
         end if
      end subroutine log_property

      subroutine log_ratio(tag, rng, val)
         character(len=*), intent(in) :: tag
         real(kind=8), intent(in) :: rng(2), val(0:2)

         real(kind=8) :: ratio

         ratio = val(2)/val(0)

         if (ratio < rng(1) .or. ratio > rng(2)) then
            call logger%warn(trim(tag), val(2), ' / '//trim(tag)//'_0 =', [ratio])
         else
            call logger%log(trim(tag), val(2), ' / '//trim(tag)//'_0 =', [ratio])
         end if
      end subroutine log_ratio
   end subroutine rhyme_sanity_check_print
end submodule print_smod
