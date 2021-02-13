submodule(rhyme_sanity_check) print_smod
contains
   module subroutine rhyme_sanity_check_print(sc, logger)
      implicit none

      type(sanity_check_t), intent(inout) :: sc
      type(logger_t), intent(inout) :: logger

      if (sc%properties(scid%rho)) then
         call logger%begin_section('rho')
         call log_property(sc%rho_info)
         call logger%end_section
      end if

      if (sc%properties(scid%vx)) then
         call logger%begin_section('vx')
         call log_property(sc%vx_info)
         call logger%end_section
      end if

      if (sc%properties(scid%vy)) then
         call logger%begin_section('vy')
         call log_property(sc%vy_info)
         call logger%end_section
      end if

      if (sc%properties(scid%vz)) then
         call logger%begin_section('vz')
         call log_property(sc%vz_info)
         call logger%end_section
      end if

      if (sc%properties(scid%e_tot)) then
         call logger%begin_section('e_tot')
         call log_property(sc%e_tot_info)
         call logger%end_section
      end if

      if (sc%properties(scid%temp)) then
         call logger%begin_section('temp')
         call log_property(sc%temp_info)
         call logger%end_section
      end if

      if (sc%properties(scid%ntr_frac_0)) then
         call logger%begin_section('ntr_frac_0')
         call log_property(sc%ntr_frac_0_info)
         call logger%end_section
      end if

      if (sc%properties(scid%ntr_frac_1)) then
         call logger%begin_section('ntr_frac_1')
         call log_property(sc%ntr_frac_1_info)
         call logger%end_section
      end if

      if (sc%properties(scid%ntr_frac_2)) then
         call logger%begin_section('ntr_frac_2')
         call log_property(sc%ntr_frac_2_info)
         call logger%end_section
      end if

      if (sc%properties(scid%abs_v)) then
         call logger%begin_section('|v|')
         call log_property(sc%abs_v_info)
         call logger%end_section
      end if

      if (sc%properties(scid%mach)) then
         call logger%begin_section('Mach_number')
         call log_property(sc%mach_info)
         call logger%end_section
      end if

      if (sc%properties(scid%total_energy)) then
         call log_ratio('E_tot', sc%total_energy_range, sc%vtotal_energy)
      end if

      if (sc%properties(scid%total_mass)) then
         call log_ratio('M_tot', sc%total_mass_range, sc%vtotal_mass)
      end if

   contains
      subroutine log_property(info)
         type(sanity_check_info_t), intent(in) :: info

         character(len=128) :: str1, str2

         if (info%nbelow(2) > 0) then
            write (str1, *) trim(.toString.info%vbelow(2)), ' @ ', trim(.toString.info%cbelow(2, :))
            write (str2, *) trim(.toString.info%vbelow(1)), ' @ ', trim(.toString.info%cbelow(1, :))
            call logger%log('# below', '', '=', [info%nbelow(2)])
            call logger%log('Min:', str1, '=PrevCheck=>', [str2])
         end if

         if (info%nabove(2) > 0) then
            write (str1, *) trim(.toString.info%vabove(2)), ' @ ', trim(.toString.info%cabove(2, :))
            write (str2, *) trim(.toString.info%vabove(1)), ' @ ', trim(.toString.info%cabove(1, :))
            call logger%log('# above', '', '=', [info%nabove(2)])
            call logger%log('Min:', str1, '=PrevCheck=>', [str2])
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
