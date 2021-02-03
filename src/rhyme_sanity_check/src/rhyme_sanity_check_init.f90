submodule(rhyme_sanity_check) init_smod
contains
   module subroutine rhyme_sanity_check_init(sc, units, thermo, samr, logger)
      implicit none

      type(sanity_check_t), intent(inout) :: sc
      type(units_t), intent(in) :: units
      type(thermo_base_t), intent(in) :: thermo
      type(samr_t), intent(in) :: samr
      type(logger_t), intent(inout) :: logger

      call logger%begin_section('sanity_check')

      call logger%log('state_of_matter', '', '=', [thid%som_names(thermo%state_of_matter)])

      sc%rho_unit => .parse.sc%rho_unit_str
      sc%vx_unit => .parse.sc%vx_unit_str
      sc%vy_unit => .parse.sc%vy_unit_str
      sc%vz_unit => .parse.sc%vz_unit_str
      sc%e_tot_unit => .parse.sc%e_tot_unit_str
      sc%temp_unit => .parse.sc%temp_unit_str
      sc%abs_v_unit => .parse.sc%abs_v_unit_str

      call rhyme_sanity_check_fill(sc, samr)

      if (sc%properties(scid%rho)) then
         sc%rho_range(1) = rhyme_nombre_get_value(((sc%rho_range(1) .u.sc%rho_unit) .to.units%rho))
         sc%rho_range(2) = rhyme_nombre_get_value(((sc%rho_range(2) .u.sc%rho_unit) .to.units%rho))
         call logger%log('rho (valid range)', '[ '//trim(.printchain.units%rho)//' ]', '=', sc%rho_range)

         call copy_last_to_prev(sc%rho_info)
      end if

      if (sc%properties(scid%vx)) then
         sc%vx_range(1) = rhyme_nombre_get_value(((sc%vx_range(1) .u.sc%vx_unit) .to.units%velocity))
         sc%vx_range(2) = rhyme_nombre_get_value(((sc%vx_range(2) .u.sc%vx_unit) .to.units%velocity))
         call logger%log('vx (valid range)', '[ '//trim(.printchain.units%velocity)//' ]', '=', sc%vx_range)

         call copy_last_to_prev(sc%vx_info)
      end if

      if (sc%properties(scid%vy)) then
         sc%vx_range(1) = rhyme_nombre_get_value(((sc%vx_range(1) .u.sc%vx_unit) .to.units%velocity))
         sc%vx_range(2) = rhyme_nombre_get_value(((sc%vx_range(2) .u.sc%vx_unit) .to.units%velocity))
         call logger%log('vy (valid range)', '[ '//trim(.printchain.units%velocity)//' ]', '=', sc%vy_range)

         call copy_last_to_prev(sc%vy_info)
      end if

      if (sc%properties(scid%vz)) then
         sc%vz_range(1) = rhyme_nombre_get_value(((sc%vz_range(1) .u.sc%vz_unit) .to.units%velocity))
         sc%vz_range(2) = rhyme_nombre_get_value(((sc%vz_range(2) .u.sc%vz_unit) .to.units%velocity))
         call logger%log('vz (valid range)', '[ '//trim(.printchain.units%velocity)//' ]', '=', sc%vz_range)

         call copy_last_to_prev(sc%vz_info)
      end if

      if (sc%properties(scid%e_tot)) then
         sc%e_tot_range(1) = rhyme_nombre_get_value(((sc%e_tot_range(1) .u.sc%e_tot_unit) .to.units%energy))
         sc%e_tot_range(2) = rhyme_nombre_get_value(((sc%e_tot_range(2) .u.sc%e_tot_unit) .to.units%energy))
         call logger%log('e_tot (valid range)', '[ '//trim(.printchain.units%energy)//' ]', '=', sc%e_tot_range)

         call copy_last_to_prev(sc%e_tot_info)
      end if

      if (sc%properties(scid%temp)) then
         sc%temp_range(1) = rhyme_nombre_get_value(((sc%temp_range(1) .u.sc%temp_unit) .to.units%temperature))
         sc%temp_range(2) = rhyme_nombre_get_value(((sc%temp_range(2) .u.sc%temp_unit) .to.units%temperature))
         call logger%log('temp (valid range)', '[ '//trim(.printchain.units%temperature)//' ]', '=', sc%temp_range)

         call copy_last_to_prev(sc%temp_info)
      end if

      if (sc%properties(scid%ntr_frac_0)) then
         call logger%log('ntr_frac_0 (valid range)', '[]', '=', sc%ntr_frac_0_range)
         call copy_last_to_prev(sc%ntr_frac_0_info)
      end if

      if (sc%properties(scid%ntr_frac_1)) then
         call logger%log('ntr_frac_1 (valid range)', '[]', '=', sc%ntr_frac_1_range)
         call copy_last_to_prev(sc%ntr_frac_1_info)
      end if

      if (sc%properties(scid%ntr_frac_2)) then
         call logger%log('ntr_frac_2 (valid range)', '[]', '=', sc%ntr_frac_2_range)
         call copy_last_to_prev(sc%ntr_frac_2_info)
      end if

      if (sc%properties(scid%abs_v)) then
         sc%abs_v_range(1) = rhyme_nombre_get_value(((sc%abs_v_range(1) .u.sc%abs_v_unit) .to.units%velocity))
         sc%abs_v_range(2) = rhyme_nombre_get_value(((sc%abs_v_range(2) .u.sc%abs_v_unit) .to.units%velocity))
         call logger%log('|v| (valid range)', '[ '//trim(.printchain.units%velocity)//' ]', '=', sc%abs_v_range)

         call copy_last_to_prev(sc%abs_v_info)
      end if

      if (sc%properties(scid%mach)) then
         call logger%log('Mach number (valid range)', '[]', '=', sc%mach_range)

         call copy_last_to_prev(sc%mach_info)
      end if

      if (sc%properties(scid%total_mass)) then
         call logger%log('M_tot (valid ratio to IC)', '[]', '=', sc%total_mass_range)

         sc%vtotal_mass(0) = sc%vtotal_mass(2)
         sc%vtotal_mass(1) = sc%vtotal_mass(2)
      end if

      if (sc%properties(scid%total_energy)) then
         call logger%log('E_tot (valid ratio to IC)', '[]', '=', sc%total_energy_range)

         sc%vtotal_energy(0) = sc%vtotal_energy(2)
         sc%vtotal_energy(1) = sc%vtotal_energy(2)
      end if

      call rhyme_sanity_check_print(sc, logger)

      call logger%end_section

   contains
      pure subroutine copy_last_to_prev(info)
         type(sanity_check_info_t), intent(inout) :: info

         info%nbelow(1) = info%nbelow(2)
         info%vbelow(1) = info%vbelow(2)
         info%cbelow(1, :) = info%cbelow(2, :)
         info%nabove(1) = info%nabove(2)
         info%vabove(1) = info%vabove(2)
         info%cabove(1, :) = info%cabove(2, :)
      end subroutine copy_last_to_prev
   end subroutine rhyme_sanity_check_init
end submodule init_smod
