submodule(rhyme_sanity_check) init_smod
contains
   module subroutine rhyme_sanity_check_init(sc, units, logger)
      implicit none

      type(sanity_check_t), intent(inout) :: sc
      type(units_t), intent(in) :: units
      type(logger_t), intent(inout) :: logger

      call logger%begin_section('sanity_check')

      sc%rho_unit => .parse.sc%rho_unit_str
      sc%vx_unit => .parse.sc%vx_unit_str
      sc%vy_unit => .parse.sc%vy_unit_str
      sc%vz_unit => .parse.sc%vz_unit_str
      sc%e_tot_unit => .parse.sc%e_tot_unit_str
      sc%temp_unit => .parse.sc%temp_unit_str

      if (sc%properties(scid%rho)) then
         sc%rho_range(1) = rhyme_nombre_get_value(((sc%rho_range(1) .u.sc%rho_unit) .to.units%rho))
         sc%rho_range(2) = rhyme_nombre_get_value(((sc%rho_range(2) .u.sc%rho_unit) .to.units%rho))
         call logger%log('rho', '[ '//trim(.printchain.units%rho)//' ]', '=', sc%rho_range)
      end if

      if (sc%properties(scid%vx)) then
         sc%vx_range(1) = rhyme_nombre_get_value(((sc%vx_range(1) .u.sc%vx_unit) .to.units%velocity))
         sc%vx_range(2) = rhyme_nombre_get_value(((sc%vx_range(2) .u.sc%vx_unit) .to.units%velocity))
         call logger%log('vx', '[ '//trim(.printchain.units%velocity)//' ]', '=', sc%vx_range)
      end if

      if (sc%properties(scid%vy)) then
         sc%vx_range(1) = rhyme_nombre_get_value(((sc%vx_range(1) .u.sc%vx_unit) .to.units%velocity))
         sc%vx_range(2) = rhyme_nombre_get_value(((sc%vx_range(2) .u.sc%vx_unit) .to.units%velocity))
         call logger%log('vy', '[ '//trim(.printchain.units%velocity)//' ]', '=', sc%vy_range)
      end if

      if (sc%properties(scid%vz)) then
         sc%vz_range(1) = rhyme_nombre_get_value(((sc%vz_range(1) .u.sc%vz_unit) .to.units%velocity))
         sc%vz_range(2) = rhyme_nombre_get_value(((sc%vz_range(2) .u.sc%vz_unit) .to.units%velocity))
         call logger%log('vz', '[ '//trim(.printchain.units%velocity)//' ]', '=', sc%vz_range)
      end if

      if (sc%properties(scid%e_tot)) then
         sc%e_tot_range(1) = rhyme_nombre_get_value(((sc%e_tot_range(1) .u.sc%e_tot_unit) .to.units%energy))
         sc%e_tot_range(2) = rhyme_nombre_get_value(((sc%e_tot_range(2) .u.sc%e_tot_unit) .to.units%energy))
         call logger%log('e_tot', '[ '//trim(.printchain.units%energy)//' ]', '=', sc%e_tot_range)
      end if

      if (sc%properties(scid%temp)) then
         sc%temp_range(1) = rhyme_nombre_get_value(((sc%temp_range(1) .u.sc%temp_unit) .to.units%temperature))
         sc%temp_range(2) = rhyme_nombre_get_value(((sc%temp_range(2) .u.sc%temp_unit) .to.units%temperature))
         call logger%log('temp', '[ '//trim(.printchain.units%temperature)//' ]', '=', sc%temp_range)
      end if

      if (sc%properties(scid%ntr_frac_0)) then
         call logger%log('ntr_frac_0', '[]', '=', sc%ntr_frac_0_range)
      end if

      if (sc%properties(scid%ntr_frac_1)) then
         call logger%log('ntr_frac_1', '[]', '=', sc%ntr_frac_1_range)
      end if

      if (sc%properties(scid%ntr_frac_2)) then
         call logger%log('ntr_frac_2', '[]', '=', sc%ntr_frac_2_range)
      end if

      call logger%end_section
   end subroutine rhyme_sanity_check_init
end submodule init_smod
