submodule(rhyme_units) init_smod
contains
   module subroutine rhyme_units_init(units, logger)
      implicit none

      type(units_t), intent(inout) :: units
      type(logger_t), intent(inout) :: logger

      type(nombre_unit_t), pointer :: kb_unit, r_unit, amu_unit

      call logger%begin_section('units')

      call logger%log('', '# of components', '=', [NCMP])
      call logger%log('', 'component labels', '=', cid%labels)

      units%rho => .parse.units%rho_str
      call logger%log('', 'rho:', '[ '//trim(.printchain.units%rho)//' ]')

      units%length => .parse.units%length_str
      call logger%log('', 'length:', '[ '//trim(.printchain.units%length)//' ]')

      units%time => .parse.units%time_str
      call logger%log('', 'time:', '[ '//trim(.printchain.units%time)//' ]')

      units%velocity => units%length/units%time
      call logger%log('', 'velocity', '[ '//trim(.printchain.units%velocity)//' ]')

      units%pressure => units%rho*units%length**2/units%time**2
      call logger%log('', 'pressure:', '[ '//trim(.printchain.units%pressure)//' ]')

      units%temperature => 1*kelvin
      call logger%log('', 'temperature:', '[ '//trim(.printchain.units%temperature)//' ]')

      units%energy => units%rho*units%velocity**2
      ! call logger%log('', 'energy:', '[ '//trim(.printchain.units%energy)//' ]')

      kb_unit => .parse.kb_unit_str

      units%kb = kb_value.unit.kb_unit &
                 .to.units%rho*units%length**5/(units%time**2*kelvin)
      call logger%log('kB = ', units%kb%v, '[ '//trim(.printchain.units%kb%u)//' ]')

      r_unit => .parse.r_unit_str

      units%r = r_value.unit.r_unit &
                .to.units%rho*units%length**5/(units%time**2*mole*units%temperature)
      call logger%log('R = ', units%r%v, '[ '//trim(.printchain.units%r%u)//' ]')

      amu_unit => .parse.amu_unit_str

      units%amu = amu_value.u.amu_unit.to.units%rho*units%length**3
      call logger%log('1 amu = ', units%amu%v, '[ '//trim(.printchain.units%amu%u)//' ]')

      call logger%end_section
   end subroutine rhyme_units_init
end submodule init_smod
