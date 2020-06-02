submodule(rhyme_uv_background) init_smod
contains
module subroutine rhyme_uv_background_init(uvb, units, logger)
   implicit none

   type(uv_background_t), intent(inout) :: uvb
   type(units_t), intent(in) :: units
   type(logger_t), intent(inout) :: logger

   character(len=128) :: uvb_str
   type(nombre_t) :: rho_to_code_unit

   call logger%begin_section('uv_background')

   rho_to_code_unit = 1d0.u. (hydrogen_mass/(centi*meter)**3) .to.units%rho

   uvb%rho_to_code_unit = rho_to_code_unit%v
   call logger%log('rho code unit conversion', '['//trim(.printchain.units%rho)//']', '=', [uvb%rho_to_code_unit])

   write (uvb_str, *) uvb
   call logger%log('', 'uv_background', '=', [uvb_str])

   call logger%end_section
end subroutine rhyme_uv_background_init
end submodule init_smod
