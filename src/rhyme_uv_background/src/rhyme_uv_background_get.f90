submodule(rhyme_uv_background) get_smod
contains
module function rhyme_uv_background_get(uvb, z, species, logger) result(rates)
   implicit none

   type(uv_background_t), intent(in) :: uvb
   real(kind=8), intent(in) :: z
   character(len=*), dimension(:), intent(in) :: species
   type(logger_t), intent(inout) :: logger

   real(kind=4), dimension(2*size(species)) :: rates

   select case (uvb%model)
   case (uvbid%HM12)
      rates = rhyme_uv_background_haardt_madau_12_get(z, species)
   case default
      call logger%err('Unknown UVB model!', 'model', '=', [uvb%model])
      rates = 0e0
   end select
end function rhyme_uv_background_get
end submodule get_smod
