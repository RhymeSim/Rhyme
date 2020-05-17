submodule(rhyme_uv_background) h_self_shielding_n_smod
contains
module function rhyme_uv_background_h_self_shielding_n(uvb, z, logger, temp, sigma_HI, gas_fraction) result(n)
   implicit none

   type(uv_background_t), intent(in) :: uvb
   real(kind=8), intent(in) :: z
   type(logger_t), intent(inout) :: logger
   real(kind=8), intent(in), optional :: temp, sigma_HI, gas_fraction
   real(kind=8) :: n

   real(kind=8) :: T, sHI, fg

   select case (uvb%model)
   case (uvbid%HM12)
      if (present(temp)) then
         T = temp
      else
         T = 1d4
      end if

      if (present(sigma_HI)) then
         sHI = sigma_HI
      else
         sHI = 2.48d-18
      end if

      if (present(gas_fraction)) then
         fg = gas_fraction
      else
         fg = .17d0
      end if

      n = rhyme_uv_background_haardt_madau_12_h_self_shielding_n(z, T, sHI, fg)*uvb%rho_to_code_unit
   case default
      call logger%err('Unknown UVB model!', 'model', '=', [uvb%model])
      n = huge(0d0)
   end select
end function rhyme_uv_background_h_self_shielding_n
end submodule h_self_shielding_n_smod