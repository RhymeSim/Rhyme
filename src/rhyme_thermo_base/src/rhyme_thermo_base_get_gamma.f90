submodule(rhyme_thermo_base) get_gamma_smod
contains
pure module function rhyme_thermo_base_get_gamma() result(g)
   implicit none

   real(kind=8) :: g

   select case (rhyme_thermo_base_state_of_matter)
      ! TODO: case ( thid%on_the_fly )
      ! TODO: case ( thid%unset )
   case (thid%monatomic)
      g = ig_gamma(thid%monatomic)
   case (thid%diatomic)
      g = ig_gamma(thid%diatomic)
   case (thid%polyatomic)
      g = ig_gamma(thid%polyatomic)
   case default
      g = -huge(0d0)
   end select
end function rhyme_thermo_base_get_gamma
end submodule get_gamma_smod
