module rhyme_hydro_base
   use rhyme_physics

   implicit none

   type, private :: hydro_base_indices_t
   end type hydro_base_indices_t

   type(hydro_base_indices_t), parameter :: hyid = hydro_base_indices_t()

   type hydro_base_t
   end type hydro_base_t

   interface
      pure module subroutine rhyme_hydro_base_primitive_to_conserved(w, e_int, u)
         real(kind=8), intent(in) :: w(cid%rho:cid%p), e_int
         real(kind=8), intent(out) :: u(cid%rho:cid%e_tot)
      end subroutine rhyme_hydro_base_primitive_to_conserved

      pure module function rhyme_hydro_base_specific_kinetic_energy(u) result(sp_kin_e)
         real(kind=8), intent(in) :: u(cid%rho:cid%e_tot)
         real(kind=8) :: sp_kin_e
      end function rhyme_hydro_base_specific_kinetic_energy

      pure module function rhyme_hydro_base_specific_internal_energy(u) result(sp_int_e)
         real(kind=8), intent(in) :: u(cid%rho:cid%e_tot)
         real(kind=8) :: sp_int_e
      end function rhyme_hydro_base_specific_internal_energy
   end interface
end module rhyme_hydro_base
