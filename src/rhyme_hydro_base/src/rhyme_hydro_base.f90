module rhyme_hydro_base
  implicit none

  type hydro_indices_t
    integer :: rho = 1, u = 2, v = 3, w = 4, p = 5 ! primitive variables
    integer :: rho_u = 2, rho_v = 3, rho_w = 4, e_tot = 5 ! conserved variables
    integer :: x = 1, y = 2, z = 3 ! axes
  contains
    procedure :: vel => rhyme_hydro_base_get_directional_velocity_index
    procedure :: rho_vel => rhyme_hydro_base_get_directional_velocity_index
  end type hydro_indices_t

  type ( hydro_indices_t ), parameter :: hyid = hydro_indices_t()


  type hydro_primitive_t
    real ( kind=8 ) :: w(5) = 0.d0
  end type hydro_primitive_t


  type hydro_conserved_t
    real ( kind=8 ) :: u(5) = 0.d0
  end type hydro_conserved_t


  type hydro_flux_t
    real ( kind=8 ) :: f(5) = 0.d0
  end type hydro_flux_t


  interface hy_copy
    procedure rhyme_hydro_base_copy
  end interface hy_copy


  interface hy_prim_to_cons
    procedure rhyme_hydro_base_prim_to_cons
  end interface hy_prim_to_cons


  interface hy_sp_kinetic_e
    procedure rhyme_hydro_base_specific_kinetic_energy_conserved
  end interface hy_sp_kinetic_e


  interface hy_sp_kinetic_e_prim
    procedure rhyme_hydro_base_specific_kinetic_energy_primitive
  end interface hy_sp_kinetic_e_prim


  interface hy_sp_internal_e
    procedure rhyme_hydro_base_specific_internal_energy_conserved
  end interface hy_sp_internal_e

contains

  pure subroutine rhyme_hydro_base_copy (source, target)
    implicit none

    type ( hydro_conserved_t ), intent ( in ) :: source
    type ( hydro_conserved_t ), intent ( out ) :: target

    target%u = source%u
  end subroutine rhyme_hydro_base_copy


  pure subroutine rhyme_hydro_base_prim_to_cons (primitive, e_int, conserved)
    implicit none

    type ( hydro_primitive_t ), intent ( in ) :: primitive
    real ( kind=8 ), intent ( in ) :: e_int
    type ( hydro_conserved_t ), intent ( out ) :: conserved

    conserved%u(hyid%rho) = primitive%w(hyid%rho)
    conserved%u(hyid%rho_u) = primitive%w(hyid%rho) * primitive%w(hyid%u)
    conserved%u(hyid%rho_v) = primitive%w(hyid%rho) * primitive%w(hyid%v)
    conserved%u(hyid%rho_w) = primitive%w(hyid%rho) * primitive%w(hyid%w)
    conserved%u(hyid%e_tot) = primitive%w(hyid%rho) &
    * 0.5d0 * sum(primitive%w(hyid%u:hyid%w)**2) + e_int
  end subroutine rhyme_hydro_base_prim_to_cons


  pure function rhyme_hydro_base_specific_kinetic_energy_conserved (U) result (e_kin_sp)
    implicit none

    type ( hydro_conserved_t ), intent ( in ) :: U
    real ( kind=8 ) :: e_kin_sp

    e_kin_sp = 0.5d0 * sum( U%u(hyid%rho_u:hyid%rho_w)**2 ) / U%u(hyid%rho)**2
  end function rhyme_hydro_base_specific_kinetic_energy_conserved


  pure function rhyme_hydro_base_specific_kinetic_energy_primitive (W) result (e_kin_sp)
    implicit none

    type ( hydro_primitive_t ), intent ( in ) :: W
    real ( kind=8 ) :: e_kin_sp

    e_kin_sp = 0.5d0 * sum(W%w(hyid%u:hyid%w)**2)
  end function rhyme_hydro_base_specific_kinetic_energy_primitive


  pure function rhyme_hydro_base_specific_internal_energy_conserved (U) result (e_int_sp)
    implicit none

    type ( hydro_conserved_t ), intent ( in ) :: U
    real ( kind=8 ) :: e_int_sp

    e_int_sp = ( U%u(hyid%e_tot) / U%u(hyid%rho) ) &
      - rhyme_hydro_base_specific_kinetic_energy_conserved(U)
  end function rhyme_hydro_base_specific_internal_energy_conserved


  pure function rhyme_hydro_base_get_directional_velocity_index (this, dir) result (v_idx)
    implicit none

    class ( hydro_indices_t ), intent ( in ) :: this
    integer, intent ( in ) :: dir
    integer :: v_idx

    v_idx = this%rho_u + dir - 1
  end function rhyme_hydro_base_get_directional_velocity_index
end module rhyme_hydro_base
