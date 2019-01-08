module rhyme_hydro_base
  implicit none


  type hydro_primitive_t
    ! rho, x-velocity, y-velocity, z-velocity, p
    real(kind=8) :: w(5)
  end type hydro_primitive_t


  type hydro_conserved_t
    ! rho, x-momentum, y-momentum, z-momentum, e_tot per unit volume
    real(kind=8) :: u(5)
  end type hydro_conserved_t


  type hydro_flux_t
    real(kind=8) :: f(5)
  end type hydro_flux_t


  type hydro_indices_t
    integer :: rho, u, v, w, p ! primitive variables
    integer :: rho_u, rho_v, rho_w, e_tot ! conserved variables
    integer :: x, y, z ! directions
  contains
    procedure :: vel => hy_get_directional_velocity_index
    procedure :: rho_vel => hy_get_directional_velocity_index
  end type hydro_indices_t


  type ( hydro_indices_t ) :: hyid = hydro_indices_t ( &
    1, 2, 3, 4, 5, & ! primitive variables
    2, 3, 4, 5, & ! conserved variables (excluding rho)
    1, 2, 3 & ! directions
  )

  interface hy_specific_kinetic_energy
    procedure hy_specific_kinetic_energy_conserved
    procedure hy_specific_kinetic_energy_primitive
  end interface

  interface hy_specific_internal_energy
    procedure hy_specific_internal_energy_conserved
  end interface

contains

  !> Copy a given hydro cell into another one
  !> @param[in] source
  !> @param[out] traget
  subroutine hy_copy (source, target)
    implicit none

    type ( hydro_conserved_t ), intent(in) :: source
    type ( hydro_conserved_t ), intent(out) :: target

    target%u = source%u
  end subroutine hy_copy

  !> Converting primitive hydro state to conserved
  !> @param[in] primitive
  !> @param[in] e_int Internal energy
  !> @param[out] conserved
  pure subroutine hy_prim_to_cons (primitive, e_int, conserved)
    implicit none

    type ( hydro_primitive_t ), intent(in) :: primitive
    real(kind=8), intent(in) :: e_int
    type ( hydro_conserved_t ), intent(out) :: conserved

    conserved%u(hyid%rho) = primitive%w(hyid%rho)
    conserved%u(hyid%rho_u) = primitive%w(hyid%rho) * primitive%w(hyid%u)
    conserved%u(hyid%rho_v) = primitive%w(hyid%rho) * primitive%w(hyid%v)
    conserved%u(hyid%rho_w) = primitive%w(hyid%rho) * primitive%w(hyid%w)
    conserved%u(hyid%e_tot) = primitive%w(hyid%rho) &
    * 0.5d0 * sum(primitive%w(hyid%u:hyid%w)**2) + e_int
  end subroutine hy_prim_to_cons

  !> Specific Kinetic Energy of a given hydro conserved state
  !> @param[in] U a given conserved hydro state
  !> @return e_kin_sp
  pure function hy_specific_kinetic_energy_conserved (U) result (e_kin_sp)
    implicit none

    type ( hydro_conserved_t ), intent(in) :: U
    real(kind=8) :: e_kin_sp

    e_kin_sp = 0.5d0 * sum(U%u(hyid%rho_u:hyid%rho_w)**2) / U%u(hyid%rho)**2
  end function hy_specific_kinetic_energy_conserved

  !> Specific Kinetic Energy of a given hydro primitive state
  !> @param[in] W a given primitive hydro state
  !> @return e_kin_sp
  pure function hy_specific_kinetic_energy_primitive (W) result (e_kin_sp)
    implicit none

    type ( hydro_primitive_t ), intent(in) :: W
    real(kind=8) :: e_kin_sp

    e_kin_sp = 0.5d0 * sum(W%w(hyid%u:hyid%w)**2)
  end function hy_specific_kinetic_energy_primitive


  !> Specific Internal Energy of a given conserved state
  !> @param[in] U a given conserved hydro state
  !> @return e_int_sp
  pure function hy_specific_internal_energy_conserved (U) result (e_int_sp)
    implicit none

    type ( hydro_conserved_t ), intent(in) :: U
    real(kind=8) :: e_int_sp

    e_int_sp = ( U%u(hyid%e_tot) / U%u(hyid%rho) ) - hy_specific_kinetic_energy(U)
  end function hy_specific_internal_energy_conserved


  !> Return the velocity of a given direction
  pure function hy_get_directional_velocity_index (this, dir) result (v_idx)
    implicit none

    class ( hydro_indices_t ), intent(in) :: this
    integer, intent(in) :: dir
    integer :: v_idx

    v_idx = this%rho_u + dir - 1
  end function hy_get_directional_velocity_index
end module rhyme_hydro_base
