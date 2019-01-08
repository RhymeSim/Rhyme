module rhyme_riemann_problem
  implicit none

  type rp_shock_t
    real(kind=8) :: rho, speed
  end type rp_shock_t


  type rp_fan_t
    real(kind=8) :: rho, cs, speedH, speedT
  end type rp_fan_t


  type rp_side_t
    logical :: is_shock
    type(rp_shock_t) :: shock
    type(rp_fan_t) :: fan
  end type rp_side_t


  type rp_star_region_t
    real(kind=8) :: u, p
    type(rp_side_t) :: left, right
  end type rp_star_region_t
end module rhyme_riemann_problem
