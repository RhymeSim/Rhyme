module rhyme_riemann_problem
  implicit none

  type rp_shock_t
    real(kind=8) :: rho = 0.d0, speed = 0.d0
  end type rp_shock_t


  type rp_fan_t
    real(kind=8) :: rho = 0.d0, cs = 0.d0, speedH = 0.d0, speedT = 0.d0
  end type rp_fan_t


  type rp_side_t
    logical :: is_shock = .false.
    type(rp_shock_t) :: shock
    type(rp_fan_t) :: fan
  end type rp_side_t


  type rp_star_region_t
    real(kind=8) :: u = 0.d0, p = 0.d0
    type(rp_side_t) :: left, right
  end type rp_star_region_t
end module rhyme_riemann_problem
