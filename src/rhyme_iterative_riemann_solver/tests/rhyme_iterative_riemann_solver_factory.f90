module rhyme_iterative_riemann_solver_factory
  use rhyme_iterative_riemann_solver
  use rhyme_ideal_gas

  implicit none

  logical :: irs_factory_initialized = .false.

  integer, parameter :: irs_factory_n_iteration = 101
  integer, parameter :: irs_factory_gastype = igid%diatomic

  real ( kind=8 ), parameter :: irs_factory_pressure_floor = 1.01d-10
  real ( kind=8 ), parameter :: irs_factory_tolerance = 1.01d-6

  type ( ideal_gas_t ) :: ig
  type ( iterative_riemann_solver_t ) :: irs

contains


  subroutine rhyme_iterative_riemann_solver_factory_init ()
    implicit none

    if ( irs_factory_initialized ) return

    call ig%init_with ( irs_factory_gastype )
    call irs%init_with ( ig, irs_factory_n_iteration, irs_factory_tolerance, &
      irs_factory_pressure_floor )

    irs_factory_initialized = .true.
  end subroutine rhyme_iterative_riemann_solver_factory_init

  !> Sod test (very mild test)
  !! a left rarefacton, a contact discontinuity and a right shock
  subroutine irs_Sod_test (L, R, star)
    implicit none

    type(hydro_conserved_t), intent(out) :: L, R
    type(rp_star_region_t), intent(out) :: star

    type(hydro_primitive_t) :: L_prim, R_prim
    real(kind=8) :: e_int_L, e_int_R


    L_prim = hydro_primitive_t([1.d0, 0.d0, 0.d0, 0.d0, 1.d0])
    R_prim = hydro_primitive_t([.125d0, 0.d0, 0.d0, 0.d0, .1d0])

    e_int_L = 1.d0 / (irs%ig%gamma - 1.d0)
    e_int_R = .1d0 / (irs%ig%gamma - 1.d0)

    call hy_prim_to_cons(L_prim, e_int_L, L)
    call hy_prim_to_cons(R_prim, e_int_R, R)


    star%p = 0.30313d0
    star%u = 0.92745d0
    star%left%is_shock = .false.
    star%left%fan%rho = 0.42632d0
    star%right%is_shock = .true.
    star%right%shock%rho = 0.26557d0
  end subroutine irs_Sod_test


  !> 123 test (einfeldt et al. 1991)
  !! Two strong rarefactions and a trivial stationary constact dicontinuity
  !! p* is too small and close to vacuum
  pure subroutine irs_123_test (L, R, star)
    implicit none

    type(hydro_conserved_t), intent(out) :: L, R
    type(rp_star_region_t), intent(out) :: star

    type(hydro_primitive_t) :: L_prim, R_prim
    real(kind=8) :: e_int_L, e_int_R


    L_prim = hydro_primitive_t([1.d0, -2.d0, -2.d0, -2.d0, .4d0])
    R_prim = hydro_primitive_t([1.d0, 2.d0, 2.d0, 2.d0, .4d0])

    e_int_L = .4d0 / (irs%ig%gamma - 1.d0)
    e_int_R = .4d0 / (irs%ig%gamma - 1.d0)

    call hy_prim_to_cons(L_prim, e_int_L, L)
    call hy_prim_to_cons(R_prim, e_int_R, R)

    star%p = 0.00189d0
    star%u = 0.d0
    star%left%is_shock = .false.
    star%left%fan%rho = 0.02185d0
    star%right%is_shock = .false.
    star%right%fan%rho = 0.02185d0
  end subroutine irs_123_test


  !> Left half of the blast wave problem of Woodward and Colella
  !! a left rarefaction, a contact and a right shock (very severe)
  pure subroutine irs_left_blast_wave_test (L, R, star)
    implicit none

    type(hydro_conserved_t), intent(out) :: L, R
    type(rp_star_region_t), intent(out) :: star

    type(hydro_primitive_t) :: L_prim, R_prim
    real(kind=8) :: e_int_L, e_int_R


    L_prim = hydro_primitive_t([1.d0, 0.d0, 0.d0, 0.d0, 1.d3])
    R_prim = hydro_primitive_t([1.d0, 0.d0, 0.d0, 0.d0, 1.d-2])

    e_int_L = 1.d3 / (irs%ig%gamma - 1.d0)
    e_int_R = 1.d-2 / (irs%ig%gamma - 1.d0)

    call hy_prim_to_cons(L_prim, e_int_L, L)
    call hy_prim_to_cons(R_prim, e_int_R, R)

    star%p = 460.894d0
    star%u = 19.5975d0
    star%left%is_shock = .false.
    star%left%fan%rho = 0.57506d0
    star%right%is_shock = .true.
    star%right%shock%rho = 5.99924d0
  end subroutine irs_left_blast_wave_test


  !> Right half of the blast wave problem of Woodward and Colella
  !! a left shock, a contact and a right rarefaction (very severe)
  pure subroutine irs_right_blast_wave_test (L, R, star)
    implicit none

    type(hydro_conserved_t), intent(out) :: L, R
    type(rp_star_region_t), intent(out) :: star

    type(hydro_primitive_t) :: L_prim, R_prim
    real(kind=8) :: e_int_L, e_int_R

    L_prim = hydro_primitive_t([1.d0, 0.d0, 0.d0, 0.d0, 1.d-2])
    R_prim = hydro_primitive_t([1.d0, 0.d0, 0.d0, 0.d0, 1.d2])

    e_int_L = 1.d-2 / (irs%ig%gamma - 1.d0)
    e_int_R = 1.d2 / (irs%ig%gamma - 1.d0)

    call hy_prim_to_cons(L_prim, e_int_L, L)
    call hy_prim_to_cons(R_prim, e_int_R, R)

    star%p = 46.0950d0
    star%u = -6.19633d0
    star%left%is_shock = .true.
    star%left%shock%rho = 5.99242d0
    star%right%is_shock = .false.
    star%right%fan%rho = 0.57511d0
  end subroutine irs_right_blast_wave_test


  !> Collision of thw stronc shocks emerging from blast waves
  !! a left facing shock (travelling very slowly to the right), a right
  !! travelling contact discontinuity and a right travelling shock wave
  pure subroutine irs_two_shocks_collision_test (L, R, star)
    implicit none

    type(hydro_conserved_t), intent(out) :: L, R
    type(rp_star_region_t), intent(out) :: star

    type(hydro_primitive_t) :: L_prim, R_prim
    real(kind=8) :: e_int_L, e_int_R

    L_prim = hydro_primitive_t([5.99924d0, 19.5975d0, 19.5975d0, 19.5975d0, 460.894d0])
    R_prim = hydro_primitive_t([5.99924d0, -6.19633d0, -6.19633d0, -6.19633d0, 46.0950d0])

    e_int_L = 460.894d0 / (irs%ig%gamma - 1.d0)
    e_int_R = 46.0950d0 / (irs%ig%gamma - 1.d0)

    call hy_prim_to_cons (L_prim, e_int_L, L)
    call hy_prim_to_cons (R_prim, e_int_R, R)

    star%p = 1691.64d0
    star%u = 8.68975d0
    star%left%is_shock = .true.
    star%left%shock%rho = 14.2823d0
    star%right%is_shock = .true.
    star%right%shock%rho = 31.0426d0
  end subroutine irs_two_shocks_collision_test
end module rhyme_iterative_riemann_solver_factory
