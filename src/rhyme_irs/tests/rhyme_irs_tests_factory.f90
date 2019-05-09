module rhyme_irs_tests_factory
  use rhyme_irs
  use rhyme_ideal_gas_factory

  implicit none

contains

  !> Sod test (very mild test)
  !! a left rarefacton, a contact discontinuity and a right shock
  subroutine rhyme_irs_Sod_test ( ig, L, R, solution )
    implicit none

    type ( ideal_gas_t ), intent ( in ) :: ig
    type ( hydro_conserved_t ), intent ( out ) :: L, R
    type ( riemann_problem_solution_t ), intent ( out ) :: solution

    type ( hydro_primitive_t ) :: L_prim, R_prim
    real ( kind=8 ) :: e_int_L, e_int_R


    L_prim = hydro_primitive_t( [ 1.d0, 0.d0, 0.d0, 0.d0, 1.d0 ] )
    R_prim = hydro_primitive_t( [ .125d0, 0.d0, 0.d0, 0.d0, .1d0 ] )

    e_int_L = 1.d0 / ( ig%gamma - 1.d0 )
    e_int_R = .1d0 / ( ig%gamma - 1.d0 )

    call hy_prim_to_cons( L_prim, e_int_L, L )
    call hy_prim_to_cons( R_prim, e_int_R, R )

    call rhyme_irs_factory_set_sides( ig, L, R, solution )

    solution%star%p = 0.30313d0
    solution%star%u = 0.92745d0
    solution%star%left%is_shock = .false.
    solution%star%left%fan%rho = 0.42632d0
    solution%star%right%is_shock = .true.
    solution%star%right%shock%rho = 0.26557d0
  end subroutine rhyme_irs_Sod_test


  !> 123 test (einfeldt et al. 1991)
  !! Two strong rarefactions and a trivial stationary constact dicontinuity
  !! p* is too small and close to vacuum
  pure subroutine rhyme_irs_123_test ( ig, L, R, solution )
    implicit none

    type ( ideal_gas_t ), intent ( in ) :: ig
    type ( hydro_conserved_t ), intent(out) :: L, R
    type ( riemann_problem_solution_t ), intent(out) :: solution

    type ( hydro_primitive_t ) :: L_prim, R_prim
    real ( kind=8 ) :: e_int_L, e_int_R


    L_prim = hydro_primitive_t( [ 1.d0, -2.d0, 0.d0, 0.d0, .4d0 ] )
    R_prim = hydro_primitive_t( [ 1.d0, 2.d0, 0.d0, 0.d0, .4d0 ] )

    e_int_L = .4d0 / ( ig%gamma - 1.d0 )
    e_int_R = .4d0 / ( ig%gamma - 1.d0 )

    call hy_prim_to_cons( L_prim, e_int_L, L )
    call hy_prim_to_cons( R_prim, e_int_R, R )

    call rhyme_irs_factory_set_sides( ig, L, R, solution )

    solution%star%p = 0.00189d0
    solution%star%u = 0.d0
    solution%star%left%is_shock = .false.
    solution%star%left%fan%rho = 0.02185d0
    solution%star%right%is_shock = .false.
    solution%star%right%fan%rho = 0.02185d0
  end subroutine rhyme_irs_123_test


  !> Left half of the blast wave problem of Woodward and Colella
  !! a left rarefaction, a contact and a right shock (very severe)
  pure subroutine rhyme_irs_left_blast_wave_test ( ig, L, R, solution )
    implicit none

    type ( ideal_gas_t ), intent ( in ) :: ig
    type ( hydro_conserved_t ), intent ( out ) :: L, R
    type ( riemann_problem_solution_t ), intent ( out ) :: solution

    type ( hydro_primitive_t ) :: L_prim, R_prim
    real ( kind=8 ) :: e_int_L, e_int_R


    L_prim = hydro_primitive_t( [ 1.d0, 0.d0, 0.d0, 0.d0, 1.d3 ] )
    R_prim = hydro_primitive_t( [ 1.d0, 0.d0, 0.d0, 0.d0, 1.d-2 ] )

    e_int_L = 1.d3 / ( ig%gamma - 1.d0 )
    e_int_R = 1.d-2 / ( ig%gamma - 1.d0 )

    call hy_prim_to_cons( L_prim, e_int_L, L )
    call hy_prim_to_cons( R_prim, e_int_R, R )

    call rhyme_irs_factory_set_sides( ig, L, R, solution )

    solution%star%p = 460.894d0
    solution%star%u = 19.5975d0
    solution%star%left%is_shock = .false.
    solution%star%left%fan%rho = 0.57506d0
    solution%star%right%is_shock = .true.
    solution%star%right%shock%rho = 5.99924d0
  end subroutine rhyme_irs_left_blast_wave_test


  !> Right half of the blast wave problem of Woodward and Colella
  !! a left shock, a contact and a right rarefaction (very severe)
  pure subroutine rhyme_irs_right_blast_wave_test ( ig, L, R, solution )
    implicit none

    type ( ideal_gas_t ), intent ( in ) :: ig
    type ( hydro_conserved_t ), intent ( out ) :: L, R
    type ( riemann_problem_solution_t ), intent ( out ) :: solution

    type ( hydro_primitive_t ) :: L_prim, R_prim
    real ( kind=8 ) :: e_int_L, e_int_R

    L_prim = hydro_primitive_t( [ 1.d0, 0.d0, 0.d0, 0.d0, 1.d-2 ] )
    R_prim = hydro_primitive_t( [ 1.d0, 0.d0, 0.d0, 0.d0, 1.d2 ] )

    e_int_L = 1.d-2 / ( ig%gamma - 1.d0 )
    e_int_R = 1.d2 / ( ig%gamma - 1.d0 )

    call hy_prim_to_cons( L_prim, e_int_L, L )
    call hy_prim_to_cons( R_prim, e_int_R, R )

    call rhyme_irs_factory_set_sides( ig, L, R, solution )

    solution%star%p = 46.0950d0
    solution%star%u = -6.19633d0
    solution%star%left%is_shock = .true.
    solution%star%left%shock%rho = 5.99242d0
    solution%star%right%is_shock = .false.
    solution%star%right%fan%rho = 0.57511d0
  end subroutine rhyme_irs_right_blast_wave_test


  !> Collision of the strong shocks emerging from blast waves
  !! a left facing shock (travelling very slowly to the right), a right
  !! travelling contact discontinuity and a right travelling shock wave
  pure subroutine rhyme_irs_two_shocks_collision_test ( ig, L, R, solution )
    implicit none

    type ( ideal_gas_t ), intent ( in ) :: ig
    type ( hydro_conserved_t ), intent ( out ) :: L, R
    type ( riemann_problem_solution_t ), intent ( out ) :: solution

    type ( hydro_primitive_t ) :: L_prim, R_prim
    real ( kind=8 ) :: e_int_L, e_int_R

    L_prim = hydro_primitive_t( [ 5.99924d0, 19.5975d0, 0.d0, 0.d0, 460.894d0 ] )
    R_prim = hydro_primitive_t( [ 5.99924d0, -6.19633d0, 0.d0, 0.d0, 46.0950d0 ] )

    e_int_L = 460.894d0 / ( ig%gamma - 1.d0 )
    e_int_R = 46.0950d0 / ( ig%gamma - 1.d0 )

    call hy_prim_to_cons( L_prim, e_int_L, L )
    call hy_prim_to_cons( R_prim, e_int_R, R )

    call rhyme_irs_factory_set_sides( ig, L, R, solution )

    solution%star%p = 1691.64d0
    solution%star%u = 8.68975d0
    solution%star%left%is_shock = .true.
    solution%star%left%shock%rho = 14.2823d0
    solution%star%right%is_shock = .true.
    solution%star%right%shock%rho = 31.0426d0
  end subroutine rhyme_irs_two_shocks_collision_test


  pure subroutine rhyme_irs_factory_set_sides ( ig, L, R, s )
    implicit none

    type ( ideal_gas_t ), intent ( in ) :: ig
    type ( hydro_conserved_t ), intent ( in ) :: L, R
    type ( riemann_problem_solution_t ), intent ( inout ) :: s

    s%left%rho = L%u( hyid%rho )
    s%right%rho = R%u( hyid%rho )

    s%left%v = L%u( hyid%rho_u:hyid%rho_w ) / L%u(hyid%rho)
    s%right%v = R%u( hyid%rho_u:hyid%rho_w ) / R%u(hyid%rho)

    s%left%p = ig%p(L)
    s%right%p = ig%p(R)

    s%left%cs = ig%cs(L)
    s%right%cs = ig%cs(R)
  end subroutine rhyme_irs_factory_set_sides
end module rhyme_irs_tests_factory
