module rhyme_irs_tests_factory
  use rhyme_irs
  use rhyme_ideal_gas_factory

  implicit none

contains

  !> Sod test (very mild test)
  !! a left rarefacton, a contact discontinuity and a right shock
  subroutine rhyme_irs_Sod_test ( l, r, solution )
    implicit none

    real ( kind=8 ), dimension ( cid%rho:cid%e_tot ), intent ( out ) :: l, r
    type ( riemann_problem_solution_t ), intent ( out ) :: solution

    real ( kind=8 ), dimension ( cid%rho:cid%p ) :: l_prim = 0.d0, r_prim = 0.d0
    real ( kind=8 ) :: e_int_l, e_int_r

    l_prim( cid%rho ) = 1.d0
    l_prim( cid%u ) = 0.d0
    l_prim( cid%p ) = 1.d0

    r_prim( cid%rho ) = .125d0
    r_prim( cid%u ) = 0.d0
    r_prim( cid%p ) = .1d0

    e_int_l = l_prim( cid%p ) / ( get_gamma() - 1.d0 )
    e_int_r = r_prim( cid%p ) / ( get_gamma() - 1.d0 )

    call rhyme_hydro_base_primitive_to_conserved( l_prim, e_int_l, l )
    call rhyme_hydro_base_primitive_to_conserved( r_prim, e_int_r, r )

    call rhyme_irs_factory_set_sides( l, r, solution )

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
  subroutine rhyme_irs_123_test ( l, r, solution )
    implicit none

    real ( kind=8 ), dimension ( cid%rho:cid%e_tot ), intent(out) :: l, r
    type ( riemann_problem_solution_t ), intent(out) :: solution

    real ( kind=8 ), dimension ( cid%rho:cid%p ) :: l_prim = 0.d0, r_prim = 0.d0
    real ( kind=8 ) :: e_int_l, e_int_r


    l_prim( cid%rho ) = 1.d0
    l_prim( cid%u ) = -2.d0
    l_prim( cid%p ) = .4d0

    r_prim( cid%rho ) = 1.d0
    r_prim( cid%u ) = 2.d0
    r_prim( cid%p ) = .4d0

    e_int_l = l_prim( cid%p ) / ( get_gamma() - 1.d0 )
    e_int_r = r_prim( cid%p ) / ( get_gamma() - 1.d0 )

    call rhyme_hydro_base_primitive_to_conserved( l_prim, e_int_l, l )
    call rhyme_hydro_base_primitive_to_conserved( r_prim, e_int_r, r )

    call rhyme_irs_factory_set_sides( l, r, solution )

    solution%star%p = 0.00189d0
    solution%star%u = 0.d0
    solution%star%left%is_shock = .false.
    solution%star%left%fan%rho = 0.02185d0
    solution%star%right%is_shock = .false.
    solution%star%right%fan%rho = 0.02185d0
  end subroutine rhyme_irs_123_test


  !> left half of the blast wave problem of Woodward and Colella
  !! a left rarefaction, a contact and a right shock (very severe)
  subroutine rhyme_irs_left_blast_wave_test ( l, r, solution )
    implicit none

    real ( kind=8 ), dimension ( cid%rho:cid%e_tot ), intent ( out ) :: l, r
    type ( riemann_problem_solution_t ), intent ( out ) :: solution

    real ( kind=8 ), dimension ( cid%rho:cid%p ) :: l_prim = 0.d0, r_prim = 0.d0
    real ( kind=8 ) :: e_int_l, e_int_r


    l_prim( cid%rho ) = 1.d0
    l_prim( cid%u ) = 0.d0
    l_prim( cid%p ) = 1.d3

    r_prim( cid%rho ) = 1.d0
    r_prim( cid%u ) = 0.d0
    r_prim( cid%p ) = 1.d-2

    e_int_l = l_prim( cid%p ) / ( get_gamma() - 1.d0 )
    e_int_r = r_prim( cid%p ) / ( get_gamma() - 1.d0 )

    call rhyme_hydro_base_primitive_to_conserved( l_prim, e_int_l, l )
    call rhyme_hydro_base_primitive_to_conserved( r_prim, e_int_r, r )

    call rhyme_irs_factory_set_sides( l, r, solution )

    solution%star%p = 460.894d0
    solution%star%u = 19.5975d0
    solution%star%left%is_shock = .false.
    solution%star%left%fan%rho = 0.57506d0
    solution%star%right%is_shock = .true.
    solution%star%right%shock%rho = 5.99924d0
  end subroutine rhyme_irs_left_blast_wave_test


  !> right half of the blast wave problem of Woodward and Colella
  !! a left shock, a contact and a right rarefaction (very severe)
  subroutine rhyme_irs_right_blast_wave_test ( l, r, solution )
    implicit none

    real ( kind=8 ), dimension ( cid%rho:cid%e_tot ), intent ( out ) :: l, r
    type ( riemann_problem_solution_t ), intent ( out ) :: solution

    real ( kind=8 ), dimension ( cid%rho:cid%p ) :: l_prim = 0.d0, r_prim = 0.d0
    real ( kind=8 ) :: e_int_l, e_int_r

    l_prim( cid%rho ) = 1.d0
    l_prim( cid%u ) = 0.d0
    l_prim( cid%p ) = 1.d-2

    r_prim( cid%rho ) = 1.d0
    r_prim( cid%u ) = 0.d0
    r_prim( cid%p ) = 1.d2

    e_int_l = l_prim( cid%p ) / ( get_gamma() - 1.d0 )
    e_int_r = r_prim( cid%p ) / ( get_gamma() - 1.d0 )

    call rhyme_hydro_base_primitive_to_conserved( l_prim, e_int_l, l )
    call rhyme_hydro_base_primitive_to_conserved( r_prim, e_int_r, r )

    call rhyme_irs_factory_set_sides( l, r, solution )

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
  subroutine rhyme_irs_two_shocks_collision_test ( l, r, solution )
    implicit none

    real ( kind=8 ), dimension ( cid%rho:cid%e_tot ), intent ( out ) :: l, r
    type ( riemann_problem_solution_t ), intent ( out ) :: solution

    real ( kind=8 ), dimension ( cid%rho:cid%p ) :: l_prim = 0.d0, r_prim = 0.d0
    real ( kind=8 ) :: e_int_l, e_int_r

    l_prim( cid%rho ) = 5.99924d0
    l_prim( cid%u ) = 19.5975d0
    l_prim( cid%p ) = 460.894d0

    r_prim( cid%rho ) = 5.99924d0
    r_prim( cid%u ) = -6.19633d0
    r_prim( cid%p ) = 46.0950d0

    e_int_l = l_prim( cid%p ) / ( get_gamma() - 1.d0 )
    e_int_r = r_prim( cid%p ) / ( get_gamma() - 1.d0 )

    call rhyme_hydro_base_primitive_to_conserved( l_prim, e_int_l, l )
    call rhyme_hydro_base_primitive_to_conserved( r_prim, e_int_r, r )

    call rhyme_irs_factory_set_sides( l, r, solution )

    solution%star%p = 1691.64d0
    solution%star%u = 8.68975d0
    solution%star%left%is_shock = .true.
    solution%star%left%shock%rho = 14.2823d0
    solution%star%right%is_shock = .true.
    solution%star%right%shock%rho = 31.0426d0
  end subroutine rhyme_irs_two_shocks_collision_test


  subroutine rhyme_irs_factory_set_sides ( l, r, s )
    implicit none

    real ( kind=8 ), dimension ( cid%rho:cid%e_tot ), intent ( in ) :: l, r
    type ( riemann_problem_solution_t ), intent ( inout ) :: s

    s%left%rho = l( cid%rho )
    s%right%rho = r( cid%rho )

    s%left%v = l( cid%rho_u:cid%rho_u+NDIM-1 ) / l( cid%rho )
    s%right%v = r( cid%rho_u:cid%rho_u+NDIM-1 ) / r( cid%rho )

    s%left%p = calc_p(l)
    s%right%p = calc_P(r)

    s%left%cs = calc_cs(l)
    s%right%cs = calc_cs(r)
  end subroutine rhyme_irs_factory_set_sides
end module rhyme_irs_tests_factory
