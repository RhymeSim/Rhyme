logical function rhyme_irs_test_cases_test () result (failed)
  use rhyme_iterative_riemann_solver_factory

  implicit none

  logical :: failed_sod, failed_123, failed_left, failed_right, failed_shocks

  call rhyme_iterative_riemann_solver_factory_init

  call irs_test_cases ( irs_Sod_test, "Sod", irs_sod_acc, failed_sod )
  call irs_test_cases ( irs_123_test, "123 test", irs_123_acc, failed_123 )
  call irs_test_cases ( irs_left_blast_wave_test, "left blast wave", irs_lblast_acc, failed_left )
  call irs_test_cases ( irs_right_blast_wave_test, "right blast wave", irs_rblast_acc, failed_right )
  call irs_test_cases ( irs_two_shocks_collision_test, "two shock collision", irs_two_shocks_acc, failed_shocks )

  failed = failed_sod .or. failed_123 .or. failed_left .or. failed_right .or. failed_shocks

contains

  subroutine irs_test_cases ( func, test_name, acc, failed )
    implicit none

    external :: func
    character(len=*) :: test_name
    type ( irs_accuracy_t ) :: acc
    logical :: failed

    type ( hydro_conserved_t ) :: L, R
    type ( rp_star_region_t ) :: expected_star, star
    real(kind=8) :: ex_p, ex_u, ex_left_rho, ex_right_rho
    real(kind=8) :: star_left_rho, star_right_rho
    logical :: failed_p, failed_v, failed_lshock, failed_rshock, failed_lrho, failed_rrho

    call func (L, R, expected_star)
    ex_p = expected_star%p
    ex_u = expected_star%u

    call irs%solve ( L, R, hyid%x, star )

    if ( expected_star%left%is_shock ) then
      ex_left_rho = expected_star%left%shock%rho
      star_left_rho = star%left%shock%rho
    else
      ex_left_rho = expected_star%left%fan%rho
      star_left_rho = star%left%fan%rho
    end if

    if ( expected_star%right%is_shock ) then
      ex_right_rho = expected_star%right%shock%rho
      star_right_rho = star%right%shock%rho
    else
      ex_right_rho = expected_star%right%fan%rho
      star_right_rho = star%right%fan%rho
    end if


    failed_p = abs ( ( star%p - ex_p ) / ex_p ) > acc%star_p

    if ( failed_p ) then
      print *, test_name // " star region pressure"
      print *, "  - accuracy: ", abs ( ( star%p - ex_p ) / ex_p ), "  - expected: ", acc%star_p
    end if


    failed_v = abs ( ( star%u - ex_u ) / ex_u ) > acc%star_v

    if ( failed_v ) then
      print *, test_name // " star region velocity"
      print *, "  - accuracy: ", abs ( ( star%u - ex_u ) / ex_u ), "  - expected: ", acc%star_v
    end if


    failed_lshock = .not. star%left%is_shock .eqv. expected_star%left%is_shock
    if ( failed_lshock ) print *, test_name // " left shock test has failed"


    failed_lrho = abs ( ( star_left_rho - ex_left_rho ) / ex_left_rho ) > acc%left_rho

    if ( failed_lrho ) then
      print *, test_name // " left wave density"
      print *, "  - accuracy: ", abs ( ( star_left_rho - ex_left_rho ) / ex_left_rho ) &
      , "  - expected: ", acc%left_rho
    end if


    failed_rshock = .not. star%right%is_shock .eqv. expected_star%right%is_shock
    if ( failed_rshock ) print *, test_name // " right shock test has failed"


    failed_rrho = abs ( ( star_right_rho - ex_right_rho ) / ex_right_rho ) > acc%right_rho

    if ( failed_rrho ) then
      print *, test_name //" right wave density"
      print *, "  - accuracy: ", abs ( ( star_right_rho - ex_right_rho ) / ex_right_rho ) &
      , "  - expected: ", acc%right_rho
    end if

    failed = failed_p .or. failed_v .or. failed_lshock .or. failed_lrho .or. failed_rshock .or. failed_rrho
  end subroutine irs_test_cases

end function rhyme_irs_test_cases_test
