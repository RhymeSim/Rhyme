logical function rhyme_chemistry_one_over_mu_test () result (failed)
  use rhyme_chemistry_factory
  use rhyme_physics_factory
  use rhyme_logger_factory
  use rhyme_assertion

  implicit none

  type ( assertion_t ) :: ch_tester

  type ( chemistry_t ) :: chemistry
  type ( physics_t ) :: physics
  type ( logger_t ) :: logger

  real ( kind=8 ) :: hydrogen_mass_fraction, hydrogen_neutral_fraction
  real ( kind=8 ) :: helium_neutral_fractions(2)
  real ( kind=8 ) :: X, Y, f(3), one_over_mu
  real ( kind=8 ) :: n_H, n_He, N, one_over_mu_exp
  integer :: i

  ch_tester = .describe. "chemistry one_over_mu"

  call rhyme_nombre_init

  chemistry = ch_factory%generate()
  physics = ph_factory%generate()
  logger = log_factory%generate()

  call rhyme_chemistry_init( chemistry, physics, logger )

  do i = 1, 100
    call random_number( hydrogen_mass_fraction )
    call random_number( hydrogen_neutral_fraction )

    X = hydrogen_mass_fraction
    Y = 1 - X

    f(1) = hydrogen_neutral_fraction

    helium_neutral_fractions = 1
    do while ( sum( helium_neutral_fractions ) > 1d0 )
      call random_number( helium_neutral_fractions )
    end do
    f(2:3) = helium_neutral_fractions

    one_over_mu = rhyme_chemistry_one_over_mu( chemistry, X, Y, f )

    n_H = 1 * X / chemistry%amu%H
    n_He = 1 * Y / chemistry%amu%He
    N = n_H * ( 1 + (1 - f(1)) ) + n_He * ( 1 + (1 - f(2)) + 2 * (1 - f(2) - f(3)) )
    one_over_mu_exp = N * chemistry%amu%H

    call ch_tester%expect( one_over_mu / one_over_mu_exp .toBe. 1d0 .within. 3 )
  end do

  failed = ch_tester%failed()
end function rhyme_chemistry_one_over_mu_test
