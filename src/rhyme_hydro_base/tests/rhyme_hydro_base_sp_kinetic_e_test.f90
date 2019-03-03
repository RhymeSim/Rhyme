logical function rhyme_hydro_base_sp_kinetic_e_test () result (failed)
  use rhyme_hydro_base_factory

  implicit none

  type ( hydro_conserved_t ) :: cons
  real ( kind=8 ) :: e_kin_sp

  cons = hyfact%cons()
  e_kin_sp = 0.5d0 * ( hyfact%u**2 + hyfact%v**2 + hyfact%w**2 )

  failed = abs ( hy_sp_kinetic_e( cons ) - e_kin_sp ) > epsilon(0.e0)
end function rhyme_hydro_base_sp_kinetic_e_test
