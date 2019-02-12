logical function rhyme_hydro_base_sp_kinetic_e_prim_test () result (failed)
  use rhyme_hydro_base_factory

  implicit none

  real ( kind=8 ) :: e_kin_sp

  e_kin_sp = 0.5d0 * ( u**2 + v**2 + w**2 )

  failed = abs ( hy_sp_kinetic_e_prim( prim ) - e_kin_sp ) > epsilon(0.e0)
end function rhyme_hydro_base_sp_kinetic_e_prim_test
