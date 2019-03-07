logical function rhyme_hydro_base_sp_kinetic_e_prim_test () result (failed)
  use rhyme_hydro_base_factory

  implicit none

  type ( hydro_primitive_t ) :: prim
  real ( kind=8 ) :: e_kin_sp
  type ( rhyme_hydro_factory_t ) :: hyfact

  prim = hyfact%primitive()
  e_kin_sp = 0.5d0 * ( hyfact%u**2 + hyfact%v**2 + hyfact%w**2 )

  failed = abs ( hy_sp_kinetic_e_prim( prim ) - e_kin_sp ) > epsilon(0.e0)
end function rhyme_hydro_base_sp_kinetic_e_prim_test
