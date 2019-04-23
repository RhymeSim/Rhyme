logical function rhyme_hydro_base_sp_kinetic_e_prim_test () result (failed)
  use rhyme_hydro_base_factory
  use rhyme_assertion

  implicit none

  type ( assertion_t ) :: hy_tester

  type ( hydro_primitive_t ) :: prim
  real ( kind=8 ) :: e_kin_sp
  type ( rhyme_hydro_factory_t ) :: hyfact

  hy_tester = .describe. "hydro_base sp_kinetic_e_prim"

  prim = hyfact%primitive()
  e_kin_sp = 0.5d0 * ( hyfact%u**2 + hyfact%v**2 + hyfact%w**2 )

  call hy_tester%expect( hy_sp_kinetic_e_prim( prim ) .toBe. e_kin_sp )

  failed = hy_tester%failed()
end function rhyme_hydro_base_sp_kinetic_e_prim_test
