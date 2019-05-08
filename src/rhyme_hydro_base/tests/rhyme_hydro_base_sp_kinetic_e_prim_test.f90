logical function rhyme_hydro_base_sp_kinetic_e_prim_test () result (failed)
  use rhyme_hydro_base_factory
  use rhyme_assertion

  implicit none

  type ( assertion_t ) :: hy_tester

  type ( hydro_primitive_t ) :: prim
  real ( kind=8 ) :: e_kin_sp

  hy_tester = .describe. "hydro_base sp_kinetic_e_prim"

  prim = hy_factory%primitive()
  e_kin_sp = 0.5d0 * ( hy_factory%u**2 + hy_factory%v**2 + hy_factory%w**2 )

  call hy_tester%expect( hy_sp_kinetic_e_prim( prim ) .toBe. e_kin_sp )

  failed = hy_tester%failed()
end function rhyme_hydro_base_sp_kinetic_e_prim_test
