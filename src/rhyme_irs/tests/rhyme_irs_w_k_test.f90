logical function rhyme_irs_w_k_test () result ( failed )
  use rhyme_irs_factory
  use rhyme_ideal_gas_factory
  use rhyme_hydro_base_factory
  use rhyme_assertion

  implicit none

  type ( assertion_t ) :: irs_tester

  type ( ideal_gas_t ) :: ig
  type ( rp_side_t ) :: state
  type ( hydro_conserved_t ) :: U

  irs_tester = .describe. "irs_w_k"

  ig = ig_factory%generate( igid%monatomic )
  call hy_factory%init

  state%rho = hy_factory%rho
  state%v(1) = hy_factory%u
  state%v(2) = hy_factory%v
  state%v(3) = hy_factory%w
  state%p = hy_factory%p

  U = irs_w_k( ig, state )

  call irs_tester%expect( U%u .toBe. hy_factory%cons%u )

  failed = irs_tester%failed()
end function rhyme_irs_w_k_test
