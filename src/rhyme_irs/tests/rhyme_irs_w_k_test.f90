logical function rhyme_irs_w_k_test () result ( failed )
  use rhyme_irs_factory
  use rhyme_hydro_base_factory

  implicit none

  type ( rp_side_t ) :: state
  type ( hydro_conserved_t ) :: U
  type ( rhyme_hydro_factory_t ) :: hy

  call hy%init
  call rhyme_irs_factory_init

  state%rho = hy%rho
  state%v(1) = hy%u
  state%v(2) = hy%v
  state%v(3) = hy%w
  state%p = hy%p

  U = irs_w_k( irs_fac_ig_mon, state )

  failed = any( abs( U%u - hy%cons%u ) > epsilon(0.d0) )
end function rhyme_irs_w_k_test
