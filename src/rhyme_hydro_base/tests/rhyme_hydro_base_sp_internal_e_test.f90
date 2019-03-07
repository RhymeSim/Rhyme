logical function rhyme_hydro_base_sp_internal_e_test () result (failed)
  use rhyme_hydro_base_factory

  implicit none

  type ( hydro_conserved_t ) :: cons
  type ( rhyme_hydro_factory_t ) :: hyfact

  cons = hyfact%conserved()

  failed = abs ( &
    hy_sp_internal_e( cons ) - hyfact%e_internal() / hyfact%rho &
  ) > epsilon(0.e0)
end function rhyme_hydro_base_sp_internal_e_test
