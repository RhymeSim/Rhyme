logical function rhyme_hydro_base_sp_internal_e_test () result (failed)
  use rhyme_hydro_base_factory

  implicit none

  failed = abs ( &
    hy_sp_internal_e( cons ) - e_int / rho &
  ) > epsilon(0.e0)
end function rhyme_hydro_base_sp_internal_e_test
