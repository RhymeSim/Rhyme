logical function rhyme_samr_bc_init_with_test () result ( failed )
  use rhyme_samr_bc_factory

  implicit none

  type ( samr_bc_t ) :: bc


  call rhyme_samr_bc_factory_init

  call bc%init_with ( samr_bc_fac_samr , samr_bc_fac_bc_types, samr_bc_fac_log )
  failed = any ( bc%types .ne. samr_bc_fac_bc_types)
end function rhyme_samr_bc_init_with_test
