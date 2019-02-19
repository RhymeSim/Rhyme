logical function rhyme_samr_bc_init_with_test () result ( failed )
  use rhyme_samr_bc_factory

  implicit none

  type ( samr_bc_t ) :: bc


  call rhyme_samr_bc_factory_init

  call bc%init_with ( samr , bc_types )
  failed = any ( bc%types .ne. bc_types)
end function rhyme_samr_bc_init_with_test
