logical function rhyme_samr_bc_init_with_test () result ( failed )
  use rhyme_samr_bc_factory
  use rhyme_assertion

  implicit none

  type ( assertion_t ) :: bc_tester

  type ( samr_bc_t ) :: bc

  bc_tester = .describe. "samr_bc_init_with"

  call rhyme_samr_bc_factory_init

  call bc%init_with ( samr_bc_fac_samr , samr_bc_fac_bc_types, samr_bc_fac_log )

  call bc_tester%expect( bc%types .toBe. samr_bc_fac_bc_types )

  failed = bc_tester%failed()
end function rhyme_samr_bc_init_with_test
