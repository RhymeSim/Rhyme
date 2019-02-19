logical function rhyme_samr_bc_set_base_grid_boundaries_test () result (failed)
  use rhyme_samr_bc_factory

  implicit none

  type ( samr_bc_t ) :: bc

  call rhyme_samr_bc_factory_init

  bc%ghost_cells = ghost_cells

  call bc%set_base_grid_boundaries ( samr )

  failed = .false.
end function rhyme_samr_bc_set_base_grid_boundaries_test
