logical function rhyme_samr_bc_init_test () result (failed)
  use rhyme_samr_bc_factory

  implicit none

  type ( samr_bc_t ) :: bc
  type ( samr_box_t ) :: box


  call rhyme_samr_bc_factory_init

  bc%types = samr_bc_fac_bc_types
  call bc%init ( samr_bc_fac_samr, samr_bc_fac_log )

  box = samr_bc_fac_samr%levels(0)%boxes(1)
  failed = &
  .not. bc%initialized &
  .or. box%flags(-samr_bc_fac_ghost_cells(1) + 1, 1, 1) .ne. samrid%ghost &
  .or. box%flags(1, 1, 1) .eq. samrid%ghost &
  .or. box%flags(box%dims(1) + samr_bc_fac_ghost_cells(1), 1, 1) .ne. samrid%ghost &
  .or. box%flags(box%dims(1), 1, 1) .eq. samrid%ghost &
  .or. box%flags(1, -samr_bc_fac_ghost_cells(2) + 1, 1) .ne. samrid%ghost &
  .or. box%flags(1, box%dims(2) + samr_bc_fac_ghost_cells(2), 1) .ne. samrid%ghost &
  .or. box%flags(1, box%dims(2), 1) .eq. samrid%ghost &
  .or. any ( bc%ghost_cells .ne. samr_bc_fac_samr%ghost_cells )
end function rhyme_samr_bc_init_test
