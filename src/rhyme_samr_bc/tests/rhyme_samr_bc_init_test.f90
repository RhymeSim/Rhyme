logical function rhyme_samr_bc_init_test () result (failed)
  use rhyme_samr_bc_factory

  implicit none

  type ( samr_bc_t ) :: bc


  call rhyme_samr_bc_factory_init

  bc%types = bc_types
  call bc%init ( samr )

  failed = &
  .not. bc%initialized &
  .or. samr%levels(0)%boxes(1)%flags(-ghost_cells(1) + 1, 1, 1) .ne. samrid%ghost &
  .or. samr%levels(0)%boxes(1)%flags(1, 1, 1) .eq. samrid%ghost &
  .or. samr%levels(0)%boxes(1)%flags(samr%levels(0)%boxes(1)%dims(1) + ghost_cells(1), 1, 1) .ne. samrid%ghost &
  .or. samr%levels(0)%boxes(1)%flags(samr%levels(0)%boxes(1)%dims(1), 1, 1) .eq. samrid%ghost &
  .or. samr%levels(0)%boxes(1)%flags(1, -ghost_cells(2) + 1, 1) .ne. samrid%ghost &
  .or. samr%levels(0)%boxes(1)%flags(1, samr%levels(0)%boxes(1)%dims(2) + ghost_cells(2), 1) .ne. samrid%ghost &
  .or. samr%levels(0)%boxes(1)%flags(1, samr%levels(0)%boxes(1)%dims(2), 1) .eq. samrid%ghost &
  .or. any ( bc%ghost_cells .ne. samr%ghost_cells )
end function rhyme_samr_bc_init_test
