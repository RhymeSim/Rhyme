logical function rhyme_samr_boundary_condition_init_with_test () result (failed)
  use rhyme_samr_boundary_condition

  implicit none

  type(samr_t) :: samr
  type(samr_boundary_condition_t) :: bc

  integer, parameter :: base_grid(3) = [ 32, 16, 8 ]
  integer, parameter :: nlevels = 3
  integer, parameter :: ghost_cells(3) = [2, 1, 0]
  integer, parameter :: bc_types(6) = [ &
    bcid%reflective, &
    bcid%outflow, &
    bcid%periodic, &
    bcid%reflective, &
    bcid%outflow, &
    bcid%periodic &
  ]

  integer :: tot_nboxes(0:23)

  tot_nboxes(0) = 1
  tot_nboxes(1) = 10
  tot_nboxes(2) = 100
  tot_nboxes(3) = 1000


  call samr%init_with ( base_grid, nlevels, tot_nboxes, ghost_cells )
  call bc%init_with ( samr, bc_types )

  failed = &
  .not. bc%initialized &
  .or. any ( bc%types .ne. bc_types ) &
  .or. samr%levels(0)%boxes(1)%flags(-ghost_cells(1) + 1, 1, 1) .ne. samrid%ghost &
  .or. samr%levels(0)%boxes(1)%flags(1, 1, 1) .eq. samrid%ghost &
  .or. samr%levels(0)%boxes(1)%flags(samr%levels(0)%boxes(1)%dims(1) + ghost_cells(1), 1, 1) .ne. samrid%ghost &
  .or. samr%levels(0)%boxes(1)%flags(samr%levels(0)%boxes(1)%dims(1), 1, 1) .eq. samrid%ghost &
  .or. samr%levels(0)%boxes(1)%flags(1, -ghost_cells(2) + 1, 1) .ne. samrid%ghost &
  .or. samr%levels(0)%boxes(1)%flags(1, samr%levels(0)%boxes(1)%dims(2) + ghost_cells(2), 1) .ne. samrid%ghost &
  .or. samr%levels(0)%boxes(1)%flags(1, samr%levels(0)%boxes(1)%dims(2), 1) .eq. samrid%ghost
end function rhyme_samr_boundary_condition_init_with_test
