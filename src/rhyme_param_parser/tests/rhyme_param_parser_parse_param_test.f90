logical function rhyme_param_parser_parse_param_test () result ( failed )
  use rhyme_param_parser

  implicit none

  type ( samr_t ) :: samr
  type ( samr_boundary_condition_t ) :: bc
  type ( chemistry_t ) :: chemi
  type ( ideal_gas_t ) :: ig

  character(len=1024), parameter :: param_file = "parameters.conf.example"

  failed = .not. parse_params ( param_file, samr, bc, chemi, ig )
  if ( failed ) return

  ! SAMR tests
  failed = &
  any ( samr%base_grid .ne. [ 32, 32, 1 ] ) &
  .or. any ( samr%ghost_cells .ne. [ 2, 2, 0 ] ) &
  .or. samr%nlevels .ne. 1 &
  .or. samr%nboxes .ne. 1

  if ( failed ) return

  ! Boundary condition tests
  failed = &
  bc%types(bc_id%left) .ne. 1 &
  .or. bc%types(bc_id%right) .ne. 2 &
  .or. bc%types(bc_id%bottom) .ne. 3 &
  .or. bc%types(bc_id%top) .ne. 1 &
  .or. bc%types(bc_id%back) .ne. 2 &
  .or. bc%types(bc_id%front) .ne. 3

  if ( failed ) return

  ! Ideal gas
  failed = ig%type .ne. igid%monatomic

end function rhyme_param_parser_parse_param_test
