logical function rhyme_param_parser_parse_param_test () result ( failed )
  use rhyme_param_parser

  implicit none

  type ( samr_t ) :: samr
  type ( samr_boundary_condition_t ) :: bc
  type ( cfl_t ) :: cfl
  type ( chemistry_t ) :: chemi
  type ( ideal_gas_t ) :: ig
  type ( iterative_riemann_solver_config_t ) :: irs_config

  character(len=1024), parameter :: param_file = "parameters.conf.example"

  failed = .not. parse_params ( param_file, samr, bc, cfl, chemi, ig, irs_config )
  if ( failed ) return

  ! Structured AMR
  failed = &
  any ( samr%base_grid .ne. [ 32, 32, 1 ] ) &
  .or. any ( samr%ghost_cells .ne. [ 2, 2, 0 ] ) &
  .or. samr%nlevels .ne. 1 &
  .or. samr%nboxes .ne. 1

  if ( failed ) return

  ! Boundary Condition
  failed = &
  bc%types(bc_id%left) .ne. 1 &
  .or. bc%types(bc_id%right) .ne. 2 &
  .or. bc%types(bc_id%bottom) .ne. 3 &
  .or. bc%types(bc_id%top) .ne. 1 &
  .or. bc%types(bc_id%back) .ne. 2 &
  .or. bc%types(bc_id%front) .ne. 3

  if ( failed ) return

  ! Ideal Gas
  failed = ig%type .ne. igid%monatomic

  if ( failed ) return

  ! Iterative Riemann Solver
  failed = &
  abs ( irs_config%pressure_floor - 1.d-10 ) > epsilon(0.d0) &
  .or. abs ( irs_config%tolerance - 1.d-6 ) > epsilon(0.d0) &
  .or. irs_config%n_iteration .ne. 100

  if ( failed ) return

  ! CFL
  failed = abs ( cfl%courant_number - .81d0 ) > epsilon(0.d0)

end function rhyme_param_parser_parse_param_test
