logical function rhyme_param_parser_parse_param_test () result ( failed )
  use rhyme_param_parser

  implicit none

  type ( samr_t ) :: samr
  type ( samr_boundary_condition_t ) :: bc
  type ( cfl_t ) :: cfl
  type ( ideal_gas_t ) :: ig
  type ( initial_condition_t ) :: ic
  type ( iterative_riemann_solver_config_t ) :: irs_config

  character(len=1024), parameter :: param_file = "parameters.conf.example"

  failed = .not. parse_params ( param_file, samr, bc, cfl, ig, ic, irs_config )
  if ( failed ) return

  ! Structured AMR
  failed = &
  any ( samr%base_grid .ne. [ 128, 128, 1 ] ) &
  .or. any ( samr%ghost_cells .ne. [ 2, 2, 0 ] ) &
  .or. samr%nlevels .ne. 1 &
  .or. samr%nboxes .ne. 1

  if ( failed ) return

  ! Boundary Condition
  failed = &
  bc%types(bcid%left) .ne. 1 &
  .or. bc%types(bcid%right) .ne. 2 &
  .or. bc%types(bcid%bottom) .ne. 3 &
  .or. bc%types(bcid%top) .ne. 1 &
  .or. bc%types(bcid%back) .ne. 2 &
  .or. bc%types(bcid%front) .ne. 3

  if ( failed ) return

  ! Ideal Gas
  failed = ig%type .ne. igid%monatomic

  if ( failed ) return

  ! Initial Condition
  failed = &
  any ( abs ( ic%background%w - [.125d0, 0.d0, 0.d0, 0.d0, .1d0] ) > epsilon(0.d0) ) &
  .or. ic%shapes%type .ne. icid%rect &
  .or. any ( ic%shapes%xl .ne. 1 ) &
  .or. any ( ic%shapes%length .ne. [ 64, 128, 0 ] ) &
  .or. ic%shapes%trans%type .ne. icid%linear &
  .or. abs ( ic%shapes%trans%width_px - 0.d0 ) > epsilon(0.d0) &
  .or. ic%shapes%fill%type .ne. icid%uniform &
  .or. any ( abs ( ic%shapes%fill%states(1)%w - [ 1.d0, 0.d0, 0.d0, 0.d0, 1.d0 ] ) > epsilon(0.d0) ) &
  .or. ic%shapes%next%type .ne. icid%circle &
  .or. any ( abs ( ic%shapes%next%x0 - [ 3.d0, 4.d0, 5.d0 ] ) > epsilon(0.d0) ) &
  .or. abs ( ic%shapes%next%r - 2.34d0 ) > epsilon(0.d0) &
  .or. ic%shapes%next%trans%type .ne. icid%cubic &
  .or. abs ( ic%shapes%next%trans%width_px - 3.d0 ) > epsilon(0.d0) &
  .or. ic%shapes%next%fill%type .ne. icid%grad_y &
  .or. any ( abs ( ic%shapes%next%fill%states(1)%w - [ 1.d0, 0.d0, 0.d0, 0.d0, 1.d0 ] ) > epsilon(0.d0) ) &
  .or. any ( abs ( ic%shapes%next%fill%states(2)%w - [ 2.d0, 3.d0, 4.d0, 5.d0, 6.d0 ] ) > epsilon(0.d0) )

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
