logical function rhyme_param_parser_parse_param_test () result ( failed )
  use rhyme_param_parser

  implicit none

  type ( log_t ) :: log
  type ( initial_condition_t ) :: ic
  type ( samr_bc_t ) :: bc
  type ( cfl_t ) :: cfl
  type ( ideal_gas_t ) :: ig
  type ( drawing_t ) :: draw
  type ( irs_t ) :: irs
  type ( slope_limiter_t ) :: sl
  type ( muscl_hancock_t ) :: mh
  type ( chombo_t ) :: chombo

  character(len=1024), parameter :: param_file = "parameters.conf.example"


  call parse_params ( param_file, log, ic, bc, cfl, ig, draw, irs, sl, mh, chombo )

  ! Structured AMR
  failed = &
  ic%type .ne. icid%simple &
  .or. any ( ic%base_grid .ne. [ 128, 128, 1 ] ) &
  .or. ic%nlevels .ne. 3 &
  .or. any ( ic%max_nboxes(0:ic%nlevels-1) .ne. [ 1, 10, 100 ] ) &
  .or. any ( ic%max_nboxes(ic%nlevels:) .ne. 0 )
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
  failed = ig%type .ne. igid%diatomic
  if ( failed ) return

  ! Drawing
  failed = &
  draw%type .ne. drid%uniform_canvas &
  .or. any( abs( draw%canvas%w - [ .125d0, 0.d0, 0.d0, 0.d0, .1d0 ] ) > epsilon(0.d0) ) &
  .or. draw%shapes%type .ne. drid%cuboid &
  .or. any( draw%shapes%cuboid%left_corner .ne. 1 ) &
  .or. any( draw%shapes%cuboid%lengths .ne. [ 56, 128, 1 ] ) &
  .or. draw%shapes%trans%type( samrid%top ) .ne. drid%ramp &
  .or. abs( draw%shapes%trans%sigma( samrid%top ) - 2.5d0 ) > epsilon(0.d0) &
  .or. any( abs( draw%shapes%trans%colors( samrid%top, 1 )%w - [ .125d0, 0.d0, 0.d0, 0.d0, .1d0 ] ) > epsilon(0.d0) ) &
  .or. any( abs( draw%shapes%trans%colors( samrid%top, 2 )%w - [ 1.d0, 0.d0, 0.d0, 0.d0, 1.d0 ] ) > epsilon(0.d0) ) &
  .or. draw%shapes%trans%type( samrid%bottom ) .ne. drid%linear &
  .or. abs( draw%shapes%trans%sigma( samrid%bottom ) - 1.5d0 ) > epsilon(0.d0) &
  .or. any( abs( draw%shapes%trans%colors( samrid%bottom, 1 )%w - [ .125d0, 0.d0, 0.d0, 0.d0, .1d0 ] ) > epsilon(0.d0) ) &
  .or. any( abs( draw%shapes%trans%colors( samrid%bottom, 2 )%w - [ 1.d0, 0.d0, 0.d0, 0.d0, 1.d0 ] ) > epsilon(0.d0) ) &
  .or. draw%shapes%fill%type .ne. drid%uniform &
  .or. any ( abs ( draw%shapes%fill%colors(1)%w - [ 1.d0, 0.d0, 0.d0, 0.d0, 1.d0 ] ) > epsilon(0.d0) ) &
  .or. draw%shapes%next%type .ne. drid%prism &
  .or. any ( abs ( draw%shapes%next%prism%vertices(1,:) - [ 56, 1, 1 ] ) > epsilon(0.d0) ) &
  .or. any ( abs ( draw%shapes%next%prism%vertices(2,:) - [ 56, 128, 1 ] ) > epsilon(0.d0) ) &
  .or. any ( abs ( draw%shapes%next%prism%vertices(3,:) - [ 72, 1, 1 ] ) > epsilon(0.d0) ) &
  .or. abs ( draw%shapes%next%prism%thickness - 1 ) > epsilon(0.d0) &
  .or. draw%shapes%fill%type .ne. drid%uniform &
  .or. any ( abs ( draw%shapes%fill%colors(1)%w - [ 1.d0, 0.d0, 0.d0, 0.d0, 1.d0 ] ) > epsilon(0.d0) ) &
  .or. draw%shapes%next%next%type .ne. drid%sphere &
  .or. any ( abs ( draw%shapes%next%next%sphere%origin - [ 3.d0, 4.d0, 5.d0 ] ) > epsilon(0.d0) ) &
  .or. abs ( draw%shapes%next%next%sphere%r - 2.34d0 ) > epsilon(0.d0) &
  .or. draw%shapes%next%next%fill%type .ne. drid%uniform &
  .or. any ( abs ( draw%shapes%next%next%fill%colors(1)%w - [ 1.d0, 0.d0, 0.d0, 0.d0, 1.d0 ] ) > epsilon(0.d0) ) &
  .or. draw%shapes%next%next%next%type .ne. drid%smoothed_slab_2d &
  .or. draw%shapes%next%next%next%slab_2d%dir .ne. drid%x &
  .or. any( abs( draw%shapes%next%next%next%slab_2d%pos - [ 56, 72 ] ) > epsilon(0.d0) ) &
  .or. any( abs( draw%shapes%next%next%next%slab_2d%sigma - [ .2d0, .4d0 ] ) > epsilon(0.d0) ) &
  .or. any( abs( draw%shapes%next%next%next%fill%colors(1)%w - [ .125d0, 0.d0, 0.d0, 0.d0, .1d0 ] ) > epsilon(0.d0) ) &
  .or. any( abs( draw%shapes%next%next%next%fill%colors(2)%w - [ 1.d0, 0.d0, 0.d0, 0.d0, 1.d0 ] ) > epsilon(0.d0) )
  if ( failed ) return

  ! Perturbation
  failed = &
  draw%perturbs%type .ne. drid%harmonic &
  .or. draw%perturbs%coor_type .ne. drid%cartesian &
  .or. draw%perturbs%dir .ne. drid%x &
  .or. abs( draw%perturbs%harmonic%A - .05d0 ) > epsilon(0.d0) &
  .or. abs( draw%perturbs%harmonic%lambda - 32 ) > epsilon(0.d0) &
  .or. any( abs( draw%perturbs%harmonic%base%w - [ 0.d0, 0.d0, 0.d0, 0.d0, 1.d0 ] ) > epsilon(0.d0) ) &
  .or. draw%perturbs%next%type .ne. drid%symmetric_decaying &
  .or. draw%perturbs%next%coor_type .ne. drid%cartesian &
  .or. draw%perturbs%next%dir .ne. drid%y &
  .or. abs( draw%perturbs%next%sym_decaying%A - 1.d0 ) > epsilon(0.d0) &
  .or. abs( draw%perturbs%next%sym_decaying%pos - 56 ) > epsilon(0.d0) &
  .or. abs( draw%perturbs%next%sym_decaying%sigma - 2 ) > epsilon(0.d0) &
  .or. any( abs( draw%perturbs%next%sym_decaying%base%w - [ 0.d0, 0.d0, 0.d0, 0.d0, 1.d0 ] ) > epsilon(0.d0) ) &
  .or. draw%perturbs%next%next%type .ne. drid%symmetric_decaying &
  .or. draw%perturbs%next%next%coor_type .ne. drid%cartesian &
  .or. draw%perturbs%next%next%dir .ne. drid%y &
  .or. abs( draw%perturbs%next%next%sym_decaying%A - 1.d0 ) > epsilon(0.d0) &
  .or. abs( draw%perturbs%next%next%sym_decaying%pos - 72 ) > epsilon(0.d0) &
  .or. abs( draw%perturbs%next%next%sym_decaying%sigma - 8 ) > epsilon(0.d0) &
  .or. any( abs( draw%perturbs%next%next%sym_decaying%base%w - [ 0.d0, 0.d0, 0.d0, 0.d0, 1.d0 ] ) > epsilon(0.d0) )
  if ( failed ) return

  ! Iterative Riemann Solver
  failed = &
  abs ( irs%pressure_floor - 1.d-10 ) > epsilon(0.d0) &
  .or. abs ( irs%tolerance - 1.d-6 ) > epsilon(0.d0) &
  .or. irs%n_iteration .ne. 100
  if ( failed ) return

  ! Slope Limiter
  failed = &
  abs ( sl%w - 0.d0 ) > epsilon(0.d0) &
  .or. sl%type .ne. slid%van_Leer
  if ( failed ) return

  ! MUSCL-Hancock solver
  failed = mh%solver_type .ne. mhid%cpu_intensive
  if ( failed ) return

  ! CFL
  failed = abs ( cfl%courant_number - .81d0 ) > epsilon(0.d0)
  if ( failed ) return

  ! Chombo
  failed = &
  trim(chombo%prefix) .ne. "./prefix" &
  .or. trim(chombo%nickname) .ne. "hydro-simulation"
  if ( failed ) return

end function rhyme_param_parser_parse_param_test
