logical function rhyme_param_parser_load_params_test () result ( failed )
  use rhyme_param_parser
  use rhyme_assertion

  implicit none

  type ( assertion_t ) :: tester

  type ( log_t ) :: logger
  type ( rhyme_units_t ) :: units
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

  tester = .describe. "rhyme_param_parser_load_params"

  call load_params( param_file, logger, units, ic, bc, cfl, ig, draw, irs, &
  sl, mh, chombo )

  ! Structured AMR
  call tester%expect( ic%type .toBe. icid%simple )
  call tester%expect( ic%base_grid .toBe. [ 128, 128, 1 ] )
  call tester%expect( (ic%box_lengths%v) .toBe. [ 1.d0, 1.d0, 0.d0 ] )
  call tester%expect( ic%box_length_unit .toBe. 'kpc' )
  call tester%expect( ic%nlevels .toBe. 3 )
  call tester%expect( ic%max_nboxes(0:ic%nlevels-1) .toBe. [ 1, 10, 100 ] )
  call tester%expect( ic%max_nboxes(ic%nlevels:) .toBe. 0 )

  ! Boundary Condition
  call tester%expect( bc%types(bcid%left) .toBe. 1 )
  call tester%expect( bc%types(bcid%right) .toBe. 2 )
  call tester%expect( bc%types(bcid%bottom) .toBe. 3 )
  call tester%expect( bc%types(bcid%top) .toBe. 1 )
  call tester%expect( bc%types(bcid%back) .toBe. 2 )
  call tester%expect( bc%types(bcid%front) .toBe. 3 )

  ! Units
  call tester%expect( units%rho_str .toBe. 'kg / m^3' )
  call tester%expect( units%length_str .toBe. 'm' )
  call tester%expect( units%time_str .toBe. 's' )

  ! CFL
  call tester%expect( cfl%courant_number .toBe. .81d0 )

  ! Ideal Gas
  call tester%expect( ig%type .toBe. igid%diatomic )

  ! Drawing
  call tester%expect( draw%type .toBe. drid%uniform_canvas )
  call tester%expect( draw%canvas%w .toBe. [ .125d0, 0.d0, 0.d0, 0.d0, .1d0 ] )
  call tester%expect( draw%shapes%type .toBe. drid%cuboid )
  call tester%expect( draw%shapes%cuboid%left_corner .toBe. 1 )
  call tester%expect( draw%shapes%cuboid%lengths .toBe. [ 56, 128, 1 ] )
  call tester%expect( draw%shapes%fill%type .toBe. drid%uniform )
  call tester%expect( draw%shapes%fill%colors(1)%w .toBe. [ 1.d0, 0.d0, 0.d0, 0.d0, 1.d0 ] )
  call tester%expect( draw%shapes%next%type .toBe. drid%prism )
  call tester%expect( draw%shapes%next%prism%vertices(1,:) .toBe. [ 56.0, 1.0, 1.0 ] )
  call tester%expect( draw%shapes%next%prism%vertices(2,:) .toBe. [ 56.0, 128.0, 1.0 ] )
  call tester%expect( draw%shapes%next%prism%vertices(3,:) .toBe. [ 72.0, 1.0, 1.0 ] )
  call tester%expect( draw%shapes%next%prism%thickness .toBe. 1.0 )
  call tester%expect( draw%shapes%fill%type .toBe. drid%uniform )
  call tester%expect( draw%shapes%fill%colors(1)%w .toBe. [ 1.d0, 0.d0, 0.d0, 0.d0, 1.d0 ] )
  call tester%expect( draw%shapes%next%next%type .toBe. drid%sphere )
  call tester%expect( draw%shapes%next%next%sphere%origin .toBe. [ 3.d0, 4.d0, 5.d0 ] )
  call tester%expect( draw%shapes%next%next%sphere%r .toBe. 2.34d0 )
  call tester%expect( draw%shapes%next%next%fill%type .toBe. drid%uniform )
  call tester%expect( draw%shapes%next%next%fill%colors(1)%w .toBe. [ 1.d0, 0.d0, 0.d0, 0.d0, 1.d0 ] )
  call tester%expect( draw%shapes%next%next%next%type .toBe. drid%smoothed_slab_2d )
  call tester%expect( draw%shapes%next%next%next%slab_2d%dir .toBe. drid%x )
  call tester%expect( draw%shapes%next%next%next%slab_2d%pos .toBe. [ 56.0, 72.0 ] )
  call tester%expect( draw%shapes%next%next%next%slab_2d%sigma .toBe. [ .2d0, .4d0 ] )
  call tester%expect( draw%shapes%next%next%next%fill%colors(1)%w .toBe. [ .125d0, 0.d0, 0.d0, 0.d0, .1d0 ] )
  call tester%expect( draw%shapes%next%next%next%fill%colors(2)%w .toBe. [ 1.d0, 0.d0, 0.d0, 0.d0, 1.d0 ] )

  ! Perturbation
  call tester%expect( draw%perturbs%type .toBe. drid%harmonic .hint. 'harmonic' )
  call tester%expect( draw%perturbs%coor_type .toBe. drid%cartesian )
  call tester%expect( draw%perturbs%dir .toBe. drid%x )
  call tester%expect( draw%perturbs%harmonic%A .toBe. .05d0 )
  call tester%expect( draw%perturbs%harmonic%lambda .toBe. 32.0 )
  call tester%expect( draw%perturbs%harmonic%base%w .toBe. [ 0.d0, 0.d0, 0.d0, 0.d0, 1.d0 ] )
  call tester%expect( draw%perturbs%next%type .toBe. drid%symmetric_decaying )
  call tester%expect( draw%perturbs%next%coor_type .toBe. drid%cartesian )
  call tester%expect( draw%perturbs%next%dir .toBe. drid%y )
  call tester%expect( draw%perturbs%next%sym_decaying%A .toBe. 1.d0 )
  call tester%expect( draw%perturbs%next%sym_decaying%pos .toBe. 56.0 )
  call tester%expect( draw%perturbs%next%sym_decaying%sigma .toBe. 2.0 )
  call tester%expect( draw%perturbs%next%sym_decaying%base%w .toBe. [ 0.d0, 0.d0, 0.d0, 0.d0, 1.d0 ] )
  call tester%expect( draw%perturbs%next%next%type .toBe. drid%symmetric_decaying )
  call tester%expect( draw%perturbs%next%next%coor_type .toBe. drid%cartesian )
  call tester%expect( draw%perturbs%next%next%dir .toBe. drid%y )
  call tester%expect( draw%perturbs%next%next%sym_decaying%A .toBe. 1.d0 )
  call tester%expect( draw%perturbs%next%next%sym_decaying%pos .toBe. 72.0 )
  call tester%expect( draw%perturbs%next%next%sym_decaying%sigma .toBe. 8.0 )
  call tester%expect( draw%perturbs%next%next%sym_decaying%base%w .toBe. [ 0.d0, 0.d0, 0.d0, 0.d0, 1.d0 ] )

  ! Iterative Riemann Solver
  call tester%expect( irs%pressure_floor .toBe. 1.d-10 )
  call tester%expect( irs%tolerance .toBe. 1.d-6 )
  call tester%expect( irs%n_iteration .toBe. 100 )

  ! Slope Limiter
  call tester%expect( sl%w .toBe. 0.d0 )
  call tester%expect( sl%type .toBe. slid%van_Leer )

  ! MUSCL-Hancock solver
  call tester%expect( mh%solver_type .toBe. mhid%cpu_intensive )

  ! Chombo
  call tester%expect( trim(chombo%prefix) .toBe. "./prefix" )
  call tester%expect( trim(chombo%nickname) .toBe. "hydro-simulation" )

  failed = tester%failed()
end function rhyme_param_parser_load_params_test
