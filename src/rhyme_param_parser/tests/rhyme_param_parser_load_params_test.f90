logical function rhyme_param_parser_load_params_test () result ( failed )
  use rhyme_param_parser
  use rhyme_assertion

  implicit none

  type ( assertion_t ) :: tester

#if NDIM == 1
#define PARAM_FILE_NAME "parameters_1d.conf.example"
#define CUBOID_LENGTH_ARRAY [ 56 ]
#define SPHERE_ORIGIN_ARRAY [ 3.d0 ]
#elif NDIM == 2
#define PARAM_FILE_NAME "parameters_2d.conf.example"
#define CUBOID_LENGTH_ARRAY [ 56, 128 ]
#define SPHERE_ORIGIN_ARRAY [ 3.d0, 4.d0 ]
#define PRISM_VERTEX_1 [ 56.d0, 1.d0 ]
#define PRISM_VERTEX_2 [ 56.d0, 128.d0 ]
#define PRISM_VERTEX_3 [ 72.d0, 1.d0 ]
#elif NDIM == 3
#define PARAM_FILE_NAME "parameters_3d.conf.example"
#define CUBOID_LENGTH_ARRAY [ 56, 128, 1 ]
#define SPHERE_ORIGIN_ARRAY [ 3.d0, 4.d0, 5.d0 ]
#define PRISM_VERTEX_1 [ 56.d0, 1.d0, 1.d0 ]
#define PRISM_VERTEX_2 [ 56.d0, 128.d0, 1.d0 ]
#define PRISM_VERTEX_3 [ 72.d0, 1.d0, 1.d0 ]
#endif

  type ( log_t ) :: logger
  type ( physics_t ) :: physics
  type ( initial_condition_t ) :: ic
  type ( samr_bc_t ) :: bc
  type ( cfl_t ) :: cfl
  type ( thermo_base_t ) :: thermo
  type ( drawing_t ) :: draw
  type ( irs_t ) :: irs
  type ( slope_limiter_t ) :: sl
  type ( muscl_hancock_t ) :: mh
  type ( chombo_t ) :: chombo

  character(len=1024), parameter :: param_file = PARAM_FILE_NAME

  tester = .describe. "rhyme_param_parser_load_params"

  call load_params( param_file, logger, physics, ic, bc, cfl, thermo, draw, irs, &
  sl, mh, chombo )

  ! Structured AMR
  call tester%expect( ic%type .toBe. icid%simple )
  call tester%expect( ic%base_grid .toBe. 128 )
  call tester%expect( (ic%box_lengths%v) .toBe. 1.d0 )
  call tester%expect( ic%box_length_unit .toBe. 'kpc' )
  call tester%expect( ic%nlevels .toBe. 3 )
  call tester%expect( ic%max_nboxes(0:ic%nlevels-1) .toBe. [ 1, 10, 100 ] )
  call tester%expect( ic%max_nboxes(ic%nlevels:) .toBe. 0 )

  ! Boundary Condition
  call tester%expect( bc%types(bcid%left) .toBe. 1 )
  call tester%expect( bc%types(bcid%right) .toBe. 2 )
#if NDIM > 1
  call tester%expect( bc%types(bcid%bottom) .toBe. 3 )
  call tester%expect( bc%types(bcid%top) .toBe. 1 )
#endif
#if NDIM > 2
  call tester%expect( bc%types(bcid%back) .toBe. 2 )
  call tester%expect( bc%types(bcid%front) .toBe. 3 )
#endif

  ! Physics
  call tester%expect( physics%rho_str .toBe. 'kg / m^3' )
  call tester%expect( physics%length_str .toBe. 'm' )
  call tester%expect( physics%time_str .toBe. 's' )

  ! CFL
  call tester%expect( cfl%courant_number .toBe. .81d0 )

  ! Ideal Gas
  call tester%expect( thermo%state_of_matter .toBe. thid%diatomic )

  ! Drawing
  call tester%expect( draw%type .toBe. drid%uniform_canvas )
  call tester%expect( draw%canvas( cid%rho ) .toBe. .125d0 )
  call tester%expect( draw%canvas( cid%u:cid%u+NDIM-1 ) .toBe. 0.d0 )
  call tester%expect( draw%canvas( cid%p ) .toBe. .1d0 )

  call tester%expect( draw%shapes%type .toBe. drid%cuboid )
  call tester%expect( draw%shapes%cuboid%left_corner .toBe. 1 )
  call tester%expect( draw%shapes%cuboid%lengths .toBe. CUBOID_LENGTH_ARRAY .hint. 'cuboid length array' )
  call tester%expect( draw%shapes%fill%type .toBe. drid%uniform )
  call tester%expect( draw%shapes%fill%colors( cid%rho:cid%rho, 1 ) .toBe. 1.d0 )
  call tester%expect( draw%shapes%fill%colors( cid%u:cid%u+NDIM-1, 1 ) .toBe. 0.d0 )
  call tester%expect( draw%shapes%fill%colors( cid%p, 1 ) .toBe. 1.d0 )

  call tester%expect( draw%shapes%next%type .toBe. drid%sphere )
  call tester%expect( draw%shapes%next%sphere%origin .toBe. SPHERE_ORIGIN_ARRAY )
  call tester%expect( draw%shapes%next%sphere%r .toBe. 2.34d0 )
  call tester%expect( draw%shapes%next%fill%type .toBe. drid%uniform )
  call tester%expect( draw%shapes%next%fill%colors( cid%rho, 1 ) .toBe. 1.d0 )
  call tester%expect( draw%shapes%next%fill%colors( cid%u:cid%u+NDIM-1, 1 ) .toBe. 0.d0 )
  call tester%expect( draw%shapes%next%fill%colors( cid%p, 1 ) .toBe. 1.d0 )

#if NDIM > 1
  call tester%expect( draw%shapes%next%next%type .toBe. drid%prism )
  call tester%expect( draw%shapes%next%next%prism%vertices(:, 1) .toBe. PRISM_VERTEX_1 )
  call tester%expect( draw%shapes%next%next%prism%vertices(:, 2) .toBe. PRISM_VERTEX_2 )
  call tester%expect( draw%shapes%next%next%prism%vertices(:, 3) .toBe. PRISM_VERTEX_3 )
#if NDIM > 2
  call tester%expect( draw%shapes%next%next%prism%thickness .toBe. 1.0 )
#endif
  call tester%expect( draw%shapes%next%fill%type .toBe. drid%uniform )
  call tester%expect( draw%shapes%next%fill%colors( cid%rho, 1 ) .toBe. 1.d0 )
  call tester%expect( draw%shapes%next%fill%colors( cid%u:cid%u+NDIM-1, 1 ) .toBe. 0.d0 )
  call tester%expect( draw%shapes%next%fill%colors( cid%p, 1 ) .toBe. 1.d0 )
  call tester%expect( draw%shapes%next%next%next%type .toBe. drid%smoothed_slab_2d )
  call tester%expect( draw%shapes%next%next%next%slab_2d%axis .toBe. drid%x )
  call tester%expect( draw%shapes%next%next%next%slab_2d%pos .toBe. [ 56.0, 72.0 ] )
  call tester%expect( draw%shapes%next%next%next%slab_2d%sigma .toBe. [ .2d0, .4d0 ] )
  call tester%expect( draw%shapes%next%next%next%fill%colors( cid%rho, 1 ) .toBe. .125d0 )
  call tester%expect( draw%shapes%next%next%next%fill%colors( cid%u:cid%u+NDIM-1, 1 ) .toBe. 0.d0 )
  call tester%expect( draw%shapes%next%next%next%fill%colors( cid%p, 1 ) .toBe. .1d0 )
  call tester%expect( draw%shapes%next%next%next%fill%colors( cid%rho, 2 ) .toBe. 1.d0 )
  call tester%expect( draw%shapes%next%next%next%fill%colors( cid%u:cid%u+NDIM-1, 2 ) .toBe. 0.d0 )
  call tester%expect( draw%shapes%next%next%next%fill%colors( cid%p, 2 ) .toBe. 1.d0 )
#endif

  ! Perturbation
  call tester%expect( draw%perturbs%type .toBe. drid%harmonic .hint. 'harmonic' )
  call tester%expect( draw%perturbs%coor_type .toBe. drid%cartesian )
  call tester%expect( draw%perturbs%axis .toBe. drid%x )
  call tester%expect( draw%perturbs%harmonic%A .toBe. .05d0 )
  call tester%expect( draw%perturbs%harmonic%lambda .toBe. 32.0 )
  call tester%expect( draw%perturbs%harmonic%base( cid%rho ) .toBe. 0.d0 )
  call tester%expect( draw%perturbs%harmonic%base( cid%u:cid%u+NDIM-1 ) .toBe. 0.d0 )
  call tester%expect( draw%perturbs%harmonic%base( cid%p ) .toBe. 1.d0 )

#if NDIM > 1
  call tester%expect( draw%perturbs%next%type .toBe. drid%symmetric_decaying )
  call tester%expect( draw%perturbs%next%coor_type .toBe. drid%cartesian )
  call tester%expect( draw%perturbs%next%axis .toBe. drid%y )
  call tester%expect( draw%perturbs%next%sym_decaying%A .toBe. 1.d0 )
  call tester%expect( draw%perturbs%next%sym_decaying%pos .toBe. 56.0 )
  call tester%expect( draw%perturbs%next%sym_decaying%sigma .toBe. 2.0 )
  call tester%expect( draw%perturbs%next%sym_decaying%base(cid%rho) .toBe. 0.d0 )
  call tester%expect( draw%perturbs%next%sym_decaying%base(cid%u:cid%u+NDIM-1) .toBe. 0.d0 )
  call tester%expect( draw%perturbs%next%sym_decaying%base(cid%p) .toBe. 1.d0 )
  call tester%expect( draw%perturbs%next%next%type .toBe. drid%symmetric_decaying )
  call tester%expect( draw%perturbs%next%next%coor_type .toBe. drid%cartesian )
  call tester%expect( draw%perturbs%next%next%axis .toBe. drid%y )
  call tester%expect( draw%perturbs%next%next%sym_decaying%A .toBe. 1.d0 )
  call tester%expect( draw%perturbs%next%next%sym_decaying%pos .toBe. 72.0 )
  call tester%expect( draw%perturbs%next%next%sym_decaying%sigma .toBe. 8.0 )
  call tester%expect( draw%perturbs%next%next%sym_decaying%base(cid%rho) .toBe. 0.d0 )
  call tester%expect( draw%perturbs%next%next%sym_decaying%base(cid%u:cid%u+NDIM-1) .toBe. 0.d0 )
  call tester%expect( draw%perturbs%next%next%sym_decaying%base(cid%p) .toBe. 1.d0 )
#endif

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
