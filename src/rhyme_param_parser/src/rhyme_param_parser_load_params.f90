submodule ( rhyme_param_parser ) rhyme_param_parser_load_params_submodule
contains
  module subroutine load_params ( param_file, logger, physics, ic, bc, cfl, &
    thermo, draw, irs, sl, mh, chombo )
    implicit none

    character (len=1024), intent ( in ) :: param_file
    type ( log_t ), intent ( inout ) :: logger
    type ( physics_t ), intent ( inout ) :: physics
    type ( initial_condition_t ), intent ( inout ) :: ic
    type ( samr_bc_t ), intent ( inout ) :: bc
    type ( cfl_t ), intent ( inout ) :: cfl
    type ( thermo_base_t ), intent ( inout ) :: thermo
    type ( drawing_t ), intent ( inout ) :: draw
    type ( irs_t ), intent ( inout ) :: irs
    type ( slope_limiter_t ), intent ( inout ) :: sl
    type ( muscl_hancock_t ), intent ( inout ) :: mh
    type ( chombo_t ), intent ( inout ) :: chombo

    type ( config_t ) :: config
    type ( config_switch_t ) :: ic_types, ic_snapshot_types
    type ( config_switch_t ) :: bc_types
    type ( config_switch_t ) :: gas_types
    type ( config_switch_t ) :: canvas_types, shape_types, filling_types
    type ( config_switch_t ) :: perturb_types, coord_types, perturb_domain_types
    type ( config_switch_t ) :: limiter_types
    type ( config_switch_t ) :: solver_types
    type ( config_switch_t ) :: axes

    type ( shape_t ), pointer :: shape
    type ( perturbation_t ), pointer :: perturb

    integer :: shape_type, perturb_type, n_occur, i


    call logger%begin_section( 'params' )

    call config%init( param_file, logger )

    ! Initial Condition
    call ic_types%add( 'simple', icid%simple )
    call ic_types%add( 'snapshot', icid%snapshot )

    call ic_snapshot_types%add( 'rhyme', icid%rhyme )
    call ic_snapshot_types%add( 'radamesh', icid%radamesh )

    call config%read( 'ic_type' .at. 1, ic%type, ic_types )
    call config%read( 'ic_snapshot_type' .at. 1, ic%snapshot_type, ic_snapshot_types )
    call config%read( 'ic_snapshot_path' .at. 1, ic%snapshot_path )
    call config%read_array( 'ic_grid' .at. 1, ic%base_grid(1:NDIM) )
    call config%read( 'ic_box_lengths' .at. 1, ic%box_lengths(1)%v )
#if NDIM > 1
    call config%read( 'ic_box_lengths' .at. 2, ic%box_lengths(2)%v )
#endif
#if NDIM > 2
    call config%read( 'ic_box_lengths' .at. 3, ic%box_lengths(3)%v )
#endif
    call config%read( 'ic_box_lengths' .at. 1+NDIM, ic%box_length_unit )
    call config%read( 'ic_nlevels' .at. 1, ic%nlevels )
    call config%read_array( 'max_nboxes' .at. 1, ic%max_nboxes( 0:ic%nlevels-1 ) )

    ! Boundary Conditions
    call bc_types%add( 'reflective', bcid%reflective )
    call bc_types%add( 'outflow', bcid%outflow )
    call bc_types%add( 'periodic', bcid%periodic )

    call config%read( 'left_bc' .at. 1, bc%types( bcid%left ), bc_types)
    call config%read( 'right_bc' .at. 1, bc%types( bcid%right ), bc_types)
#if NDIM > 1
    call config%read( 'bottom_bc' .at. 1, bc%types( bcid%bottom ), bc_types)
    call config%read( 'top_bc' .at. 1, bc%types( bcid%top ), bc_types)
#endif
#if NDIM > 2
    call config%read( 'back_bc' .at. 1, bc%types( bcid%back ), bc_types)
    call config%read( 'front_bc' .at. 1, bc%types( bcid%front ), bc_types)
#endif

    ! Physics
    call config%read( 'density_unit' .at. 1, physics%rho_str )
    call config%read( 'length_unit' .at. 1, physics%length_str )
    call config%read( 'time_unit' .at. 1, physics%time_str )

    ! CFL
    call config%read( 'courant_number' .at. 1,  cfl%courant_number )

    ! Ideal Gas
    call gas_types%add( 'monatomic', thid%monatomic )
    call gas_types%add( 'diatomic', thid%diatomic )
    call gas_types%add( 'polyatomic', thid%polyatomic )

    call config%read( 'ideal_gas_type' .at. 1, thermo%state_of_matter, gas_types )

    ! Drawing
    call canvas_types%add( 'uniform', drid%uniform_canvas )
    call canvas_types%add( 'transparent', drid%transparent_canvas )

    call config%read( 'canvas' .at. 1, draw%type, canvas_types )
    if ( draw%type .eq. drid%uniform_canvas ) then
      call config%read_array( 'canvas' .at. 2 .hint. 'color', draw%canvas( cid%rho:cid%p ) )
    end if

    ! Shapes
    call shape_types%add( 'cuboid', drid%cuboid )
#if NDIM > 1
    call shape_types%add( 'prism', drid%prism )
    call shape_types%add( 'smoothed_slab_2d', drid%smoothed_slab_2d )
#endif
    call shape_types%add( 'sphere', drid%sphere )

    n_occur = config%occur( 'shape' )

    call axes%add( 'x', samrid%x )
#if NDIM > 1
    call axes%add( 'y', samrid%y )
#endif
#if NDIM > 2
    call axes%add( 'z', samrid%z )
#endif

    call filling_types%add( 'uniform', drid%uniform )

    do i = 1, n_occur
      call config%read( 'shape' .at. 1 .occur. i, shape_type, shape_types )
      shape => draw%new_shape( shape_type )

      select case ( shape_type )
      case ( drid%cuboid )
        call config%read_array( 'shape' .at. 2 .occur. i .hint. 'left_corner', shape%cuboid%left_corner(1:NDIM) )
        call config%read_array( 'shape' .at. 2+NDIM .occur. i .hint. 'lengths', shape%cuboid%lengths(1:NDIM) )

        call config%read( 'shape_filling' .at. 1 .occur. i, shape%fill%type, filling_types )
        ! currently only uniform filling is supported
        call config%read_array( 'shape_filling' .at. 2 .occur. i .hint. 'color', shape%fill%colors( cid%rho:cid%p, 1 ) )

#if NDIM > 1
      case ( drid%prism )
        call config%read_array( 'shape' .at. 2 .occur. i .hint. 'vertex', shape%prism%vertices(1:NDIM, 1) )
        call config%read_array( 'shape' .at. 2+1*NDIM .occur. i .hint. 'vertex', shape%prism%vertices(1:NDIM, 2) )
        call config%read_array( 'shape' .at. 2+2*NDIM .occur. i .hint. 'vertex', shape%prism%vertices(1:NDIM, 3) )
#if NDIM > 2
        call config%read( 'shape' .at. 2+3*NDIM .occur. i .hint. 'thickness', shape%prism%thickness )
#endif
#endif

        call config%read( 'shape_filling' .at. 1 .occur. i, shape%fill%type, filling_types )
        ! currently only uniform filling is supported
        call config%read_array( 'shape_filling' .at. 2 .occur. i .hint. 'color', shape%fill%colors( cid%rho:cid%p, 1 ) )

      case ( drid%sphere )
        call config%read_array( 'shape' .at. 2 .occur. i .hint. 'origin', shape%sphere%origin(1:NDIM) )
        call config%read( 'shape' .at. 2+NDIM .occur. i .hint. 'radius', shape%sphere%r )

        call config%read( 'shape_filling' .at. 1 .occur. i, shape%fill%type, filling_types )
        ! currently only uniform filling is supported
        call config%read_array( 'shape_filling' .at. 2 .occur. i .hint. 'color', shape%fill%colors( cid%rho:cid%p, 1 ) )

#if NDIM > 1
      case ( drid%smoothed_slab_2d )
        call config%read( 'shape' .at. 2 .occur. i .hint. 'axis', shape%slab_2d%axis, axes )
        call config%read_array( 'shape' .at. 3 .occur. i .hint. 'positions', shape%slab_2d%pos(1:2) )
        call config%read_array( 'shape' .at. 5 .occur. i .hint. 'sigmas', shape%slab_2d%sigma(1:2) )

        call config%read_array( 'shape_filling' .at. 1 .occur. i .hint. 'color(1)', shape%fill%colors( cid%rho:cid%p, 1 ) )
        call config%read_array( 'shape_filling' .at. 1+1+NDIM+1 .occur. i .hint. 'color(2)', shape%fill%colors( cid%rho:cid%p, 2 ) )
#endif

      end select
    end do

    ! Perturbations
    call perturb_types%add( 'harmonic', drid%harmonic )
#if NDIM > 1
    call perturb_types%add( 'symmetric_decaying', drid%symmetric_decaying )
#endif

    call coord_types%add( 'cartesian', drid%cartesian )

    call perturb_domain_types%add( 'x', drid%x )
#if NDIM > 1
    call perturb_domain_types%add( 'y', drid%y )
#endif
#if NDIM > 2
    call perturb_domain_types%add( 'z', drid%z )
#endif

    n_occur = config%occur( 'perturb' )

    do i = 1, n_occur
      call config%read( 'perturb' .at. 1 .occur. i, perturb_type, perturb_types )
      perturb => draw%new_perturb( perturb_type )

      call config%read( 'perturb' .at. 2 .occur. i .hint. 'coordinate', perturb%coor_type, coord_types )
      call config%read( 'perturb' .at. 3 .occur. i .hint. 'domain', perturb%axis, perturb_domain_types )

      select case ( perturb_type )
      case ( drid%harmonic )
        call config%read( 'perturb' .at. 4 .occur. i .hint. 'A', perturb%harmonic%A )
        call config%read( 'perturb' .at. 5 .occur. i .hint. 'lambda', perturb%harmonic%lambda )
        call config%read_array( 'perturb' .at. 6 .occur. i .hint. 'state', perturb%harmonic%base( cid%rho:cid%p ) )

#if NDIM > 1
      case ( drid%symmetric_decaying )
        call config%read( 'perturb' .at. 4 .occur. i .hint. 'A', perturb%sym_decaying%A )
        call config%read( 'perturb' .at. 5 .occur. i .hint. 'position', perturb%sym_decaying%pos )
        call config%read( 'perturb' .at. 6 .occur. i .hint. 'sigma', perturb%sym_decaying%sigma )
        call config%read_array( 'perturb' .at. 7 .occur. i .hint. 'state', perturb%sym_decaying%base( cid%rho:cid%p ) )
#endif
      end select
    end do

    ! Iterative Riemann Solver
    call config%read( 'pressure_floor' .at. 1, irs%pressure_floor )
    call config%read( 'tolerance' .at. 1, irs%tolerance )
    call config%read( 'n_iteration' .at. 1, irs%n_iteration )

    ! Slope limiter
    call limiter_types%add( 'van_leer', slid%van_Leer )
    call limiter_types%add( 'minmod', slid%minmod )
    call limiter_types%add( 'van_albada', slid%van_albada )
    call limiter_types%add( 'superbee', slid%superbee )

    call config%read( 'slope_limiter' .at. 1, sl%type, limiter_types )
    call config%read( 'slope_limiter_omega' .at. 1, sl%w )

    ! MUSCL-Hancock solver
    call solver_types%add( 'memory_intensive', mhid%memory_intensive )
    call solver_types%add( 'cpu_intensive', mhid%cpu_intensive )

    call config%read( 'solver_type' .at. 1, mh%solver_type, solver_types )

    ! Chombo
    call config%read( 'prefix' .at. 1, chombo%prefix )
    call config%read( 'nickname' .at. 1, chombo%nickname )

    call logger%end_section
  end subroutine load_params
end submodule rhyme_param_parser_load_params_submodule
