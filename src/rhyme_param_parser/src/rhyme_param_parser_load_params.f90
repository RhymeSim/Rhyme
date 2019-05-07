submodule ( rhyme_param_parser ) rhyme_param_parser_load_params_submodule
contains
  module subroutine load_params ( param_file, logger, units, ic, bc, cfl, ig, &
    draw, irs, sl, mh, chombo )
    implicit none

    character (len=1024), intent ( in ) :: param_file
    type ( log_t ), intent ( inout ) :: logger
    type ( rhyme_units_t ), intent ( inout ) :: units
    type ( initial_condition_t ), intent ( inout ) :: ic
    type ( samr_bc_t ), intent ( inout ) :: bc
    type ( cfl_t ), intent ( inout ) :: cfl
    type ( ideal_gas_t ), intent ( inout ) :: ig
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


    call logger%set_section( 'params' )

    call config%init( param_file, logger )

    ! Initial Condition
    call ic_types%add( 'simple', icid%simple )
    call ic_types%add( 'snapshot', icid%snapshot )

    call ic_snapshot_types%add( 'rhyme', icid%rhyme )
    call ic_snapshot_types%add( 'radamesh', icid%radamesh )
    call ic_snapshot_types%add( 'r2c_2d', icid%r2c_2d )

    call config%read( 'ic_type' .at. 1, ic%type, ic_types )
    call config%read( 'ic_snapshot_type' .at. 1, ic%snapshot_type, ic_snapshot_types )
    call config%read( 'ic_snapshot_path' .at. 1, ic%snapshot_path )
    call config%read_array( 'ic_grid' .at. 1, ic%base_grid(1:3) )
    call config%read_array( 'ic_box_lengths' .at. 1, (ic%box_lengths(1:3)%v) )
    call config%read( 'ic_box_lengths' .at. 4, ic%box_length_unit )
    call config%read( 'ic_nlevels' .at. 1, ic%nlevels )
    call config%read_array( 'max_nboxes' .at. 1, ic%max_nboxes( 0:ic%nlevels-1 ) )

    ! Boundary Conditions
    call bc_types%add( 'reflective', bcid%reflective )
    call bc_types%add( 'outflow', bcid%outflow )
    call bc_types%add( 'periodic', bcid%periodic )

    call config%read( 'left_bc' .at. 1, bc%types( bcid%left ), bc_types)
    call config%read( 'right_bc' .at. 1, bc%types( bcid%right ), bc_types)
    call config%read( 'bottom_bc' .at. 1, bc%types( bcid%bottom ), bc_types)
    call config%read( 'top_bc' .at. 1, bc%types( bcid%top ), bc_types)
    call config%read( 'back_bc' .at. 1, bc%types( bcid%back ), bc_types)
    call config%read( 'front_bc' .at. 1, bc%types( bcid%front ), bc_types)

    ! Units
    call config%read( 'density_unit' .at. 1, units%rho_str )
    call config%read( 'length_unit' .at. 1, units%length_str )
    call config%read( 'time_unit' .at. 1, units%time_str )

    ! CFL
    call config%read( 'courant_number' .at. 1,  cfl%courant_number )

    ! Ideal Gas
    call gas_types%add( 'monatomic', igid%monatomic )
    call gas_types%add( 'diatomic', igid%diatomic )
    call gas_types%add( 'polyatomic', igid%polyatomic )

    call config%read( 'ideal_gas_type' .at. 1, ig%type, gas_types )

    ! Drawing
    call canvas_types%add( 'uniform', drid%uniform_canvas )
    call canvas_types%add( 'transparent', drid%transparent_canvas )

    call config%read( 'canvas' .at. 1, draw%type, canvas_types )
    if ( draw%type .eq. drid%uniform_canvas ) then
      call config%read_array( 'canvas' .at. 2 .hint. 'color', draw%canvas%w( hyid%rho:hyid%p ) )
    end if

    ! Shapes
    call shape_types%add( 'cuboid', drid%cuboid )
    call shape_types%add( 'prism', drid%prism )
    call shape_types%add( 'sphere', drid%sphere )
    call shape_types%add( 'smoothed_slab_2d', drid%smoothed_slab_2d )

    n_occur = config%occur( 'shape' )

    call axes%add( 'x', hyid%x )
    call axes%add( 'y', hyid%y )
    call axes%add( 'z', hyid%z )

    call filling_types%add( 'uniform', drid%uniform )

    do i = 1, n_occur
      call config%read( 'shape' .at. 1 .occur. i, shape_type, shape_types )
      shape => draw%new_shape( shape_type )

      select case ( shape_type )
      case ( drid%cuboid )
        call config%read_array( 'shape' .at. 2 .occur. i .hint. 'left_corner', shape%cuboid%left_corner(1:3) )
        call config%read_array( 'shape' .at. 5 .occur. i .hint. 'lengths', shape%cuboid%lengths(1:3) )

        call config%read( 'shape_filling' .at. 1 .occur. i, shape%fill%type, filling_types )
        ! currently only uniform filling is supported
        call config%read_array( 'shape_filling' .at. 2 .occur. i .hint. 'color', shape%fill%colors(1)%w( hyid%rho:hyid%p) )

      case ( drid%prism )
        call config%read_array( 'shape' .at. 2 .occur. i .hint. 'vertex', shape%prism%vertices(1,1:3) )
        call config%read_array( 'shape' .at. 5 .occur. i .hint. 'vertex', shape%prism%vertices(2,1:3) )
        call config%read_array( 'shape' .at. 8 .occur. i .hint. 'vertex', shape%prism%vertices(3,1:3) )
        call config%read( 'shape' .at. 11 .occur. i .hint. 'thickness', shape%prism%thickness )

        call config%read( 'shape_filling' .at. 1 .occur. i, shape%fill%type, filling_types )
        ! currently only uniform filling is supported
        call config%read_array( 'shape_filling' .at. 2 .occur. i .hint. 'color', shape%fill%colors(1)%w( hyid%rho:hyid%p) )

      case ( drid%sphere )
        call config%read_array( 'shape' .at. 2 .occur. i .hint. 'origin', shape%sphere%origin(1:3) )
        call config%read( 'shape' .at. 5 .occur. i .hint. 'radius', shape%sphere%r )

        call config%read( 'shape_filling' .at. 1 .occur. i, shape%fill%type, filling_types )
        ! currently only uniform filling is supported
        call config%read_array( 'shape_filling' .at. 2 .occur. i .hint. 'color', shape%fill%colors(1)%w( hyid%rho:hyid%p) )

      case ( drid%smoothed_slab_2d )
        call config%read( 'shape' .at. 2 .occur. i .hint. 'axis', shape%slab_2d%dir, axes )
        call config%read_array( 'shape' .at. 3 .occur. i .hint. 'positions', shape%slab_2d%pos(1:2) )
        call config%read_array( 'shape' .at. 5 .occur. i .hint. 'sigmas', shape%slab_2d%sigma(1:2) )

        call config%read_array( 'shape_filling' .at. 1 .occur. i .hint. 'color(1)', shape%fill%colors(1)%w( hyid%rho:hyid%p) )
        call config%read_array( 'shape_filling' .at. 6 .occur. i .hint. 'color(2)', shape%fill%colors(2)%w( hyid%rho:hyid%p) )

      end select
    end do

    ! Perturbations
    call perturb_types%add( 'harmonic', drid%harmonic )
    call perturb_types%add( 'symmetric_decaying', drid%symmetric_decaying )

    call coord_types%add( 'cartesian', drid%cartesian )
    call coord_types%add( 'cylindrical', drid%cylindrical )
    call coord_types%add( 'spherical', drid%spherical )

    call perturb_domain_types%add( 'x', drid%x )
    call perturb_domain_types%add( 'y', drid%y )
    call perturb_domain_types%add( 'z', drid%z )
    call perturb_domain_types%add( 'xy', drid%xy )
    call perturb_domain_types%add( 'xz', drid%xz )
    call perturb_domain_types%add( 'yz', drid%yz )
    call perturb_domain_types%add( 'xyz', drid%xyz )
    call perturb_domain_types%add( 'r', drid%r )
    call perturb_domain_types%add( 'theta', drid%theta )
    call perturb_domain_types%add( 'phi', drid%phi )
    call perturb_domain_types%add( 'rtheta', drid%rtheta )
    call perturb_domain_types%add( 'rphi', drid%rphi )
    call perturb_domain_types%add( 'thetaphi', drid%thetaphi )
    call perturb_domain_types%add( 'rthetaphi', drid%rthetaphi )

    n_occur = config%occur( 'perturb' )

    do i = 1, n_occur
      call config%read( 'perturb' .at. 1 .occur. i, perturb_type, perturb_types )
      perturb => draw%new_perturb( perturb_type )

      call config%read( 'perturb' .at. 2 .occur. i .hint. 'coordinate', perturb%coor_type, coord_types )
      call config%read( 'perturb' .at. 3 .occur. i .hint. 'domain', perturb%dir, perturb_domain_types )

      select case ( perturb_type )
      case ( drid%harmonic )
        call config%read( 'perturb' .at. 4 .occur. i .hint. 'A', perturb%harmonic%A )
        call config%read( 'perturb' .at. 5 .occur. i .hint. 'lambda', perturb%harmonic%lambda )
        call config%read_array( 'perturb' .at. 6 .occur. i .hint. 'state', perturb%harmonic%base%w( hyid%rho:hyid%p ) )

      case ( drid%symmetric_decaying )
        call config%read( 'perturb' .at. 4 .occur. i .hint. 'A', perturb%sym_decaying%A )
        call config%read( 'perturb' .at. 5 .occur. i .hint. 'position', perturb%sym_decaying%pos )
        call config%read( 'perturb' .at. 6 .occur. i .hint. 'sigma', perturb%sym_decaying%sigma )
        call config%read_array( 'perturb' .at. 7 .occur. i .hint. 'state', perturb%sym_decaying%base%w( hyid%rho:hyid%p ) )
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

    call logger%set_section( '' )
  end subroutine load_params
end submodule rhyme_param_parser_load_params_submodule
