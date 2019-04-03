module rhyme_param_parser
  ! TODO: replace iteration with fortran namelists

  use rhyme_log
  use rhyme_initial_condition
  use rhyme_samr_bc
  use rhyme_cfl
  use rhyme_ideal_gas
  use rhyme_drawing
  use rhyme_irs
  use rhyme_slope_limiter
  use rhyme_muscl_hancock
  use rhyme_chombo

  implicit none

contains

  subroutine parse_params ( param_file, log, ic, bc, cfl, ig, draw, irs, sl, mh, chombo )
    implicit none

    character (len=1024), intent ( in ) :: param_file
    type ( log_t ), intent ( inout ) :: log
    type ( initial_condition_t ), intent ( inout ) :: ic
    type ( samr_bc_t ), intent ( inout ) :: bc
    type ( cfl_t ), intent ( inout ) :: cfl
    type ( ideal_gas_t ), intent ( inout ) :: ig
    type ( drawing_t ), intent ( inout ) :: draw
    type ( irs_t ), intent ( inout ) :: irs
    type ( slope_limiter_t ), intent ( inout ) :: sl
    type ( muscl_hancock_t ), intent ( inout ) :: mh
    type ( chombo_t ), intent ( inout ) :: chombo

    integer :: ios
    character(len=1024) :: key, op, str, dir_str
    type ( shape_t ), pointer :: shape
    logical :: param_file_was_found


    call log%set_section( 'params' )

    inquire( file=param_file, exist=param_file_was_found )

    if ( param_file_was_found ) then
      open (1, file=param_file, action='read', form="formatted")
    else
      call log%err( 'Parameter file was not found,', 'param_file', ':', [ param_file ] )
      stop
    end if

    do
      read (1, *, iostat=ios) key
      if ( ios .ne. 0 ) exit

      key = adjustl(trim(key))
      if ( key(1:1) .eq. "#" ) cycle

      backspace(1)

      select case ( adjustl(trim(key)) )

        ! Structured AMR
      case ( 'ic_type' )
        read (1, *) key, op, str

        if ( trim(str) .eq. 'simple' ) then
          ic%type = icid%simple
        else if ( trim(str) .eq. 'snapshot' ) then
          ic%type = icid%snapshot
        else
          call log%err( 'Unknown ic_type', 'ic_type', '=', [ str ] )
        end if

        call log%log( '', 'ic_type', '=', [ ic%type ] )

      case ( 'ic_snapshot_type')
        read (1, *) key, op, str

        if ( trim(str) .eq. 'rhyme' ) then
          ic%snapshot_type = icid%rhyme
        else if ( trim(str) .eq. 'r2c_2d' ) then
          ic%snapshot_type = icid%r2c_2d
        else
          call log%err( 'Unknown ic_snapshot_type', 'ic_snapshot_type', '=', [ str ] )
        end if

        call log%log( '', 'ic_snapshot_type', '=', [ str ] )

      case ( 'ic_snapshot_path')
        read (1, *) key, op, ic%snapshot_path
        call log%log( '', 'ic_snapshot_path', '=', [ ic%snapshot_path ] )

      case ( "ic_grid" )
        read (1, *) key, op, ic%base_grid(1:3)
        call log%log( '', 'ic_grid', '=', ic%base_grid )
      case ( "ic_nlevels" )
        read (1, *) key, op, ic%nlevels
        call log%log( '', 'ic_nlevels', '=', [ ic%nlevels ] )
      case ( "max_nboxes" )
        ic%max_nboxes = 0
        read (1, *) key, op, ic%max_nboxes( 0:ic%nlevels - 1 )
        call log%log( '', 'max_nboxes', '=', ic%max_nboxes( 0:samrid%max_nlevels ) )

        ! Boundary Condition
      case ( "left_bc" )
        read (1, *) key, op, str
        bc%types(bcid%left) = select_boundary ( str )
        call log%log( '', 'left_bc', '=', [ str ] )
      case ( "right_bc" )
        read (1, *) key, op, str
        bc%types(bcid%right) = select_boundary ( str )
        call log%log( '', 'right_bc', '=', [ str ] )
      case ( "bottom_bc" )
        read (1, *) key, op, str
        bc%types(bcid%bottom) = select_boundary ( str )
        call log%log( '', 'bottom_bc', '=', [ str ] )
      case ( "top_bc" )
        read (1, *) key, op, str
        bc%types(bcid%top) = select_boundary ( str )
        call log%log( '', 'top_bc', '=', [ str ] )
      case ( "back_bc" )
        read (1, *) key, op, str
        bc%types(bcid%back) = select_boundary ( str )
        call log%log( '', 'back_bc', '=', [ str ] )
      case ( "front_bc" )
        read (1, *) key, op, str
        bc%types(bcid%front) = select_boundary ( str )
        call log%log( '', 'front_bc', '=', [ str ] )

        !CFL
      case ( "courant_number" )
        read (1, *) key, op, cfl%courant_number
        call log%log( '', 'courant_number', '=', [ cfl%courant_number ] )

        ! Ideal Gas
      case ( "ideal_gas_type" );
         read (1, *) key, op, str
         call log%log( '', 'ideal_gas_type', '=', [ str ] )

        if ( trim(str) .eq. "monatomic" ) then
          ig%type = igid%monatomic
        else if ( trim(str) .eq. "diatomic" ) then
          ig%type = igid%diatomic
        else if ( trim(str) .eq. "polyatomic" ) then
          ig%type = igid%polyatomic
        else
          call log%err( 'Unknown ideal gas type', 'ideal_gas_type', '=', [ str ] )
        end if

        ! drawing
      case ( "canvas" )
        read (1, *) key, op, str
        call log%log( '', 'canvas (type)', '=', [ str ] )
        backspace(1)

        if ( trim(str) .eq. 'uniform' ) then
          read (1, *) key, op, str, draw%canvas%w(hyid%rho:hyid%p)
          draw%type = drid%uniform_canvas
          call log%log( '', 'canvas (uniform)', '=', draw%canvas%w )
        else if ( trim(str) .eq. 'transparent' ) then
          read (1, *) key, op, str
          draw%type = drid%transparent_canvas
        else
          call log%err( 'Unsuported canvas', 'canvas_type', '=', [ str ] )
        end if

      case ( "shape" ); call read_shape()

      case ( "shape_transition_left" ); call read_shape_transition( 'left', samrid%left )
      case ( "shape_transition_right" ); call read_shape_transition( 'right', samrid%right )
      case ( "shape_transition_bottom" ); call read_shape_transition( 'bottom', samrid%bottom )
      case ( "shape_transition_top" ); call read_shape_transition( 'top', samrid%top )
      case ( "shape_transition_back" ); call read_shape_transition( 'back', samrid%back )
      case ( "shape_transition_front" ); call read_shape_transition( 'front', samrid%front )

      case ( 'shape_transition_left_colors' ); call read_shape_transition_colors( 'left', samrid%left )
      case ( 'shape_transition_right_colors' ); call read_shape_transition_colors( 'right', samrid%right )
      case ( 'shape_transition_bottom_colors' ); call read_shape_transition_colors( 'bottom', samrid%bottom )
      case ( 'shape_transition_top_colors' ); call read_shape_transition_colors( 'top', samrid%top )
      case ( 'shape_transition_back_colors' ); call read_shape_transition_colors( 'back', samrid%back )
      case ( 'shape_transition_front_colors' ); call read_shape_transition_colors( 'front', samrid%front )

      case ( "shape_filling" ); call read_shape_filling()
      case ( 'shape_filling_colors' ); call read_shape_filling_colors()

        ! Iterative Riemann Solver
      case ( "pressure_floor" )
        read (1, *) key, op, irs%pressure_floor
        call log%log( '', 'pressure_floor', '=', [ irs%pressure_floor ] )
      case ( "tolerance" )
        read (1, *) key, op, irs%tolerance
        call log%log( '', 'tolerance', '=', [ irs%tolerance ] )
      case ( "n_iteration" )
        read (1, *) key, op, irs%n_iteration
        call log%log( '', 'n_iteration', '=', [ irs%n_iteration ] )

        ! Slope limiter
      case ( "limiter" )
        read (1, *) key, op, str
        call log%log( '', 'slope_limiter', '=', [ str ] )

        if ( trim(str) .eq. "van_leer") then
          sl%type = slid%van_Leer
        else if ( trim(str) .eq. "minmod") then
          sl%type = slid%minmod
        else if ( trim(str) .eq. "van_albada") then
          sl%type = slid%van_albada
        else if ( trim(str) .eq. "superbee") then
          sl%type = slid%superbee
        end if

      case ( "limiter_omega" )
        read (1, *) key, op, sl%w
        call log%log( '', 'slope_limiter_omega (w)', '=', [ sl%w ] )

        ! MUSCL-Hancock solver
      case ( 'solver_type' )
        read (1, *) key, op, str
        call log%log( '', 'solver_type', '=', [ str ] )

        if ( trim(str) .eq. 'memory_intensive' ) then
          mh%solver_type = mhid%memory_intensive
        else if ( trim(str) .eq. 'cpu_intensive' ) then
          mh%solver_type = mhid%cpu_intensive
        else
          call log%err( 'Unknown solver_type' )
        end if

        ! Chombo
      case ( "prefix" )
        read (1, *) key, op, chombo%prefix
        call log%log( '', 'prefix', '=', [ chombo%prefix ] )
      case ( "nickname" )
        read (1, *) key, op, chombo%nickname
        call log%log( '', 'nickname', '=', [ chombo%nickname ] )


        ! Unknown option
      case default
        read (1, *) key, op, str
        call log%warn( 'Unknown option', key, '=', [ str ] )
      end select
    end do

    close (1)

  contains

    integer function select_boundary ( bundry ) result ( bc )
      implicit none

      character(len=1024) :: bundry

      select case ( trim(bundry) )
      case ( "reflective" ); bc = bcid%reflective
      case ( "outflow" ); bc = bcid%outflow
      case ( "periodic" ); bc = bcid%periodic
      end select
    end function select_boundary

    subroutine read_shape ()
      implicit none

      read (1, *) key, op, str
      call log%log( '', 'shape', '=', [ str ] )

      select case ( trim(str) )
      case ( 'rect' )
        shape => draw%new_shape( drid%rect )
        backspace (1)
        read (1, *) key, op, str, shape%xl(1:3), shape%length(1:3)

        call log%log( '', 'shape (rect, left_edge)', '=', shape%xl )
        call log%log( '', 'shape (rect, length)', '=', shape%length )

      case ( 'triangle' )
        shape => draw%new_shape( drid%triangle )
        backspace (1)
        read (1, *) key, op, str, shape%vertices(1,:), shape%vertices(2,:), &
          shape%vertices(3,:), shape%thickness

        call log%log( '', 'shape (triangle, vertice_1)', '=', shape%vertices(1,:) )
        call log%log( '', 'shape (triangle, vertice_2)', '=', shape%vertices(2,:) )
        call log%log( '', 'shape (triangle, vertice_3)', '=', shape%vertices(3,:) )
        call log%log( '', 'shape (triangle, thickness)', '=', [ shape%thickness ] )

      case ( 'sphere' )
        shape => draw%new_shape( drid%sphere )
        backspace (1)
        read (1, *) key, op, str, shape%x0(1:3), shape%r

        call log%log( '', 'shape (sphere, origin)', '=', shape%x0 )
        call log%log( '', 'shape (sphere, radius)', '=', [ shape%r ] )

      case ( 'smoothed_slab_2d' )
        shape => draw%new_shape( drid%smoothed_slab_2d )
        backspace (1)
        read (1, *) key, op, str, dir_str, shape%position(1:2), shape%sigma(1:2)

        select case ( trim(dir_str) )
        case ( 'x' ); shape%direction = drid%x
        case ( 'y' ); shape%direction = drid%y
        case ( 'z' ); shape%direction = drid%z
        case DEFAULT; call log%err( 'Unknown direction', dir_str )
        end select

        call log%log( '', 'shape (smoothed_slab_2d, direction)', '=', [ dir_str ] )
        call log%log( '', 'shape (smoothed_slab_2d, position)', '=', shape%position )
        call log%log( '', 'shape (smoothed_slab_2d, sigma)', '=', shape%sigma )
      case DEFAULT
        read (1, *)
        call log%err( 'Unknow shape', str )
      end select

    end subroutine read_shape


    subroutine read_shape_transition ( tag, idx )
      implicit none

      character ( len=* ), intent ( in ) :: tag
      integer, intent ( in ) :: idx

      read (1, *) key, op, str, shape%trans%sigma( idx )

      call log%log( '', 'shape (transition_'//trim(tag)//')', '=', [ str ] )

      select case ( trim(str) )
      case ( 'linear' )
        shape%trans%type( idx ) = drid%linear
      case ( 'cubic' )
        shape%trans%type( idx ) = drid%cubic
      case ( 'ramp' )
        shape%trans%type( idx ) = drid%ramp
      case DEFAULT
        read (1, *)
        call log%err( 'Unknown shape_transition_'//trim(tag), str )
      end select

      call log%log( '', 'shape (transition_'//trim(tag)//', sigma)', '=', &
        [ shape%trans%sigma( idx ) ] )

    end subroutine read_shape_transition


    subroutine read_shape_transition_colors ( tag, idx )
      implicit none

      character ( len=* ), intent ( in ) :: tag
      integer, intent ( in ) :: idx

      read (1, *) key, op, shape%trans%colors(idx, 1)%w(hyid%rho:hyid%p), &
        shape%trans%colors(idx, 2)%w(hyid%rho:hyid%p)

      call log%log( '', 'shape (transition_'//trim(tag)//', color(1))', '=', &
        [ shape%trans%colors( idx, 1 )%w ] )
      call log%log( '', 'shape (transition_'//trim(tag)//', color(2))', '=', &
        [ shape%trans%colors( idx, 2 )%w ] )
    end subroutine read_shape_transition_colors


    subroutine read_shape_filling ()
      implicit none

      read (1, *) key, op, str

      call log%log( '', 'shape_filling', '=', [ str ] )

      select case ( trim(str) )
      case ( 'uniform' )
        shape%fill%type = drid%uniform
      case DEFAULT
        read (1, *)
        call log%warn( 'Unknown shape_filling', str )
      end select
    end subroutine read_shape_filling


    subroutine read_shape_filling_colors ()
      implicit none

      if ( shape%fill%type .eq. drid%uniform ) then
        read (1, *) key, op, shape%fill%colors(1)%w(hyid%rho:hyid%p)

        call log%log( '', 'shape_filling_colors', '=', shape%fill%colors(1)%w )

      else if ( shape%type .eq. drid%smoothed_slab_2d ) then
        read (1, *) key, op, shape%fill%colors(1)%w(hyid%rho:hyid%p), &
          shape%fill%colors(2)%w(hyid%rho:hyid%p)

        call log%log( '', 'shape_filling_colors(1)', '=', shape%fill%colors(1)%w )
        call log%log( '', 'shape_filling_colors(2)', '=', shape%fill%colors(2)%w )
      else
        read (1, *)
        call log%err( 'Unknown shape_filling type', 'Cannot read colors')
      end if
    end subroutine read_shape_filling_colors

  end subroutine parse_params
end module rhyme_param_parser
