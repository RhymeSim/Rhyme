module rhyme_param_parser
  use rhyme_log
  use rhyme_samr
  use rhyme_samr_bc
  use rhyme_cfl
  use rhyme_ideal_gas
  use rhyme_drawing
  use rhyme_iterative_riemann_solver
  use rhyme_slope_limiter
  use rhyme_chombo

  implicit none

contains

  logical function parse_params ( &
    param_file, log, samr, bc, cfl, ig, draw, irs, sl, chombo ) &
  result ( passed )
    implicit none

    character (len=1024), intent ( in ) :: param_file
    type ( log_t ), intent ( out ) :: log
    type ( samr_t ), intent ( out ) :: samr
    type ( samr_bc_t ), intent ( out ) :: bc
    type ( cfl_t ), intent ( out ) :: cfl
    type ( ideal_gas_t ), intent ( out ) :: ig
    type ( drawing_t ), intent ( out ) :: draw
    type ( iterative_riemann_solver_t ), intent ( out ) :: irs
    type ( slope_limiter_t ), intent ( out ) :: sl
    type ( chombo_t ), intent ( out ) :: chombo

    integer :: i, ios
    character(len=1024) :: key, op, str
    type ( shape_t ), pointer :: shape

    call log%set_section( 'params' )

    open (1, file=param_file, action='read', form="formatted")

    do
      read (1, *, iostat=ios) key
      if ( ios .ne. 0 ) exit

      key = adjustl(trim(key))
      if ( key(1:1) .eq. "#" ) cycle

      backspace (1)

      select case ( adjustl(trim(key)) )

        ! Structured AMR
      case ( "base_grid" )
        read (1, *) key, op, samr%base_grid(1:3)
        call log%write_kw1d( 'base_grid', samr%base_grid )
      case ( "nlevels" )
        read (1, *) key, op, samr%nlevels
        call log%write_kw( 'nlevels', samr%nlevels )
      case ( "max_nboxes" )
        read (1, *) key, op, samr%max_nboxes( 0:samr%nlevels-1 )
        samr%max_nboxes( samr%nlevels: ) = 0
        call log%write_kw1d( 'max_nboxes', samr%max_nboxes )

        ! Boundary Condition
      case ( "left_bc" )
        read (1, *) key, op, str
        bc%types(bcid%left) = select_boundary ( str )
        call log%write_kw ( 'left_bc', str )
      case ( "right_bc" )
        read (1, *) key, op, str
        bc%types(bcid%right) = select_boundary ( str )
        call log%write_kw ( 'right_bc', str )
      case ( "bottom_bc" )
        read (1, *) key, op, str
        bc%types(bcid%bottom) = select_boundary ( str )
        call log%write_kw ( 'bottom_bc', str )
      case ( "top_bc" )
        read (1, *) key, op, str
        bc%types(bcid%top) = select_boundary ( str )
        call log%write_kw ( 'top_bc', str )
      case ( "back_bc" )
        read (1, *) key, op, str
        bc%types(bcid%back) = select_boundary ( str )
        call log%write_kw ( 'back_bc', str )
      case ( "front_bc" )
        read (1, *) key, op, str
        bc%types(bcid%front) = select_boundary ( str )
        call log%write_kw ( 'front_bc', str )

        !CFL
      case ( "courant_number" )
        read (1, *) key, op, cfl%courant_number
        call log%write_kw ( 'courant_number', cfl%courant_number )

        ! Ideal Gas
      case ( "ideal_gas_type" );
         read (1, *) key, op, str
         call log%write_kw( 'ideal_gas_type', str )

        if ( trim(str) .eq. "monatomic" ) then
          ig%type = igid%monatomic
        else if ( trim(str) .eq. "diatomic" ) then
          ig%type = igid%diatomic
        else if ( trim(str) .eq. "polyatomic" ) then
          ig%type = igid%polyatomic
        end if

        ! Initial Condition
      case ( "background" )
        read (1, *) key, op, str
        call log%write_kw( 'background_type', str )
        backspace(1)

        if ( trim(str) .eq. 'uniform' ) then
          draw%type = drid%uniform_bg
          read (1, *) key, op, str, draw%background%w(hyid%rho:hyid%p)
          call log%write_kw1d( 'background_uniform', draw%background%w )
        else if ( trim(str) .eq. 'transparent' ) then
          draw%type = drid%transparent_bg
          call log%write( 'Using transparent background' )
        else
          call log%warn_kw( 'Unsuported background', 'bakcground_type', str )
        end if



      case ( "shape" )
        read (1, *) key, op, str
        call log%write_kw( 'shape', str )
        backspace (1)

        if ( trim(str) .eq. "rect" ) then
          shape => draw%new_shape ( drid%rect )
          read (1, *) key, op, str, shape%xl(1:3), shape%length(1:3)
          call log%write_kw1d( 'rect_left_edge', shape%xl )
          call log%write_kw1d( 'rect_length', shape%length )
        else if ( trim(str) .eq. "circle" ) then
          shape => draw%new_shape ( drid%circle )
          read (1, *) key, op, str, shape%x0(1:3), shape%r
          call log%write_kw1d( 'circle_origin', shape%x0 )
          call log%write_kw( 'circle_radius', shape%r )
        end if

      case ( "shape_trans" );
        read (1, *) key, op, str, shape%trans%width_px
        call log%write_kw( 'shape_transition', str)
        call log%write_kw( 'shape_transition_width', shape%trans%width_px)

        if ( trim(str) .eq. "linear" ) then
          shape%trans%type = drid%linear
        else if ( trim(str) .eq. "cubic" ) then
          shape%trans%type = drid%cubic
        end if

      case ( "shape_fill" );
        read (1, *) key, op, str
        call log%write_kw( 'shape_filling', str )

        backspace (1)

        if ( trim(str) .eq. "uniform" ) then
          read (1, *) key, op, str, shape%fill%states(1)%w(hyid%rho:hyid%p)
          shape%fill%type = drid%uniform
          call log%write_kw1d( 'shape_filling_primary', shape%fill%states(1)%w )
        else
          read (1, *) key, op, str, shape%fill%states(1)%w(hyid%rho:hyid%p), &
          shape%fill%states(2)%w(hyid%rho:hyid%p)
          call log%write_kw( 'shape_filling_gradient', str )
          call log%write_kw1d( 'shape_filling_secondary', shape%fill%states(2)%w )

          if ( trim(str) .eq. "grad_x" ) then
            shape%fill%type = drid%grad_x
          else if ( trim(str) .eq. "grad_y" ) then
            shape%fill%type = drid%grad_y
          else if ( trim(str) .eq. "grad_z" ) then
            shape%fill%type = drid%grad_z
          else if ( trim(str) .eq. "grad_r" ) then
            shape%fill%type = drid%grad_r
          end if
        end if

        ! Iterative Riemann Solver
      case ( "pressure_floor" )
        read (1, *) key, op, irs%pressure_floor
        call log%write_kw( 'pressure_floor', irs%pressure_floor )
      case ( "tolerance" )
        read (1, *) key, op, irs%tolerance
        call log%write_kw( 'tolerance', irs%tolerance )
      case ( "n_iteration" )
        read (1, *) key, op, irs%n_iteration
        call log%write_kw( 'n_iteration', irs%n_iteration )

        ! Slope limiter
      case ( "limiter" )
        read (1, *) key, op, str
        call log%write_kw( 'slope_limiter', str )

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
        call log%write_kw( 'slope_limiter_omega (w)', sl%w )

        ! Chombo
      case ( "prefix" )
        read (1, *) key, op, chombo%prefix
        call log%write_kw( 'prefix', chombo%prefix )
      case ( "nickname" )
        read (1, *) key, op, chombo%nickname
        call log%write_kw( 'nickname', chombo%nickname )


        ! Unknown option
      case default
        print *, "**", trim(key), "**", " is not an option!"
        read (1, *)
      end select
    end do

    close (1)

    do i = 1, 3
      if ( samr%base_grid(i) .gt. 1 ) then
        samr%ghost_cells(i) = 2
      else
        samr%ghost_cells(i) = 0
      end if
    end do

    passed = .true.

  contains

    integer function select_boundary ( str ) result ( bc )
      implicit none

      character(len=1024) :: str

      select case ( trim(str) )
      case ( "reflective" ); bc = bcid%reflective
      case ( "outflow" ); bc = bcid%outflow
      case ( "periodic" ); bc = bcid%periodic
      end select
    end function select_boundary
  end function parse_params

end module rhyme_param_parser
