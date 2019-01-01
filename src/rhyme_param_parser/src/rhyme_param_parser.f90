module rhyme_param_parser
  use rhyme_samr
  use rhyme_samr_boundary_condition
  use rhyme_cfl
  use rhyme_ideal_gas
  use rhyme_initial_condition
  use rhyme_iterative_riemann_solver
  use rhyme_slope_limiter

  implicit none

contains

  logical function parse_params ( param_file, samr, bc, cfl, ig, ic, irs_config, sl ) result ( passed )
    implicit none

    character (len=1024), intent(in) :: param_file
    type ( samr_t ) :: samr
    type ( samr_boundary_condition_t ) :: bc
    type ( cfl_t ) :: cfl
    type ( ideal_gas_t ) :: ig
    type ( initial_condition_t ) :: ic
    type ( iterative_riemann_solver_config_t ) :: irs_config
    type ( slope_limiter_t ) :: sl

    integer :: i, ios
    character(len=1024) :: key, op, str
    type ( ic_shape_t ), pointer :: shape


    open (1, file=param_file, action='read', form="formatted")

    do
      read (1, *, iostat=ios) key
      if ( ios .ne. 0 ) exit

      key = adjustl(trim(key))
      if ( key(1:1) .eq. "#" ) cycle

      backspace (1)

      select case ( adjustl(trim(key)) )

        ! Structured AMR
      case ( "base_grid" ); read (1, *) key, op, samr%base_grid(1:3)
      case ( "nlevels" ); read (1, *) key, op, samr%nlevels
      case ( "nboxes" ); read (1, *) key, op, samr%tot_nboxes(0:samr%nlevels-1)

        ! Boundary Condition
      case ( "left_bc" ); read (1, *) key, op, bc%types(bcid%left)
      case ( "right_bc" ); read (1, *) key, op, bc%types(bcid%right)
      case ( "bottom_bc" ); read (1, *) key, op, bc%types(bcid%bottom)
      case ( "top_bc" ); read (1, *) key, op, bc%types(bcid%top)
      case ( "back_bc" ); read (1, *) key, op, bc%types(bcid%back)
      case ( "front_bc" ); read (1, *) key, op, bc%types(bcid%front)

        !CFL
      case ( "courant_number" ); read (1, *) key, op, cfl%courant_number

        ! Ideal Gas
      case ( "ideal_gas_type" );
         read (1, *) key, op, str

        if ( trim(str) .eq. "monatomic" ) then
          ig%type = igid%monatomic
        else if ( trim(str) .eq. "diatomic" ) then
          ig%type = igid%diatomic
        else if ( trim(str) .eq. "polyatomic" ) then
          ig%type = igid%polyatomic
        end if

        ! Initial Condition
      case ( "background" ); read (1, *) key, op, ic%background%w(hyid%rho:hyid%p)
      case ( "shape" )
        read (1, *) key, op, str

        if ( trim(str) .eq. "rect" ) then
          backspace (1)
          shape => ic%new_shape ( icid%rect )
          read (1, *) key, op, str, shape%xl(1:3), shape%length(1:3)
        else if ( trim(str) .eq. "circle" ) then
          backspace (1)
          shape => ic%new_shape ( icid%circle )
          read (1, *) key, op, str, shape%x0(1:3), shape%r
        end if
      case ( "shape_trans" );
        read (1, *) key, op, str, shape%trans%width_px

        if ( trim(str) .eq. "linear" ) then
          shape%trans%type = icid%linear
        else if ( trim(str) .eq. "cubic" ) then
          shape%trans%type = icid%cubic
        end if

      case ( "shape_fill" );
        read (1, *) key, op, str
        backspace (1)

        if ( trim(str) .eq. "uniform" ) then
          read (1, *) key, op, str, shape%fill%states(1)%w(hyid%rho:hyid%p)
          shape%fill%type = icid%uniform
        else
          read (1, *) key, op, str, shape%fill%states(1)%w(hyid%rho:hyid%p), &
          shape%fill%states(2)%w(hyid%rho:hyid%p)

          if ( trim(str) .eq. "grad_x" ) then
            shape%fill%type = icid%grad_x
          else if ( trim(str) .eq. "grad_y" ) then
            shape%fill%type = icid%grad_y
          else if ( trim(str) .eq. "grad_z" ) then
            shape%fill%type = icid%grad_z
          else if ( trim(str) .eq. "grad_r" ) then
            shape%fill%type = icid%grad_r
          end if
        end if

        ! Iterative Riemann Solver
      case ( "pressure_floor" ); read (1, *) key, op, irs_config%pressure_floor
      case ( "tolerance" ); read (1, *) key, op, irs_config%tolerance
      case ( "n_iteration" ); read (1, *) key, op, irs_config%n_iteration

        ! Slope limiter
      case ( "limiter" )
        read (1, *) key, op, str

        if ( trim(str) .eq. "van_leer") then
          sl%type = slid%van_Leer
        else if ( trim(str) .eq. "minmod") then
          sl%type = slid%minmod
        else if ( trim(str) .eq. "van_albada") then
          sl%type = slid%van_albada
        else if ( trim(str) .eq. "superbee") then
          sl%type = slid%superbee
        end if

      case ( "limiter_omega" ); read (1, *) key, op, sl%w


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
  end function parse_params

end module rhyme_param_parser
