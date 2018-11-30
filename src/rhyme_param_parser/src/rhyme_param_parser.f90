module rhyme_param_parser
  use rhyme_samr
  use rhyme_samr_boundary_condition
  use rhyme_chemistry

  implicit none

  type rhyme_samr_vars
    integer :: base_grid(3), ghost_cells(3), nlevels, nboxes
  end type rhyme_samr_vars

  type rhyme_bc_vars
    integer :: types(6)
  end type rhyme_bc_vars

contains

  logical function parse_params ( param_file, amr, bc, chemi ) result ( passed )
    implicit none

    character (len=1024), intent(in) :: param_file
    type ( samr_t ) :: amr
    type ( samr_boundary_condition_t ) :: bc
    type ( chemistry_t ) :: chemi

    integer :: i, ios
    character(len=1024) :: key, op

    type (rhyme_samr_vars) :: samr_vars
    type (rhyme_bc_vars) :: bc_vars


    open (1, file=param_file, action='read', form="formatted")

    do
      read (1, *, iostat=ios) key
      if ( ios .ne. 0 ) exit

      key = adjustl(trim(key))
      if ( key(1:1) .eq. "#" ) cycle

      backspace (1)

      select case ( adjustl(trim(key)) )

        ! AMR params
      case ( "base_grid" ); read (1, *) key, op, samr_vars%base_grid(1:3)
      case ( "nlevels" ); read (1, *) key, op, samr_vars%nlevels
      case ( "nboxes" ); read (1, *) key, op, samr_vars%nboxes

        ! Boundary condition params
      case ( "left_bc" ); read (1, *) key, op, bc_vars%types(bc_id%left)
      case ( "right_bc" ); read (1, *) key, op, bc_vars%types(bc_id%right)
      case ( "bottom_bc" ); read (1, *) key, op, bc_vars%types(bc_id%bottom)
      case ( "top_bc" ); read (1, *) key, op, bc_vars%types(bc_id%top)
      case ( "back_bc" ); read (1, *) key, op, bc_vars%types(bc_id%back)
      case ( "front_bc" ); read (1, *) key, op, bc_vars%types(bc_id%front)
      end select
    end do

    close (1)

    ! Initializing SAMR
    call set_samr_ghost_cells
    call amr%init ( &
      samr_vars%base_grid, &
      samr_vars%nlevels, &
      samr_vars%nboxes, &
      samr_vars%ghost_cells &
    )

    ! Initializing boundary conditions
    if ( .not. bc%init ( amr, bc_vars%types ) ) then
      passed = .false.
      return
    end if

    ! Initialize Chemistry
    call chemi%init

    passed = .true.

  contains

    subroutine set_samr_ghost_cells ()
      implicit none

      do i = 1, 3
        if ( samr_vars%base_grid(i) .gt. 1 ) then
          samr_vars%ghost_cells(i) = 2
        else
          samr_vars%ghost_cells(i) = 0
        end if
      end do

    end subroutine set_samr_ghost_cells

  end function parse_params

end module rhyme_param_parser
