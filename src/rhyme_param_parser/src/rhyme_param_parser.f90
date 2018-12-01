module rhyme_param_parser
  use rhyme_samr
  use rhyme_samr_boundary_condition
  use rhyme_chemistry

  implicit none

contains

  logical function parse_params ( param_file, samr, bc, chemi ) result ( passed )
    implicit none

    character (len=1024), intent(in) :: param_file
    type ( samr_t ) :: samr
    type ( samr_boundary_condition_t ) :: bc
    type ( chemistry_t ) :: chemi

    integer :: i, ios
    character(len=1024) :: key, op


    open (1, file=param_file, action='read', form="formatted")

    do
      read (1, *, iostat=ios) key
      if ( ios .ne. 0 ) exit

      key = adjustl(trim(key))
      if ( key(1:1) .eq. "#" ) cycle

      backspace (1)

      select case ( adjustl(trim(key)) )

        ! AMR params
      case ( "base_grid" ); read (1, *) key, op, samr%base_grid(1:3)
      case ( "nlevels" ); read (1, *) key, op, samr%nlevels
      case ( "nboxes" ); read (1, *) key, op, samr%nboxes

        ! Boundary condition params
      case ( "left_bc" ); read (1, *) key, op, bc%types(bc_id%left)
      case ( "right_bc" ); read (1, *) key, op, bc%types(bc_id%right)
      case ( "bottom_bc" ); read (1, *) key, op, bc%types(bc_id%bottom)
      case ( "top_bc" ); read (1, *) key, op, bc%types(bc_id%top)
      case ( "back_bc" ); read (1, *) key, op, bc%types(bc_id%back)
      case ( "front_bc" ); read (1, *) key, op, bc%types(bc_id%front)
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
