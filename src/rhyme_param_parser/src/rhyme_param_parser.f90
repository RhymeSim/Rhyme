module rhyme_param_parser
  use rhyme_samr

  implicit none

  type rhyme_samr_vars
    integer :: base_grid(3), ghost_cells(3), nlevels, nboxes
  end type rhyme_samr_vars

contains

  logical function parse_params ( param_file, amr ) result ( passed )
    implicit none

    character (len=1024), intent(in) :: param_file
    type (samr_t) :: amr

    integer :: i, ios
    character(len=1024) :: key, op

    type (rhyme_samr_vars) :: samr_vars


    open (1, file=param_file, action='read', form="formatted")

    do
      read (1, *, iostat=ios) key
      if ( ios .ne. 0 ) exit

      key = adjustl(trim(key))
      if ( key(1:1) .eq. "#" ) cycle

      backspace (1)

      select case ( adjustl(trim(key)) )
      case ( "base_grid" ); read (1, *) key, op, samr_vars%base_grid(1:3)
      case ( "nlevels" ); read (1, *) key, op, samr_vars%nlevels
      case ( "nboxes" ); read (1, *) key, op, samr_vars%nboxes
      end select
    end do

    close (1)

    call initialize_samr

    passed = .true.

  contains

    subroutine initialize_samr ()
      implicit none

      do i = 1, 3
        if ( samr_vars%base_grid(i) .gt. 1 ) then
          samr_vars%ghost_cells(i) = 2
        else
          samr_vars%ghost_cells(i) = 0
        end if
      end do

      call amr%init ( &
      samr_vars%base_grid, &
      samr_vars%nlevels, &
      samr_vars%nboxes, &
      samr_vars%ghost_cells )
    end subroutine initialize_samr
  end function parse_params
end module rhyme_param_parser
