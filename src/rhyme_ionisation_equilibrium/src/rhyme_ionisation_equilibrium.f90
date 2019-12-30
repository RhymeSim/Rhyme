module rhyme_ionisation_equilibrium
  use rhyme_logger

  implicit none

  type, private :: indices_t
    integer :: unset = -1
    integer :: case_a = 1, case_b = 2
  end type indices_t

  type(indices_t), parameter :: ieid = indices_t()


  type ionisation_equilibrium_t
    integer :: case = ieid%unset
    logical :: uvb = .false.
    logical :: collisional = .false.
    logical :: photo = .false.
  contains
    procedure :: rhyme_ionisation_equilibrium_write_formatted
    generic :: write(formatted) => rhyme_ionisation_equilibrium_write_formatted
  end type ionisation_equilibrium_t


  interface
    module subroutine rhyme_ionisation_equilibrium_init(ie, logger)
      type(ionisation_equilibrium_t), intent(inout) :: ie
      type(logger_t), intent(inout) :: logger
    end subroutine rhyme_ionisation_equilibrium_init

    pure module function rhyme_ionisation_equilibrium_equality(ie1, ie2) result(eq)
      type (ionisation_equilibrium_t), intent(in) :: ie1, ie2
      logical :: eq
    end function rhyme_ionisation_equilibrium_equality
  end interface


  interface operator(==)
    module procedure rhyme_ionisation_equilibrium_equality
  end interface operator(==)

contains
  subroutine rhyme_ionisation_equilibrium_write_formatted( &
    this, unit, iotype, v_list, iostat, iomsg)
    implicit none

    class (ionisation_equilibrium_t), intent(in) :: this
    integer, intent(in) :: unit
    character(len=*), intent(in) :: iotype
    integer, intent(in) :: v_list(:)
    integer, intent(out) :: iostat
    character(len=*), intent(inout) :: iomsg

    write(unit, fmt='(A,A,I0,A,L,A,L,A,L,A,A,A,A,I0,A)', iostat=iostat, iomsg=iomsg) &
      '<ionisation_equilibrium_t', &
      ' case=', this%case, &
      ' uvb=', this%uvb, &
      ' coll=', this%collisional, &
      ' photo=', this%photo, &
      ' iotype="', trim(iotype), '"', &
      ' v_list=', size(v_list), &
      '>'
  end subroutine rhyme_ionisation_equilibrium_write_formatted
end module rhyme_ionisation_equilibrium
