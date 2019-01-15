module rhyme_chombo
  use rhyme_hdf5_util
  use rhyme_samr

  implicit none

  type, extends ( rhyme_hdf5_util_t ) :: rhyme_chombo_t
    integer :: num_levels = 0
    integer :: num_components = 0
  contains
    procedure :: setup => rhyme_chombo_setup
  end type rhyme_chombo_t

contains

  subroutine rhyme_chombo_setup ( this, filename, samr )
    implicit none

    class ( rhyme_chombo_t ), intent(inout) :: this
    character ( len=* ), intent(in) :: filename
    type ( samr_t ), intent(in) :: samr

    if ( this%initialized ) return

  end subroutine rhyme_chombo_setup
end module rhyme_chombo
