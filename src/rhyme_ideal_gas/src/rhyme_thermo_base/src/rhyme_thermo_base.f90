module rhyme_thermo_base
  use rhyme_nombre
  use rhyme_log

  implicit none

  type thermo_base_t
    logical :: initialized = .false.
    type ( nombre_t ) :: kB
  contains
    procedure :: init => rhyme_thermo_base_init
  end type thermo_base_t

contains

  subroutine rhyme_thermo_base_init ( this, log )
    implicit none

    class ( thermo_base_t ), intent ( inout ) :: this
    type ( log_t ), intent ( inout ) :: log


    if ( this%initialized ) then
      call log%warn( "Try to re-initialize thermo object" )
      return
    end if

    this%kB = 1.38064852d-23 .u. meter**2 * kg / ( sec**2 * Kel )

    this%initialized = .true.
  end subroutine rhyme_thermo_base_init
end module rhyme_thermo_base
