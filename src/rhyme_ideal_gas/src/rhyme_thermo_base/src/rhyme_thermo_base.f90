module rhyme_thermo_base
  use rhyme_nombre

  implicit none

  type thermo_base_t
    logical :: initialized = .false.
    type ( nombre_t ) :: kB
  contains
    procedure :: init => rhyme_thermo_base_init
  end type thermo_base_t

contains

  subroutine rhyme_thermo_base_init ( this )
    implicit none

    class ( thermo_base_t ), intent ( inout ) :: this


    if ( this%initialized ) return

    this%kB = 1.38064852d-23 .u. m**2 * kg / ( s**2 * Kel )

    this%initialized = .true.
  end subroutine rhyme_thermo_base_init
end module rhyme_thermo_base
