module rhyme_thermo_base_factory
  use rhyme_thermo_base

  implicit none

  integer, parameter, private :: som = thid%diatomic

  type rhyme_thermo_base_factory_t
    integer :: state_of_matter = som
    logical :: initialized = .false.
  contains
    procedure :: init => rhyme_thermo_base_factory_init
    procedure :: generate => rhyme_thermo_base_factory_generate
  end type rhyme_thermo_base_factory_t

  type ( rhyme_thermo_base_factory_t ) :: th_factory = rhyme_thermo_base_factory_t()

contains

  subroutine rhyme_thermo_base_factory_init ( this )
    implicit none

    class ( rhyme_thermo_base_factory_t ), intent ( inout ) :: this

    this%state_of_matter = som

    this%initialized = .true.
  end subroutine rhyme_thermo_base_factory_init


  function rhyme_thermo_base_factory_generate ( this, gas_type ) result ( thermo )
    implicit none

    class ( rhyme_thermo_base_factory_t ), intent ( inout ) :: this
    integer, intent ( in ), optional :: gas_type
    type ( thermo_base_t ) :: thermo

    if ( .not. this%initialized ) call this%init

    if ( present( gas_type ) ) then
      thermo%state_of_matter = gas_type
    else
      thermo%state_of_matter = this%state_of_matter
    end if
  end function rhyme_thermo_base_factory_generate
end module rhyme_thermo_base_factory
