module rhyme_ideal_gas_factory
  use rhyme_ideal_gas
  use rhyme_units

  implicit none

  type rhyme_ideal_gas_factory_t
    integer :: gas_type = igid%monatomic
    type ( chemistry_t ) :: chemi
    type ( thermo_base_t ) :: thermo
    type ( rhyme_units_t ) :: units
    type ( log_t ) :: logger
    logical :: initialized = .false.
  contains
    procedure :: init => rhyme_ideal_gas_factory_init
    procedure :: generate => rhyme_ideal_gas_factory_generate
  end type rhyme_ideal_gas_factory_t

  type ( rhyme_ideal_gas_factory_t ) :: ig_factory = rhyme_ideal_gas_factory_t()

contains

  subroutine rhyme_ideal_gas_factory_init ( this )
    implicit none

    class ( rhyme_ideal_gas_factory_t ), intent ( inout ) :: this

    this%units%rho_str = 'kg / m^3'
    this%units%length_str = 'm'
    this%units%time_str = 's'

    call rhyme_units_init( this%units, this%logger )

    call rhyme_chemistry_init( this%chemi, this%units, this%logger )
    call rhyme_thermo_base_init( this%thermo, this%units, this%logger )
  end subroutine rhyme_ideal_gas_factory_init


  function rhyme_ideal_gas_factory_generate ( this, gas_type ) result ( ig )
    implicit none

    class ( rhyme_ideal_gas_factory_t ), intent ( inout ) :: this
    integer, intent ( in ), optional :: gas_type

    type ( ideal_gas_t ) :: ig

    if ( .not. this%initialized ) call this%init

    if ( present( gas_type ) ) then
      ig%type = gas_type
    else
      ig%type = igid%monatomic
    end if

    call rhyme_ideal_gas_init( ig, this%chemi, this%thermo, this%units, this%logger )
  end function rhyme_ideal_gas_factory_generate
end module rhyme_ideal_gas_factory
