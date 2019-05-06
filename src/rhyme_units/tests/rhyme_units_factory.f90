module rhyme_units_factory
  use rhyme_units

  implicit none

  type rhyme_units_factory_t
    character ( len=32 ) :: rho_str = 'kg / m^3'
    character ( len=32 ) :: length_str = 'm'
    character ( len=32 ) :: time_str = 's'
  contains
    procedure :: generate => rhyme_units_factory_generate
  end type rhyme_units_factory_t

  type ( rhyme_units_factory_t ) :: units_factory = rhyme_units_factory_t()

contains

  function rhyme_units_factory_generate ( this ) result ( units )
    use rhyme_log

    implicit none

    class ( rhyme_units_factory_t ), intent ( inout ) :: this
    type ( rhyme_units_t ) :: units
    type ( log_t ) :: logger

    units%rho_str = this%rho_str
    units%length_str = this%length_str
    units%time_str = this%time_str

    call rhyme_units_init( units, logger )
  end function rhyme_units_factory_generate
end module rhyme_units_factory
