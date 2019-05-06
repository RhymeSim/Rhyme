module rhyme_ideal_gas_factory
  use rhyme_ideal_gas
  use rhyme_units
  use rhyme_hydro_base_factory

  implicit none

  logical :: ig_fac_initialized = .false.

  type ( rhyme_hydro_factory_t ) :: ig_hy

  type ( chemistry_t ) :: ig_chemi
  type ( thermo_base_t ) :: ig_thermo
  type ( rhyme_units_t ) :: ig_units
  type ( log_t ) :: ig_logger

  integer, parameter :: ig_gas_type = igid%monatomic
contains

  subroutine rhyme_ideal_gas_factory_init ()
    implicit none

    if ( ig_fac_initialized ) return

    call ig_hy%init

    ig_units%rho_str = 'kg / m^3'
    ig_units%length_str = 'm'
    ig_units%time_str = 's'

    call rhyme_units_init( ig_units, ig_logger )

    call rhyme_chemistry_init( ig_chemi, ig_units, ig_logger )
    call rhyme_thermo_base_init( ig_thermo, ig_units, ig_logger )

    ig_fac_initialized = .true.
  end subroutine rhyme_ideal_gas_factory_init
end module rhyme_ideal_gas_factory
