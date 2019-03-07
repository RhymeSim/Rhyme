module rhyme_ideal_gas_factory
  use rhyme_ideal_gas
  use rhyme_hydro_base_factory

  implicit none

  logical :: ig_fac_initialized = .false.

  type ( rhyme_hydro_factory_t ) :: hy

  type ( chemistry_t ) :: chemi
  type ( thermo_base_t ) :: thermo
  type ( log_t ) :: log

  integer, parameter :: gas_type = igid%monatomic
contains

  subroutine rhyme_ideal_gas_factory_init ()
    implicit none

    if ( ig_fac_initialized ) return

    call hy%init

    call chemi%init( log )
    call thermo%init( log )

    ig_fac_initialized = .true.
  end subroutine rhyme_ideal_gas_factory_init
end module rhyme_ideal_gas_factory
