module rhyme_slope_limiter_factory
  use rhyme_slope_limiter
  use rhyme_hydro_base_factory
  use rhyme_ideal_gas_factory
  use rhyme_log

  implicit none

  logical :: sl_fac_initialized = .false.

  type ( ideal_gas_t ) :: sl_ig
  type ( rhyme_hydro_factory_t ) :: sl_hy
  type ( hydro_conserved_t ) :: UL, UM, UR

contains

  subroutine rhyme_slope_limiter_factory_init ()
    implicit none

    type ( log_t ) :: logger

    if ( sl_fac_initialized ) return

    call sl_hy%init
    call rhyme_ideal_gas_factory_init

    sl_ig%type = igid%diatomic
    call rhyme_ideal_gas_init( sl_ig, ig_chemi, ig_thermo, ig_units, logger )

    sl_fac_initialized = .true.
  end subroutine rhyme_slope_limiter_factory_init
end module rhyme_slope_limiter_factory
