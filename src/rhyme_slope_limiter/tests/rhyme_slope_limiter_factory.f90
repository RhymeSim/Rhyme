module rhyme_slope_limiter_factory
  use rhyme_slope_limiter
  use rhyme_hydro_base_factory
  use rhyme_log

  implicit none

  logical :: sl_fac_initialized = .false.

  type ( chemistry_t ) :: chemi
  type ( thermo_base_t ) :: thermo
  type ( log_t ) :: log

  type ( rhyme_hydro_factory_t ) :: hy

  type ( hydro_conserved_t ) :: UL, UM, UR

contains

  subroutine rhyme_slope_limiter_factory_init ()
    implicit none

    if ( sl_fac_initialized ) return

    call chemi%init( log )
    call thermo%init( log )

    call hy%init

    sl_fac_initialized = .true.
  end subroutine rhyme_slope_limiter_factory_init
end module rhyme_slope_limiter_factory
