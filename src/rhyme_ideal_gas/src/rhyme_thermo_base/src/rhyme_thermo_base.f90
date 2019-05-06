module rhyme_thermo_base
  use rhyme_units
  use rhyme_log

  implicit none

  type thermo_base_t
    logical :: initialized = .false.
    type ( nombre_t ) :: kB
  end type thermo_base_t

  interface
    module subroutine rhyme_thermo_base_init ( this, units, logger )
      type ( thermo_base_t ), intent ( inout ) :: this
      type ( rhyme_units_t ), intent ( in ) :: units
      type ( log_t ), intent ( inout ) :: logger
    end subroutine rhyme_thermo_base_init
  end interface
end module rhyme_thermo_base
