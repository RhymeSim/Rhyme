module rhyme_ideal_gas_factory
  use rhyme_ideal_gas

  implicit none

  type rhyme_ideal_gas_factory_t
    logical :: initialized = .false.
  contains
    procedure :: init => rhyme_ideal_gas_factory_init
  end type rhyme_ideal_gas_factory_t

  type ( rhyme_ideal_gas_factory_t ) :: ig_factory = rhyme_ideal_gas_factory_t()

contains

  subroutine rhyme_ideal_gas_factory_init ( this )
    implicit none

    class ( rhyme_ideal_gas_factory_t ), intent ( inout ) :: this

    this%initialized = .true.
  end subroutine rhyme_ideal_gas_factory_init
end module rhyme_ideal_gas_factory
