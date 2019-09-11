module rhyme_nombre_base_unit_chain_factory
  use rhyme_nombre_base_unit_chain

  implicit none

  type rhyme_nombre_base_unit_chain_factory_t
    logical :: initialized = .false.
  contains
    procedure :: init => rhyme_nombre_base_unit_chain_factory_init
    procedure :: generate => rhyme_nombre_base_unit_chain_factory_generate
  end type rhyme_nombre_base_unit_chain_factory_t

  type ( rhyme_nombre_base_unit_chain_factory_t ) :: nom_buc_factory = rhyme_nombre_base_unit_chain_factory_t()

contains

  subroutine rhyme_nombre_base_unit_chain_factory_init ( this )
    implicit none

    class ( rhyme_nombre_base_unit_chain_factory_t ), intent ( inout ) :: this

    this%initialized = .true.
  end subroutine rhyme_nombre_base_unit_chain_factory_init


  function rhyme_nombre_base_unit_chain_factory_generate ( this, units ) result ( uc )
    implicit none

    class ( rhyme_nombre_base_unit_chain_factory_t ), intent ( inout ) :: this
    type ( nombre_base_unit_t ), target, intent ( in ) :: units(:)
    type ( nombre_base_unit_t ), pointer :: uc

    integer :: i

    if ( .not. this%initialized ) call this%init

    uc => null()

    if ( size( units ) > 0 ) then
      uc => .clone. units(1)

      do i = 2, size( units )
        uc%next => .clone. units(i)
        uc%next%prev => uc
        uc => uc%next
      end do
    end if

    uc => .head. uc
  end function rhyme_nombre_base_unit_chain_factory_generate
end module rhyme_nombre_base_unit_chain_factory
