module rhyme_nombre_derived_unit_factory
  use rhyme_nombre_derived_unit

  implicit none

  type rhyme_nombre_derived_unit_factory_t
    logical :: initialized = .false.
  contains
    procedure :: init => rhyme_nombre_derived_unit_factory_init
    procedure :: generate => rhyme_nombre_derived_unit_factory_generate
    procedure :: generate_chain => rhyme_nombre_derived_unit_factory_generate_chain
  end type rhyme_nombre_derived_unit_factory_t

  type ( rhyme_nombre_derived_unit_factory_t ) :: nom_du_factory = rhyme_nombre_derived_unit_factory_t()

contains

  subroutine rhyme_nombre_derived_unit_factory_init ( this )
    implicit none

    class ( rhyme_nombre_derived_unit_factory_t ), intent ( inout ) :: this

    type ( nombre_base_unit_t ), pointer :: base_unit

    this%initialized = .true.
  end subroutine rhyme_nombre_derived_unit_factory_init


  function rhyme_nombre_derived_unit_factory_generate ( this, bases, symb ) result ( dunit )
    implicit none

    class ( rhyme_nombre_derived_unit_factory_t ), intent ( inout ) :: this
    type ( nombre_base_unit_t ), intent ( in ) :: bases(:)
    character ( len=* ), intent ( in ), optional :: symb
    type ( nombre_derived_unit_t ), pointer :: dunit

    if ( .not. this%initialized ) call this%init

    dunit => rhyme_nombre_derived_unit_new()
    dunit%head => this%generate_chain( bases )
    dunit%dim = rhyme_nombre_derived_unit_get_dim( dunit )

    if ( present( symb ) ) then
      dunit%symb = symb
    end if

  end function rhyme_nombre_derived_unit_factory_generate


  function rhyme_nombre_derived_unit_factory_generate_chain ( this, units ) result ( unit_chain )
    implicit none

    class ( rhyme_nombre_derived_unit_factory_t ), intent ( inout ) :: this
    type ( nombre_base_unit_t ), target, intent ( in ) :: units(:)
    type ( nombre_base_unit_t ), pointer :: unit_chain

    integer :: i

    if ( .not. this%initialized ) call this%init

    if ( size( units ) > 0 ) then
      unit_chain => rhyme_nombre_base_unit_clone( units(1) )

      do i = 2, size( units )
        unit_chain%next => rhyme_nombre_base_unit_clone( units(i) )
        unit_chain%next%prev => unit_chain
        unit_chain => unit_chain%next
      end do
    end if

    unit_chain => rhyme_nombre_base_unit_head( unit_chain )
  end function rhyme_nombre_derived_unit_factory_generate_chain
end module rhyme_nombre_derived_unit_factory
