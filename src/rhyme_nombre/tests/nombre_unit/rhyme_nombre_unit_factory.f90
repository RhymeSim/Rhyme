module rhyme_nombre_unit_factory
  use rhyme_nombre_unit

  implicit none

  type rhyme_nombre_unit_factory_t
    logical :: initialized = .false.
  contains
    procedure :: init => rhyme_nombre_unit_factory_init
    procedure :: generate_chain => rhyme_nombre_unit_factory_generate_chain
  end type rhyme_nombre_unit_factory_t

  type ( rhyme_nombre_unit_factory_t ) :: nom_duc_factory = rhyme_nombre_unit_factory_t()

contains

  subroutine rhyme_nombre_unit_factory_init ( this )
    implicit none

    class ( rhyme_nombre_unit_factory_t ), intent ( inout ) :: this

    this%initialized = .true.
  end subroutine rhyme_nombre_unit_factory_init


  function rhyme_nombre_unit_factory_generate_chain ( this, bases ) result ( dunit_chain )
    implicit none

    class ( rhyme_nombre_unit_factory_t ), intent ( inout ) :: this
    type ( nombre_unit_t ), target :: bases(:)
    type ( nombre_unit_t ), pointer :: dunit_chain

    integer :: i

    if ( .not. this%initialized ) call this%init

    dunit_chain => null()

    if ( size( bases ) > 0 ) then
      dunit_chain => .clone. bases(1)

      do i = 2, size( bases )
        dunit_chain%next => rhyme_nombre_derived_unit_clone( bases(i) )
        dunit_chain%next%prev => dunit_chain
        dunit_chain => dunit_chain%next
      end do
    end if

    dunit_chain => .head. dunit_chain
  end function rhyme_nombre_unit_factory_generate_chain
end module rhyme_nombre_unit_factory
