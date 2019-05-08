module rhyme_drawing_factory
  use rhyme_drawing

  implicit none

  type rhyme_drawing_factory_t
    logical :: initialized = .false.
  contains
    procedure :: init => rhyme_drawing_factory_init
  end type rhyme_drawing_factory_t

  type ( rhyme_drawing_factory_t ) :: draw_factory = rhyme_drawing_factory_t()

contains

  subroutine rhyme_drawing_factory_init ( this )
    implicit none

    class ( rhyme_drawing_factory_t ), intent ( inout ) :: this


    this%initialized = .true.
  end subroutine rhyme_drawing_factory_init
end module rhyme_drawing_factory
