submodule ( rhyme_chombo ) create_chombo_smod
contains
  module subroutine rhyme_chombo_create_chombo ( this )
    implicit none

    class ( chombo_t ), intent (inout) :: this

    character ( len=1024 ) :: filename

    call this%filename_generator( filename )
    call this%create( filename )

    this%is_opened = .true.

  end subroutine rhyme_chombo_create_chombo
end submodule create_chombo_smod
