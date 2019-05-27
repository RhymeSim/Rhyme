submodule ( rhyme_chombo ) filename_generator_smod
contains
  module subroutine rhyme_chombo_filename_generator ( this, filename )
    implicit none

    class ( chombo_t ), intent ( in ) :: this
    character ( len=1024 ), intent ( out ) :: filename

    character ( len=6 ) :: itr_str

    filename = ""

    if ( len_trim( this%prefix ) > 0 ) then
      filename = trim(filename) // trim(this%prefix) // '/'
    end if

    if ( len_trim( this%nickname ) > 0 ) then
      filename = trim(filename) // trim(this%nickname) // "-"
    end if

    if ( this%iteration .eq. chid%unset ) then
      write ( itr_str, "(I0.5)" ) 0
    else
      write ( itr_str, "(I0.5)" ) this%iteration
    end if

    filename = trim(filename) // trim(itr_str) // ".chombo.h5"
  end subroutine rhyme_chombo_filename_generator
end submodule filename_generator_smod
