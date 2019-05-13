submodule ( rhyme_logger_util ) rhyme_logger_util_section_smod
contains
  module subroutine rhyme_logger_util_begin_section ( this, section )
    implicit none

    class ( logger_util_t ), intent ( inout ) :: this
    character ( len=* ), intent ( in ) :: section

    this%secid = this%secid + 1

    if ( len_trim( section ) < 32 ) then
      this%sections( this%secid ) = trim( section )
    else
      this%sections( this%secid ) = trim( section(:32) )
    end if
  end subroutine rhyme_logger_util_begin_section


  module subroutine rhyme_logger_util_end_section ( this )
    implicit none

    class ( logger_util_t ), intent ( inout ) :: this

    this%sections( this%secid ) = ''
    this%secid = this%secid - 1
  end subroutine rhyme_logger_util_end_section
end submodule rhyme_logger_util_section_smod
