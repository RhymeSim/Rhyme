submodule ( rhyme_logger_util ) rhyme_logger_util_time_smod
contains
  module subroutine rhyme_logger_util_update_time ( this )
    implicit none

    class ( logger_util_t ), intent ( inout ) :: this

    call date_and_time( values=this%t )
  end subroutine rhyme_logger_util_update_time


  module function rhyme_logger_util_time ( this, color ) result ( time_str )
    implicit none

    class ( logger_util_t ), intent ( inout ) :: this
    character ( len=* ), intent ( in ), optional :: color
    character ( len=64 ) :: time_str

    call this%update_time

    write ( time_str, fmt=logger_util_const%time_fmt ) &
      '[',this%t(1),'-',this%t(2),'-',this%t(3),'|', &
      this%t(5),':',this%t(6),':',this%t(7),']'

    if ( present( color ) ) then
      time_str = trim(color)//trim(time_str)//tc%nc
    end if
  end function rhyme_logger_util_time


  module function rhyme_logger_util_time_and_section ( this, color ) result ( tas_str )
    implicit none

    class ( logger_util_t ), intent ( inout ) :: this
    character ( len=* ), intent ( in ), optional :: color
    character ( len=126 ) :: tas_str

    integer :: i

    if ( present( color ) ) tas_str = trim( this%time( color=color ) )//' '//trim(color)

    do i = 1, this%secid
      if ( i > 1 ) tas_str = trim( tas_str )//'|'
      tas_str = trim( tas_str )//trim( adjustl( this%sections( i )))
    end do

    tas_str = trim( tas_str )//':'

    if ( present( color ) ) tas_str = trim( tas_str )//tc%nc
  end function rhyme_logger_util_time_and_section
end submodule rhyme_logger_util_time_smod
