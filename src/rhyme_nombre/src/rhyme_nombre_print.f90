submodule ( rhyme_nombre ) print_smod
contains
  module function rhyme_nombre_print ( this ) result ( str )
    implicit none

    class ( nombre_t ), intent ( in ) :: this
    character ( len=128 ) :: str

    write (str, fmt="(E9.3,A,A,A)") this%v, " [ ", trim(this%u%p()), " ]"
  end function rhyme_nombre_print
end submodule print_smod
