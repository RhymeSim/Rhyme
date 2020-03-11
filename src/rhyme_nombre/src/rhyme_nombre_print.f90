submodule(rhyme_nombre) print_smod
contains
module function rhyme_nombre_print(n) result(str)
   implicit none

   class(nombre_t), intent(in) :: n
   character(len=128) :: str

   write (str, fmt="(ES10.3,A,A,A)") n%v, " [ ", trim(.printchain.n%u), " ]"
end function rhyme_nombre_print
end submodule print_smod
