submodule(rhyme_string) rhyme_string_get_filename_smod
contains
pure elemental module function rhyme_strin_get_filename(path) result(filename)
   implicit none

   character(len=*), intent(in) :: path
   character(len=256) :: filename

   integer :: slash_loc

   slash_loc = scan(path, '/', BACK=.true.) + 1

   filename = adjustl(trim(path(slash_loc:)))
end function rhyme_strin_get_filename
end submodule rhyme_string_get_filename_smod
