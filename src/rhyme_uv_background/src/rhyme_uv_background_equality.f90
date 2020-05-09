submodule(rhyme_uv_background) equality_smod
contains
pure module function rhyme_uv_background_equality(uvb1, uvb2) result(eq)
   implicit none

   type(uv_background_t), intent(in) :: uvb1, uvb2
   logical :: eq

   character(len=128) :: todo_holder

   eq = .false.

   write (todo_holder, *) 'TODO: implement ', uvb1, ' == ', uvb2
end function rhyme_uv_background_equality
end submodule equality_smod
