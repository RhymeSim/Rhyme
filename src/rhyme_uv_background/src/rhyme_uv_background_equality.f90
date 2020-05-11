submodule(rhyme_uv_background) equality_smod
contains
pure module function rhyme_uv_background_equality(uvb1, uvb2) result(eq)
   implicit none

   type(uv_background_t), intent(in) :: uvb1, uvb2
   logical :: eq

   eq = .false.

   if (uvb1%model == uvb2%model) then
      eq = .true.
   end if
end function rhyme_uv_background_equality
end submodule equality_smod
