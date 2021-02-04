submodule(rhyme_color) equality_smod
contains
   pure module function rhyme_color_equality(clr1, clr2) result(eq)
      implicit none

      type(color_t), intent(in) :: clr1, clr2
      logical :: eq

      eq = &
         clr1%r == clr2%r &
         .and. clr1%g == clr2%g &
         .and. clr1%b == clr2%b &
         .and. clr1%n == clr2%n &
         .and. clr1%fg == clr2%fg &
         .and. clr1%bg == clr2%bg
   end function rhyme_color_equality
end submodule equality_smod
