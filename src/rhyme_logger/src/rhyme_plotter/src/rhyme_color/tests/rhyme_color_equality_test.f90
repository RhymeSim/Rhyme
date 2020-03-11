logical function rhyme_color_equality_test() result(failed)
   use rhyme_color_factory

   implicit none

   type(color_t) :: clr(2)

   call color_factory%init('Case1')
   clr(1) = color_factory%generate()

   call color_factory%init('Case2')
   clr(2) = color_factory%generate()

   failed = &
      .not. clr(1) == clr(2) &
      .and. clr(1) == clr(1) &
      .and. .not. clr(2) == clr(1) &
      .and. clr(2) == clr(2)
end function rhyme_color_equality_test
