logical function rhyme_color_test() result(failed)
   use rhyme_color_factory

   implicit none

   failed = &
      clrid%esc /= achar(27) &
      .and. clrid%start /= achar(27)//'[' &
      .and. clrid%clear /= achar(27)//'[0m'

   if (failed) return

   failed = &
      tc%black /= clrid%start//"38;5;0m"
end function rhyme_color_test
