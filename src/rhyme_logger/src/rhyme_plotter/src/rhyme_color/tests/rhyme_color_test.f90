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

   if (failed) return

   failed = &
      csid%magma_grey /= 1 &
      .and. csid%rainbow /= 2 &
      .and. csid%smooth_rainbow /= 3 &
      .and. csid%viridis /= 3 &
      .and. csid%len /= 4 &
      .and. csid%unknown /= -2 &
      .and. csid%low_end /= -1 &
      .and. csid%high_end /= 0 &
      .and. csid%pallet_len /= 32

   if (failed) return
end function rhyme_color_test
