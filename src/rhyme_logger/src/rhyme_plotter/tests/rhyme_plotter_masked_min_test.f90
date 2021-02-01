logical function rhyme_plotter_masked_min_test() result(failed)
   use rhyme_plotter

   implicit none

   real(kind=8) :: minimum, arr(7) = [-2.5d0, -1.5d0, -.5d0, 0d0, .5d0, 1.5d0, 2.5d0]

   failed = .false.

   minimum = rhyme_plotter_masked_min(arr, plid%gtr_eq_zero)
   if (abs(minimum + 2.5d0) > tiny(0d0)) then
      failed = .true.
   end if

   minimum = rhyme_plotter_masked_min(arr, plid%gtr_zero)
   if (abs(minimum + 2.5d0) > tiny(0d0)) then
      failed = .true.
   end if

   minimum = rhyme_plotter_masked_min(arr, plid%less_eq_zero)
   if (abs(minimum - .5d0) > tiny(0d0)) then
      failed = .true.
   end if

   minimum = rhyme_plotter_masked_min(arr, plid%less_zero)
   if (abs(minimum - 0d0) > tiny(0d0)) then
      failed = .true.
   end if

   minimum = rhyme_plotter_masked_min(arr, plid%no_mask)
   if (abs(minimum + 2.5d0) > tiny(0d0)) then
      failed = .true.
   end if
end function rhyme_plotter_masked_min_test
