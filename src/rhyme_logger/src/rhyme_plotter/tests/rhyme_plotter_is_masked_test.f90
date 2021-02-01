logical function rhyme_plotter_is_masked_test() result(failed)
   use rhyme_plotter

   implicit none

   real(kind=8), dimension(256) :: r
   integer :: i, seed

   failed = .false.
   seed = 12345

   call random_seed(size=seed)
   call random_number(r)

   do i = 1, size(r)
      if (rhyme_plotter_is_masked(r(i), plid%gtr_eq_zero) .neqv. r(i) >= 0d0) then
         failed = .true.
      end if
   end do

   do i = 1, size(r)
      if (rhyme_plotter_is_masked(r(i), plid%gtr_zero) .neqv. r(i) > 0d0) then
         failed = .true.
      end if
   end do

   do i = 1, size(r)
      if (rhyme_plotter_is_masked(r(i), plid%less_eq_zero) .neqv. r(i) <= 0d0) then
         failed = .true.
      end if
   end do

   do i = 1, size(r)
      if (rhyme_plotter_is_masked(r(i), plid%less_zero) .neqv. r(i) < 0d0) then
         failed = .true.
      end if
   end do

end function rhyme_plotter_is_masked_test
