logical function rhyme_plotter_two_d_histogram_test() result(failed)
   use rhyme_plotter

   implicit none

   type(plotter_2d_histogram_t) :: hist2d

   integer, parameter :: dlen = 1e6
   integer :: dl = dlen
   real(kind=8), dimension(dlen) :: x, y

   call random_seed(size=dl)
   call random_number(x)
   call random_number(y)

   failed = .false.

   if ( &
      hist2d%x%nbins /= 0 &
      .or. hist2d%x%scale /= plid%linear &
      .or. abs(hist2d%x%min - 0d0) > tiny(0d0) &
      .or. abs(hist2d%x%max - 0d0) > tiny(0d0) &
      .or. hist2d%y%nbins /= 0 &
      .or. hist2d%y%scale /= plid%linear &
      .or. abs(hist2d%y%min - 0d0) > tiny(0d0) &
      .or. abs(hist2d%y%max - 0d0) > tiny(0d0) &
      .or. any(abs(hist2d%counts - 0d0) > tiny(0d0)) &
      ) failed = .true.

   if (failed) return

   hist2d = rhyme_plotter_two_d_histogram( &
            x, y, 80, 60, plid%linear, plid%linear, &
            [0d0, 1d0], [0d0, 1d0])

   if ( &
      hist2d%x%nbins /= 80 &
      .or. hist2d%x%scale /= plid%linear &
      .or. abs(hist2d%x%min - 0d0) > tiny(0d0) &
      .or. abs(hist2d%x%max - 1d0) > tiny(0d0) &
      .or. hist2d%y%nbins /= 60 &
      .or. hist2d%y%scale /= plid%linear &
      .or. abs(hist2d%y%min - 0d0) > tiny(0d0) &
      .or. abs(hist2d%y%max - 1d0) > tiny(0d0) &
      .or. any(abs(hist2d%counts(1:80, 1:60) - 0d0) < tiny(0d0)) &
      ) failed = .true.

   if (failed) return

   hist2d = rhyme_plotter_two_d_histogram( &
            x, y, 80, 60, plid%linear, plid%log, &
            [0d0, 1d0], [minval(y, y > 0d0), 1d0])

   ! TODO: add test here

   if (failed) return

   hist2d = rhyme_plotter_two_d_histogram( &
            x, y, 80, 60, plid%linear, plid%linear, &
            [0d0, 1d0], [0d0, 1d0], normalized=.true.)

   if (any(hist2d%counts(1:80, 1:60) > 1d0)) failed = .true.

   if (failed) return
end function rhyme_plotter_two_d_histogram_test
