submodule(rhyme_plotter) histogram_smod
use iso_fortran_env

implicit none

contains
pure module function rhyme_plotter_histogram_1d( &
   d, nbins, scale, minmax, base, normalized) result(hist)
   implicit none

   real(kind=8), intent(in) :: d(:)
   integer, intent(in) :: nbins
   integer, intent(in) :: scale
   real(kind=8), intent(in), optional :: minmax(2)
   real(kind=8), intent(in), optional :: base
   logical, intent(in), optional :: normalized

   type(plotter_histogram_t) :: hist

   integer :: i, nbin
   real(kind=8) :: log10_dx

   if (present(minmax)) then
      hist%min = minmax(1)
      hist%max = minmax(2)
   else
      hist%min = minval(d)
      hist%max = maxval(d)
   end if

   call rhyme_plotter_histogram_init(hist, nbins, scale, base)

   select case (hist%scale)
   case (plid%linear)
      do i = 1, size(d)
         if (d(i) < hist%min .or. d(i) > hist%max) cycle

         nbin = int((d(i) - hist%min)/hist%dx)
         hist%counts(nbin) = hist%counts(nbin) + 1.d0
      end do
   case (plid%log)
      log10_dx = log10(hist%dx)
      do i = 1, size(d)
         if (d(i) < hist%min .or. d(i) > hist%max) cycle

         if (abs(d(i) - hist%min) < tiny(0.d0)) then
            nbin = 1
         else
            nbin = int(log10(d(i)/hist%min)/log10_dx)
         end if
         hist%counts(nbin) = hist%counts(nbin) + 1.d0
      end do
   end select

   if (.not. present(normalized) .or. (present(normalized) .and. normalized .eqv. .true.)) then
      hist%counts(1:hist%nbins) = hist%counts(1:hist%nbins)/size(d)
   end if
end function rhyme_plotter_histogram_1d

pure module function rhyme_plotter_histogram_2d( &
   d, nbins, scale, minmax, base, normalized) result(hist)
   implicit none

   real(kind=8), intent(in) :: d(:, :)
   integer, intent(in) :: nbins
   integer, intent(in) :: scale
   real(kind=8), intent(in), optional :: minmax(2)
   real(kind=8), intent(in), optional :: base
   logical, intent(in), optional :: normalized

   type(plotter_histogram_t) :: hist

   integer :: i, j, nbin
   real(kind=8) :: log10_dx

   if (present(minmax)) then
      hist%min = minmax(1)
      hist%max = minmax(2)
   else
      hist%min = minval(d)
      hist%max = maxval(d)
   end if

   call rhyme_plotter_histogram_init(hist, nbins, scale, base)

   select case (hist%scale)
   case (plid%linear)
      do j = lbound(d, dim=2), ubound(d, dim=2)
         do i = lbound(d, dim=1), ubound(d, dim=1)
            if (d(i, j) < hist%min .or. d(i, j) > hist%max) cycle

            nbin = int((d(i, j) - hist%min)/hist%dx)
            hist%counts(nbin) = hist%counts(nbin) + 1.d0
         end do
      end do
   case (plid%log)
      log10_dx = log10(hist%dx)
      do j = lbound(d, dim=2), ubound(d, dim=2)
         do i = lbound(d, dim=1), ubound(d, dim=1)
            if (d(i, j) < hist%min .or. d(i, j) > hist%max) cycle

            if (abs(d(i, j) - hist%min) < tiny(0.d0)) then
               nbin = 1
            else
               nbin = int(log10(d(i, j)/hist%min)/log10_dx)
            end if
            hist%counts(nbin) = hist%counts(nbin) + 1.d0
         end do
      end do
   end select

   if (.not. present(normalized) .or. (present(normalized) .and. normalized .eqv. .true.)) then
      hist%counts(1:hist%nbins) = hist%counts(1:hist%nbins)/size(d)
   end if
end function rhyme_plotter_histogram_2d

pure module function rhyme_plotter_histogram_3d( &
   d, nbins, scale, minmax, base, normalized) result(hist)
   implicit none

   real(kind=8), intent(in) :: d(:, :, :)
   integer, intent(in) :: nbins
   integer, intent(in) :: scale
   real(kind=8), intent(in), optional :: minmax(2)
   real(kind=8), intent(in), optional :: base
   logical, intent(in), optional :: normalized

   type(plotter_histogram_t) :: hist

   integer :: i, j, k, nbin
   real(kind=8) :: log10_dx

   if (present(minmax)) then
      hist%min = minmax(1)
      hist%max = minmax(2)
   else
      hist%min = minval(d)
      hist%max = maxval(d)
   end if

   call rhyme_plotter_histogram_init(hist, nbins, scale, base)

   select case (hist%scale)
   case (plid%linear)
      do k = lbound(d, dim=3), ubound(d, dim=3)
         do j = lbound(d, dim=2), ubound(d, dim=2)
            do i = lbound(d, dim=1), ubound(d, dim=1)
               if (d(i, j, k) < hist%min .or. d(i, j, k) > hist%max) cycle

               nbin = int((d(i, j, k) - hist%min)/hist%dx)
               hist%counts(nbin) = hist%counts(nbin) + 1.d0
            end do
         end do
      end do
   case (plid%log)
      log10_dx = log10(hist%dx)
      do k = lbound(d, dim=3), ubound(d, dim=3)
         do j = lbound(d, dim=2), ubound(d, dim=2)
            do i = lbound(d, dim=1), ubound(d, dim=1)
               if (d(i, j, k) < hist%min .or. d(i, j, k) > hist%max) cycle

               if (abs(d(i, j, k) - hist%min) < tiny(0.d0)) then
                  nbin = 1
               else
                  nbin = int(log10(d(i, j, k)/hist%min)/log10_dx)
               end if
               hist%counts(nbin) = hist%counts(nbin) + 1.d0
            end do
         end do
      end do
   end select

   if (.not. present(normalized) .or. (present(normalized) .and. normalized .eqv. .true.)) then
      hist%counts(1:hist%nbins) = hist%counts(1:hist%nbins)/size(d)
   end if
end function rhyme_plotter_histogram_3d

pure subroutine rhyme_plotter_histogram_init(hist, nbins, scale, base)
   implicit none

   type(plotter_histogram_t), intent(inout) :: hist
   integer, intent(in) :: nbins
   integer, intent(in) :: scale
   real(kind=8), intent(in), optional :: base

   integer :: nbin

   hist%nbins = min(nbins, max_nbins)
   hist%scale = scale
   hist%counts = 0d0

   if (scale .eq. plid%log) then
      if (present(base)) then
         hist%base = base
      else
         hist%base = 1d1
      end if
   end if

   select case (hist%scale)
   case (plid%linear)
      hist%dx = (hist%max - hist%min)/hist%nbins

      do nbin = 1, hist%nbins
         hist%bin_centers(nbin) = hist%min + hist%dx*real(nbin - .5d0, kind=8)
      end do
   case (plid%log)
      hist%dx = 1d1**(log10(hist%max/hist%min)/hist%nbins)

      do nbin = 1, hist%nbins
         hist%bin_centers(nbin) = hist%min*hist%dx**real(nbin - .5d0, kind=8)
      end do
   end select
end subroutine rhyme_plotter_histogram_init
end submodule histogram_smod
