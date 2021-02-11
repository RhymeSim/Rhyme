submodule(rhyme_chombo) output_init_smod
contains

   module subroutine rhyme_chombo_output_init(this, units, logger)
      implicit none

      type(chombo_output_t), intent(inout) :: this
      type(units_t), intent(in) :: units
      type(logger_t), intent(inout) :: logger

      integer :: total_noutputs, filled_elements, i
      real(kind=8) :: dt
      type(chombo_output_rule_t), pointer :: rule

      call logger%begin_section('output')

      total_noutputs = 0
      filled_elements = 0

      rule => this%rules
      do while (associated(rule))
         total_noutputs = total_noutputs + rule%noutputs
         rule => rule%next
      end do

      allocate (this%times(total_noutputs))
      allocate (this%saved(total_noutputs))

      this%saved = .false.

      rule => this%rules
      do while (associated(rule))
         select case (rule%type)
         case (chid%linear)
            dt = (rule%range(2) - rule%range(1))/(rule%noutputs - 1)
            do i = 1, rule%noutputs
               this%times(filled_elements + i) = rule%range(1) + (i - 1)*dt
            end do
         case (chid%log)
            dt = (log10(rule%range(2)) - log10(rule%range(1)))/(rule%noutputs - 1)
            do i = 1, rule%noutputs
               this%times(filled_elements + i) = rule%range(1)*10**((i - 1)*dt)
            end do
         case default
         end select

         filled_elements = filled_elements + rule%noutputs
         rule => rule%next
      end do

      call qsort(this%times, 1, total_noutputs)

      call logger%log('every', '[timestep]', '=', [this%every])
      call logger%log('times', '['//trim(.print.units%time)//']', '=', this%times)

      call logger%end_section ! output

   contains
      recursive subroutine qsort(a, first, last)
         implicit none
         real(kind=8) ::  a(:), x, t
         integer :: first, last, i, j

         x = a(int((first + last)/2))
         i = first
         j = last

         do
            do while (a(i) < x)
               i = i + 1
            end do

            do while (x < a(j))
               j = j - 1
            end do

            if (i >= j) exit

            t = a(i)
            a(i) = a(j)
            a(j) = t

            i = i + 1
            j = j - 1
         end do

         if (first < i - 1) call qsort(a, first, i - 1)
         if (j + 1 < last) call qsort(a, j + 1, last)
      end subroutine qsort
   end subroutine rhyme_chombo_output_init
end submodule output_init_smod
