submodule(rhyme_chombo) output_init_smod
contains

   module subroutine rhyme_chombo_output_init(this, logger)
      implicit none

      type(chombo_output_t), intent(inout) :: this
      type(logger_t), intent(inout) :: logger

      integer :: total_noutputs, filled_elements, i
      real(kind=8) :: dt
      type(chombo_output_rule_t), pointer :: rule

      total_noutputs = 0
      filled_elements = 0

      rule => this%rules
      do while (associated(rule))
         total_noutputs = total_noutputs + rule%noutputs
         rule => rule%next
      end do

      allocate (this%output_times(total_noutputs))

      rule => this%rules
      do while (associated(rule))
         select case (rule%type)
         case (chid%linear)
            dt = (rule%range(2) - rule%range(1))/(rule%noutputs - 1)
            do i = 1, rule%noutputs
               this%output_times(filled_elements + i) = rule%range(1) + (i - 1)*dt
            end do
         case (chid%log)
            dt = (log10(rule%range(2)) - log10(rule%range(1)))/(rule%noutputs - 1)
            do i = 1, rule%noutputs
               this%output_times(filled_elements + i) = rule%range(1)*10**((i - 1)*dt)
            end do
         case default
         end select

         filled_elements = filled_elements + rule%noutputs
         rule => rule%next
      end do

   end subroutine rhyme_chombo_output_init
end submodule output_init_smod
