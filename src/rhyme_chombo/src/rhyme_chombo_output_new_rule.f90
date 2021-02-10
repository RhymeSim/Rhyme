submodule(rhyme_chombo) output_new_rule_smod
contains

   module subroutine rhyme_chombo_output_new_rule(this, rule_type)
      implicit none

      class(chombo_output_t), intent(inout) :: this
      integer, intent(in) :: rule_type

      type(chombo_output_rule_t), pointer :: rule

      rule => this%rules

      if (associated(rule)) then
         do while (associated(rule%next))
            rule => rule%next
         end do

         allocate (rule%next)
         rule => rule%next
      else
         allocate (this%rules)
         rule => this%rules
      end if

      rule%type = rule_type
   end subroutine rhyme_chombo_output_new_rule
end submodule output_new_rule_smod
