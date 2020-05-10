submodule(rhyme_param_parser) rhyme_param_parser_add_switch_submodule
contains
module subroutine rhyme_param_parser_add_switch(this, key, val)
   implicit none

   class(config_switch_t), intent(inout) :: this
   character(len=*), intent(in) :: key
   class(*), intent(in) :: val

   this%len = this%len + 1

   this%keys(this%len) = trim(key)

   select type (v=>val)
   type is (integer)
      this%types(this%len) = 'int'
      this%int_values(this%len) = v
   type is (logical)
      this%types(this%len) = 'log'
      this%log_values(this%len) = v
   class default
      this%len = this%len - 1
      print *, 'Error in switch: Unknown type!'
   end select
end subroutine rhyme_param_parser_add_switch
end submodule rhyme_param_parser_add_switch_submodule
