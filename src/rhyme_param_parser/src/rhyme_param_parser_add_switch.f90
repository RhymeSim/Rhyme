submodule(rhyme_param_parser) rhyme_param_parser_add_switch_submodule
contains
pure module subroutine rhyme_param_parser_add_switch(this, key, val)
   implicit none

   class(config_switch_t), intent(inout) :: this
   character(len=*), intent(in) :: key
   integer, intent(in) :: val

   this%len = this%len + 1

   this%keys(this%len) = trim(key)
   this%values(this%len) = val
end subroutine rhyme_param_parser_add_switch
end submodule rhyme_param_parser_add_switch_submodule
