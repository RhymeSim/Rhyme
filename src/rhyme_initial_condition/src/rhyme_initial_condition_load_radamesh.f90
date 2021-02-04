submodule(rhyme_initial_condition) load_radamesh
contains
   module subroutine rhyme_initial_condition_load_radamesh(ic, samr, logger)
      implicit none

      type(initial_condition_t), intent(in) :: ic
      type(samr_t), intent(inout) :: samr
      type(logger_t), intent(inout) :: logger

      type(chombo_t) :: ch
   end subroutine rhyme_initial_condition_load_radamesh
end submodule load_radamesh
