module rhyme_cfl
   use rhyme_samr
   use rhyme_thermo_base

   implicit none

   type cfl_t
      real(kind=8) :: courant_number = .8d0
   end type cfl_t

   interface
      module subroutine rhyme_cfl_init(cfl, thermo, samr, logger)
         type(cfl_t), intent(inout) :: cfl
         type(thermo_base_t), intent(in) :: thermo
         type(samr_t), intent(in) :: samr
         type(logger_t), intent(inout) :: logger
      end subroutine rhyme_cfl_init

      pure module function rhyme_cfl_time_step(c, samr) result(dt)
         real(kind=8), intent(in) :: c
         type(samr_t), intent(in) :: samr
         real(kind=8) :: dt
      end function rhyme_cfl_time_step
   end interface
end module rhyme_cfl
