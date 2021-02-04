submodule(rhyme_chombo) write_samr_with_nickname_smod
contains
   module subroutine rhyme_chombo_write_samr_with_nickname(nickname, chombo, units, samr)
      implicit none

      character(len=*), intent(in) :: nickname
      type(chombo_t), intent(inout) :: chombo
      type(units_t), intent(in) :: units
      type(samr_t), intent(in) :: samr

      character(len=1024) :: old_nickname

      old_nickname = chombo%nickname
      chombo%nickname = nickname
      call rhyme_chombo_write_samr(chombo, units, samr)
      chombo%nickname = old_nickname
   end subroutine rhyme_chombo_write_samr_with_nickname
end submodule write_samr_with_nickname_smod
