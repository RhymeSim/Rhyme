submodule(rhyme_chombo) filename_generator_smod
contains
   module subroutine rhyme_chombo_filename_generator(prefix, nickname, iter, filename)
      implicit none

      character(len=*), intent(in) :: prefix, nickname
      integer, intent(in) :: iter
      character(len=1024), intent(out) :: filename

      character(len=6) :: itr_str

      filename = ""

      if (len_trim(prefix) > 0) then
         filename = trim(prefix)//'/'
      end if

      if (len_trim(nickname) > 0) then
         filename = trim(filename)//trim(nickname)//"-"
      end if

      if (iter < 0) then
         write (itr_str, "(I0.6)") 0
      else
         write (itr_str, "(I0.6)") iter
      end if

      filename = trim(filename)//trim(itr_str)//".chombo.h5"
   end subroutine rhyme_chombo_filename_generator
end submodule filename_generator_smod
