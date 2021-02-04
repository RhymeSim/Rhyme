submodule(rhyme_chombo) filename_generator_smod
contains
   module subroutine rhyme_chombo_filename_generator(chombo, filename)
      implicit none

      type(chombo_t), intent(in) :: chombo
      character(len=1024), intent(out) :: filename

      character(len=6) :: itr_str

      filename = ""

      if (len_trim(chombo%prefix) > 0) then
         filename = trim(chombo%prefix)//'/'
      end if

      if (len_trim(chombo%nickname) > 0) then
         filename = trim(filename)//trim(chombo%nickname)//"-"
      end if

      if (chombo%iteration .eq. chid%unset) then
         write (itr_str, "(I0.6)") 0
      else
         write (itr_str, "(I0.6)") chombo%iteration
      end if

      filename = trim(filename)//trim(itr_str)//".chombo.h5"
   end subroutine rhyme_chombo_filename_generator
end submodule filename_generator_smod
