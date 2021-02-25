submodule(rhyme_chombo) create_chombo_smod
contains
   module subroutine rhyme_chombo_create_chombo(chombo)
      implicit none

      type(chombo_t), intent(inout) :: chombo

      character(len=1024) :: filename

      call rhyme_chombo_filename_generator( &
         chombo%prefix, chombo%nickname, chombo%iteration, filename)
      call rhyme_hdf5_util_create(chombo%file, filename)

      chombo%is_opened = .true.

   end subroutine rhyme_chombo_create_chombo
end submodule create_chombo_smod
