submodule(rhyme_xyz) equality_smod
contains
   pure module function rhyme_xyz_equality(xxx1, xxx2) result(eq)
      implicit none

      type(xyz_t), intent(in) :: xxx1, xxx2
      logical :: eq

      character(len=128) :: todo_holder

      eq = .false.

      write (todo_holder, *) 'TODO: implement ', xxx1, ' == ', xxx2
   end function rhyme_xyz_equality
end submodule equality_smod
