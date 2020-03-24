submodule(rhyme_periodic_table) equality_smod
contains
pure module function rhyme_periodic_table_equality(pt1, pt2) result(eq)
   implicit none

   type(periodic_table_t), intent(in) :: pt1, pt2
   logical :: eq

   character(len=128) :: todo_holder

   eq = .false.

   write (todo_holder, *) 'TODO: implement ', pt1, ' == ', pt2
end function rhyme_periodic_table_equality
end submodule equality_smod
