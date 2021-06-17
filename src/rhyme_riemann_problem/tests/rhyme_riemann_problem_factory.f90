module rhyme_riemann_problem_factory
   use rhyme_riemann_problem

contains

   function riemann_problem_factory_generate(factory_type) result(rp)
      implicit none

      character(len=*), intent(in) :: factory_type

      type(riemann_problem_t) :: rp

      if (factory_type == 'default') then
         rp = riemann_problem_t()
      else
         print *, 'Unknonw Riemann Problem factory type!', factory_type
      end if
   end function riemann_problem_factory_generate
end module rhyme_riemann_problem_factory
