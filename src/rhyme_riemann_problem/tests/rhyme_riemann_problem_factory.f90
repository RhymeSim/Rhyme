module rhyme_riemann_problem_factory
   use rhyme_riemann_problem

contains

   function riemann_problem_factory_generate(factory_type) result(rp)
      implicit none

      character(len=*), intent(in) :: factory_type

      type(riemann_problem_t) :: rp

      if (factory_type == 'default') then
         rp = riemann_problem_t()
         rp%irs%n_iteration = rp%n_iteration
         rp%irs%tolerance = rp%tolerance
      else
         print *, 'Unknonw Riemann problem factory type!', factory_type
      end if
   end function riemann_problem_factory_generate
end module rhyme_riemann_problem_factory
