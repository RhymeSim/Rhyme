submodule(rhyme_riemann_problem) init_submodule
contains
   module subroutine rhyme_riemann_problem_init(rp, logger)
      implicit none

      type(riemann_problem_t), intent(inout) :: rp
      type(logger_t), intent(inout) :: logger

      real(kind=8) :: g

      call logger%begin_section('rp')

      g = get_gamma()

      call logger%log('', 'vacuum_density', '=', [rp%w_vacuum(cid%rho)])
      call logger%log('', 'vacuum_pressure', '=', [rp%w_vacuum(cid%p)])

      call logger%log('Setting up ideal gas coefficients...')

      gm1 = g - 1.d0
      gp1 = g + 1.d0
      gm1_gp1 = gm1/gp1
      gm1_2g = gm1/(2.d0*g)
      gp1_2g = gp1/(2.d0*g)
      g_inv = 1.d0/g

      call logger%end_section
   end subroutine rhyme_riemann_problem_init
end submodule init_submodule
